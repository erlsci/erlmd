%%%-----------------------------------------------------------------------------
%%% @doc Tokenizer - The core state machine driver for markdown parsing.
%%%
%%% This module implements the tokenizer that orchestrates all markdown parsing.
%%% It manages:
%%% - Current parsing state and position
%%% - Byte-by-byte consumption
%%% - Enter/exit event generation
%%% - Backtracking via the "attempt" mechanism
%%% - State dispatching to construct-specific functions
%%%
%%% Based on markdown-rs src/tokenizer.rs
%%%
%%% PERFORMANCE CRITICAL:
%%% - All loops are tail-recursive
%%% - No sub-binary creation in hot paths
%%% - Binary references preserved via binary:at/2
%%% - Match context optimization in function heads
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_tokenizer).

-include("types.hrl").
-include("consts.hrl").
-include("tokenizer_internal.hrl").

%%%=============================================================================
%%% Types
%%%=============================================================================

-type tokenizer() :: #tokenizer{}.

-export_type([tokenizer/0, state_result/0]).

%%%=============================================================================
%%% API Exports
%%%=============================================================================

-export([
    %% Creation
    new/2,

    %% Position and consumption
    current/1,
    consume/1,

    %% Events
    enter/2,
    enter_with_link/3,
    exit/2,

    %% Attempts
    attempt/3,
    check/3,

    %% Main loop
    feed/3,

    %% Finalization
    finalize/1
]).

%% Exported for testing
-export([prepare_byte/1, handle_attempt_result/2]).

%%%=============================================================================
%%% API Functions - Creation
%%%=============================================================================

-spec new(binary(), map()) -> tokenizer().
%% @doc Create a new tokenizer from input bytes and options.
%%
%% The tokenizer starts at position (1, 1, 0) - line 1, column 1, offset 0.
%% Initial state has no events, empty stack, and no current byte.
%%
%% Example:
%%   T = erlmd_tokenizer:new(<<"# Hello">>, #{})
new(Bytes, Options) when is_binary(Bytes), is_map(Options) ->
    #tokenizer{
        bytes = Bytes,
        index = 0,
        line = 1,
        column = 1,
        vs = 0,
        previous = undefined,
        current = undefined,
        consumed = true,
        events = [],
        stack = [],
        attempts = [],
        parse_state = #{},
        options = Options
    }.

%%%=============================================================================
%%% API Functions - Position and Consumption
%%%=============================================================================

-spec current(tokenizer()) -> byte() | eof.
%% @doc Get the current byte without consuming it.
%%
%% Returns 'eof' if at end of input.
%% CRITICAL: This function must be FAST as it's called constantly.
current(#tokenizer{current = Byte}) when Byte =/= undefined ->
    Byte;
current(#tokenizer{bytes = Bytes, index = Index}) when Index >= byte_size(Bytes) ->
    eof;
current(#tokenizer{bytes = Bytes, index = Index, vs = 0}) ->
    %% Fast path: no virtual steps, just get the byte
    binary:at(Bytes, Index);
current(#tokenizer{vs = Vs}) when Vs > 0 ->
    %% Virtual space from tab expansion
    $\s.

-spec consume(tokenizer()) -> tokenizer().
%% @doc Consume the current byte and advance position.
%%
%% This is THE most critical function in the entire parser.
%% It must be:
%% 1. Tail recursive
%% 2. Never create sub-binaries
%% 3. Handle line endings correctly
%% 4. Handle tab expansion
%%
%% Pattern from Rust tokenizer::move_one() and byte_action()
consume(#tokenizer{consumed = true}) ->
    error(already_consumed);
consume(#tokenizer{bytes = Bytes, index = Index} = T) when Index >= byte_size(Bytes) ->
    %% At EOF
    T#tokenizer{
        consumed = true,
        previous = T#tokenizer.current,
        current = undefined
    };
consume(T) ->
    case byte_action(T) of
        {normal, Byte} ->
            consume_normal(T, Byte);
        {insert, VsCount} when is_integer(VsCount) ->
            %% Starting tab expansion or consuming virtual space
            case T#tokenizer.vs of
                0 ->
                    %% Just hit the tab - start expanding, consume one virtual space
                    T#tokenizer{
                        consumed = true,
                        previous = $\s,
                        current = undefined,
                        column = T#tokenizer.column + 1,
                        vs = VsCount - 1
                    };
                Vs when Vs > 0 ->
                    %% Still in virtual spaces from earlier tab
                    T#tokenizer{
                        consumed = true,
                        previous = $\s,
                        current = undefined,
                        column = T#tokenizer.column + 1,
                        vs = Vs - 1,
                        index = if Vs =:= 1 -> T#tokenizer.index + 1; true -> T#tokenizer.index end
                    }
            end;
        ignore ->
            consume_ignore(T)
    end.

%%%=============================================================================
%%% API Functions - Events
%%%=============================================================================

-spec enter(tokenizer(), atom()) -> tokenizer().
%% @doc Emit an 'enter' event for a token type.
%%
%% Creates an event at the current position and pushes the token onto the stack.
%% The token remains open until exit/2 is called with the same name.
enter(T, Name) when is_atom(Name) ->
    Event = #event{
        kind = enter,
        name = Name,
        point = make_point(T),
        link = undefined,
        content = undefined
    },
    T#tokenizer{
        events = [Event | T#tokenizer.events],
        stack = [Name | T#tokenizer.stack]
    }.

-spec enter_with_link(tokenizer(), atom(), link()) -> tokenizer().
%% @doc Emit an 'enter' event with a link to nested content.
%%
%% Used for tokens that contain other content types (e.g., paragraph contains text).
enter_with_link(T, Name, Link) when is_atom(Name), is_record(Link, link) ->
    Event = #event{
        kind = enter,
        name = Name,
        point = make_point(T),
        link = Link,
        content = undefined
    },
    T#tokenizer{
        events = [Event | T#tokenizer.events],
        stack = [Name | T#tokenizer.stack]
    }.

-spec exit(tokenizer(), atom()) -> tokenizer().
%% @doc Emit an 'exit' event for a token type.
%%
%% Must match the most recent 'enter' event.
%% Pops the token from the stack and emits the exit event.
%%
%% CRITICAL: Validates that we're exiting the correct token.
exit(T, Name) when is_atom(Name) ->
    case T#tokenizer.stack of
        [Name | RestStack] ->
            Event = #event{
                kind = exit,
                name = Name,
                point = make_point(T),
                link = undefined,
                content = undefined
            },
            T#tokenizer{
                events = [Event | T#tokenizer.events],
                stack = RestStack
            };
        [Other | _] ->
            error({exit_mismatch, {expected, Name, got, Other}});
        [] ->
            error({exit_without_enter, Name})
    end.

%%%=============================================================================
%%% API Functions - Attempts (Backtracking)
%%%=============================================================================

-spec attempt(tokenizer(), state_result(), state_result()) -> tokenizer().
%% @doc Stack an attempt: succeed goes to Ok, failure reverts and goes to Nok.
%%
%% Used when trying to parse something that might fail. If it fails (returns 'nok'),
%% the tokenizer state is reverted to when attempt/3 was called.
attempt(T, Ok, Nok) ->
    %% Always capture current state for potential revert
    Progress = capture_progress(T),

    Attempt = #attempt{
        kind = attempt,
        ok = Ok,
        nok = Nok,
        progress = Progress
    },

    T#tokenizer{
        attempts = [Attempt | T#tokenizer.attempts]
    }.

-spec check(tokenizer(), state_result(), state_result()) -> tokenizer().
%% @doc Stack a check: always reverts, goes to Ok or Nok based on result.
%%
%% Like attempt/3 but ALWAYS reverts state, even on success.
%% Used for lookahead checks.
check(T, Ok, Nok) ->
    Progress = capture_progress(T),

    Attempt = #attempt{
        kind = check,
        ok = Ok,
        nok = Nok,
        progress = Progress
    },

    T#tokenizer{
        attempts = [Attempt | T#tokenizer.attempts]
    }.

-spec handle_attempt_result(tokenizer(), state_result()) ->
    {state_result(), tokenizer()}.
%% @doc Process an 'ok' or 'nok' result when an attempt is on the stack.
%%
%% This is called internally when a state function returns 'ok' or 'nok'.
%% It pops the attempt and either continues or reverts.
handle_attempt_result(T, Result) when Result =:= ok; Result =:= nok ->
    case T#tokenizer.attempts of
        [Attempt | RestAttempts] ->
            T1 = T#tokenizer{attempts = RestAttempts},

            %% Determine if we revert
            ShouldRevert = Attempt#attempt.kind =:= check orelse Result =:= nok,

            T2 = case {ShouldRevert, Attempt#attempt.progress} of
                {true, Progress} when Progress =/= undefined ->
                    restore_progress(T1, Progress);
                _ ->
                    T1
            end,

            %% Continue with ok or nok state
            NextState = case Result of
                ok -> Attempt#attempt.ok;
                nok -> Attempt#attempt.nok
            end,

            {NextState, T2#tokenizer{consumed = true}};
        [] ->
            %% No attempts - return result as-is
            {Result, T#tokenizer{consumed = true}}
    end.

%%%=============================================================================
%%% API Functions - Main Loop
%%%=============================================================================

-spec feed(tokenizer(), atom(), binary()) -> {state_result(), tokenizer()}.
%% @doc Feed bytes to the tokenizer starting from a state.
%%
%% This is the main loop that:
%% 1. Prepares the next byte
%% 2. Calls the state function
%% 3. Handles the result
%% 4. Repeats until done
%%
%% Returns when reaching 'ok', 'nok', or end of input.
feed(T, StateName, Bytes) ->
    T1 = T#tokenizer{bytes = Bytes},
    feed_loop(T1, {next, StateName}).

%%%=============================================================================
%%% API Functions - Finalization
%%%=============================================================================

-spec finalize(tokenizer()) -> {ok, [event()]} | {error, term()}.
%% @doc Finalize tokenization and return events.
%%
%% Validates that:
%% - Stack is empty (all tokens closed)
%% - No pending attempts
%%
%% Returns events in correct order (they're accumulated in reverse).
finalize(T) ->
    case {T#tokenizer.stack, T#tokenizer.attempts} of
        {[], []} ->
            {ok, lists:reverse(T#tokenizer.events)};
        {Stack, []} when Stack =/= [] ->
            {error, {unclosed_tokens, Stack}};
        {_, Attempts} when Attempts =/= [] ->
            {error, {pending_attempts, length(Attempts)}}
    end.

%%%=============================================================================
%%% Internal Functions - Consumption Helpers
%%%=============================================================================

%% @private
%% Consume a normal byte (most common path)
consume_normal(T, $\n) ->
    %% Line ending - update line number
    T#tokenizer{
        consumed = true,
        previous = $\n,
        current = undefined,
        index = T#tokenizer.index + 1,
        line = T#tokenizer.line + 1,
        column = 1,
        vs = 0
    };
consume_normal(T, Byte) ->
    %% Regular character
    T#tokenizer{
        consumed = true,
        previous = Byte,
        current = undefined,
        index = T#tokenizer.index + 1,
        column = T#tokenizer.column + 1,
        vs = 0
    }.


%% @private
%% Ignore byte (e.g., CR in CRLF)
consume_ignore(T) ->
    T#tokenizer{
        consumed = true,
        index = T#tokenizer.index + 1
    }.

%% @private
%% Determine how to handle the byte at current position.
%% Based on Rust tokenizer::byte_action()
-spec byte_action(tokenizer()) ->
    {normal, byte()} | {insert, byte()} | ignore.
byte_action(#tokenizer{bytes = Bytes, index = Index, vs = Vs, column = Column}) ->
    case {Index < byte_size(Bytes), Vs} of
        {false, _} ->
            %% Past end of input
            {normal, undefined};
        {true, 0} ->
            %% On a real byte
            Byte = binary:at(Bytes, Index),
            case Byte of
                $\r ->
                    %% Check for CRLF
                    case Index + 1 < byte_size(Bytes) andalso
                         binary:at(Bytes, Index + 1) =:= $\n of
                        true -> ignore;  % CR in CRLF - skip it
                        false -> {normal, $\n}  % Bare CR - treat as LF
                    end;
                $\t ->
                    %% Tab - calculate virtual spaces needed
                    Remainder = (Column - 1) rem ?TAB_SIZE,
                    VsNeeded = ?TAB_SIZE - Remainder,
                    {insert, VsNeeded};
                _ ->
                    {normal, Byte}
            end;
        {true, Vs} when Vs > 0 ->
            %% In virtual spaces from tab
            {insert, $\s}
    end.

%%%=============================================================================
%%% Internal Functions - Event Helpers
%%%=============================================================================

%% @private
%% Create a point record from current tokenizer position.
-spec make_point(tokenizer()) -> point().
make_point(#tokenizer{line = Line, column = Column, index = Index}) ->
    #point{
        line = Line,
        column = Column,
        offset = Index
    }.

%%%=============================================================================
%%% Internal Functions - Attempt Helpers
%%%=============================================================================

%% @private
%% Capture tokenizer progress for backtracking
-spec capture_progress(tokenizer()) -> progress().
capture_progress(T) ->
    #progress{
        events_len = length(T#tokenizer.events),
        stack_len = length(T#tokenizer.stack),
        previous = T#tokenizer.previous,
        current = T#tokenizer.current,
        index = T#tokenizer.index,
        line = T#tokenizer.line,
        column = T#tokenizer.column,
        vs = T#tokenizer.vs
    }.

%% @private
%% Restore tokenizer progress (revert state)
-spec restore_progress(tokenizer(), progress()) -> tokenizer().
restore_progress(T, P) ->
    %% Truncate events and stack to saved lengths
    EventsToRemove = length(T#tokenizer.events) - P#progress.events_len,
    StackToRemove = length(T#tokenizer.stack) - P#progress.stack_len,

    Events = case EventsToRemove of
        0 -> T#tokenizer.events;
        N when N > 0 -> lists:nthtail(N, T#tokenizer.events)
    end,

    Stack = case StackToRemove of
        0 -> T#tokenizer.stack;
        M when M > 0 -> lists:nthtail(M, T#tokenizer.stack)
    end,

    T#tokenizer{
        events = Events,
        stack = Stack,
        previous = P#progress.previous,
        current = P#progress.current,
        index = P#progress.index,
        line = P#progress.line,
        column = P#progress.column,
        vs = P#progress.vs
    }.

%%%=============================================================================
%%% Internal Functions - Main Loop
%%%=============================================================================

%% @private
%% Main feed loop - tail recursive
feed_loop(T, {next, StateName}) ->
    %% Prepare next byte
    T1 = prepare_byte(T),

    %% Call state function via dispatcher
    Result = erlmd_state:call(StateName, T1),

    %% Handle result
    case Result of
        {ok, T2} -> handle_attempt_result(T2, ok);
        {nok, T2} -> handle_attempt_result(T2, nok);
        {{next, NextState}, T2} -> feed_loop(T2, {next, NextState});
        {{retry, RetryState}, T2} ->
            %% Retry without consuming
            feed_loop(T2#tokenizer{consumed = false}, {next, RetryState})
    end;
feed_loop(T, FinalResult) ->
    %% ok or nok - done
    {FinalResult, T}.

%% @private
%% Prepare the next byte for consumption
-spec prepare_byte(tokenizer()) -> tokenizer().
prepare_byte(#tokenizer{consumed = true, bytes = Bytes, index = Index, vs = Vs} = T) ->
    %% Determine next byte
    NextByte = case Index < byte_size(Bytes) of
        true when Vs =:= 0 ->
            binary:at(Bytes, Index);
        true when Vs > 0 ->
            $\s;  % Virtual space
        false ->
            eof
    end,

    T#tokenizer{
        current = NextByte,
        consumed = false
    };
prepare_byte(#tokenizer{consumed = false}) ->
    error(byte_not_consumed).
