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

-module(erlmd_tokeniser).

-include("types.hrl").
-include("consts.hrl").
-include("tokeniser.hrl").

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
    attempt_construct/3,
    check/3,

    %% Main loop
    feed/3,

    %% Finalization
    finalize/1
]).

%% Exported for testing
-export([prepare_byte/1, handle_attempt_result/2]).

%% Helper functions for constructs
-export([
    get_events/1,
    set_events/2,
    get_state/2,
    set_state/3,
    clear_state/2,
    get_markers/1,
    set_markers/2,

    %% Phase 7: Label tracking
    add_label_start/2,
    pop_label_start/1,
    has_label_starts/1,
    peek_label_start/1,
    add_loose_label_start/2,
    add_label/2,
    get_labels/1,
    clear_labels/1,
    clear_label_starts/1,
    clear_loose_label_starts/1,
    mark_link_starts_inactive/1,

    %% Phase 7: Definitions
    has_definition/2,
    get_definition/2,

    %% Phase 7: State fields
    event_count/1,
    set_end_index/2,
    get_end_index/1,
    set_label_size/2,
    get_label_size/1,
    inc_label_size/1,
    set_label_seen_char/2,
    get_label_seen_char/1,
    set_paren_depth/2,
    get_paren_depth/1,
    inc_paren_depth/1,
    dec_paren_depth/1,
    set_marker/2,
    get_marker/1,
    set_token_names/6,
    get_token_1/1,
    get_token_2/1,
    get_token_3/1,
    get_token_4/1,
    get_token_5/1,

    %% Phase 7: Resolvers
    register_resolver/2,
    register_resolver_before/2,
    run_resolvers/1
]).

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
%%   T = erlmd_tokeniser:new(<<"# Hello">>, #{})
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

    %% Reset consumed flag so the construct being attempted can consume
    T#tokenizer{
        attempts = [Attempt | T#tokenizer.attempts],
        consumed = false
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

    %% Reset consumed flag so the construct being checked can consume
    T#tokenizer{
        attempts = [Attempt | T#tokenizer.attempts],
        consumed = false
    }.

-spec attempt_construct(tokenizer(), atom(), state_result()) ->
    {state_result(), tokenizer()}.
%% @doc Attempt to parse a construct, returning ok/nok result.
%%
%% This is a helper function that combines:
%% 1. Setting up an attempt context
%% 2. Calling the construct through erlmd_state
%% 3. Handling the result
%%
%% Usage:
%%   case erlmd_tokeniser:attempt_construct(T, paragraph, nok) of
%%       {ok, T1} -> ... construct succeeded ...
%%       {nok, T1} -> ... construct failed, state reverted ...
%%   end
attempt_construct(T, ConstructName, NokFallback) ->
    %% Set up attempt context - on success continue, on failure use fallback
    T1 = attempt(T, ok, NokFallback),

    %% Call the construct through state dispatcher and run until completion
    InitialResult = erlmd_state:call(ConstructName, T1),
    FinalResult = run_construct_to_completion(T1, InitialResult),

    %% Handle the final result
    case FinalResult of
        {ok, T2} ->
            %% Construct succeeded - pop attempt and continue
            case T2#tokenizer.attempts of
                [_Attempt | RestAttempts] ->
                    T3 = T2#tokenizer{attempts = RestAttempts},
                    {ok, T3};
                [] ->
                    {ok, T2}
            end;
        {nok, T2} ->
            %% Construct failed - pop attempt, revert state, use fallback
            case T2#tokenizer.attempts of
                [Attempt | RestAttempts] ->
                    T3 = T2#tokenizer{attempts = RestAttempts},
                    %% Revert to saved progress
                    T4 = case Attempt#attempt.progress of
                        undefined -> T3;
                        Progress -> restore_progress(T3, Progress)
                    end,
                    {nok, T4};
                [] ->
                    {nok, T2}
            end;
        {error, Reason, T2} ->
            %% Error - propagate it
            {error, Reason, T2}
    end.

%% @private
%% Run a construct to completion, handling next/retry states
run_construct_to_completion(_T, {ok, T2}) ->
    {ok, T2};
run_construct_to_completion(_T, {nok, T2}) ->
    {nok, T2};
run_construct_to_completion(_T, {error, Reason, T2}) ->
    {error, Reason, T2};
run_construct_to_completion(T, {{next, NextState}, T2}) ->
    %% Prepare next byte (resets consumed flag and advances if needed)
    T3 = prepare_byte(T2),
    %% Call the next state and continue
    NextResult = erlmd_state:call(NextState, T3),
    run_construct_to_completion(T, NextResult);
run_construct_to_completion(T, {{retry, RetryState}, T2}) ->
    %% Reset consumed flag without advancing (for retry)
    T3 = T2#tokenizer{consumed = false},
    RetryResult = erlmd_state:call(RetryState, T3),
    run_construct_to_completion(T, RetryResult).

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
%%% API Functions - Helper Functions for Constructs
%%%=============================================================================

-spec get_events(tokenizer()) -> [event()].
%% @doc Get the current events list (in reversed order).
get_events(#tokenizer{events = Events}) ->
    Events.

-spec set_events(tokenizer(), [event()]) -> tokenizer().
%% @doc Set the events list (should be in reversed order).
set_events(T, Events) ->
    T#tokenizer{events = Events}.

-spec get_state(tokenizer(), atom()) -> term().
%% @doc Get a value from the parse_state map.
get_state(#tokenizer{parse_state = State}, Key) ->
    maps:get(Key, State, undefined).

-spec set_state(tokenizer(), atom(), term()) -> tokenizer().
%% @doc Set a value in the parse_state map.
set_state(#tokenizer{parse_state = State} = T, Key, Value) ->
    T#tokenizer{parse_state = maps:put(Key, Value, State)}.

-spec clear_state(tokenizer(), atom()) -> tokenizer().
%% @doc Remove a key from the parse_state map.
clear_state(#tokenizer{parse_state = State} = T, Key) ->
    T#tokenizer{parse_state = maps:remove(Key, State)}.

-spec get_markers(tokenizer()) -> [byte()].
%% @doc Get the list of marker bytes from parse_state.
%%
%% Markers are special characters that interrupt data parsing.
get_markers(#tokenizer{parse_state = State}) ->
    maps:get(markers, State, []).

-spec set_markers(tokenizer(), [byte()]) -> tokenizer().
%% @doc Set the list of marker bytes in parse_state.
set_markers(#tokenizer{parse_state = State} = T, Markers) ->
    T#tokenizer{parse_state = maps:put(markers, Markers, State)}.

%%%=============================================================================
%%% Helper Functions - Phase 7: Label Tracking
%%%=============================================================================

-spec add_label_start(tokenizer(), label_start()) -> tokenizer().
%% @doc Add a label start to the active list.
add_label_start(#tokenizer{label_starts = Starts} = T, LabelStart) ->
    T#tokenizer{label_starts = [LabelStart | Starts]}.

-spec pop_label_start(tokenizer()) -> {label_start(), tokenizer()}.
%% @doc Remove and return the most recent label start.
pop_label_start(#tokenizer{label_starts = [Start | Rest]} = T) ->
    {Start, T#tokenizer{label_starts = Rest}};
pop_label_start(#tokenizer{label_starts = []}) ->
    error(no_label_starts).

-spec has_label_starts(tokenizer()) -> boolean().
%% @doc Check if there are any active label starts.
has_label_starts(#tokenizer{label_starts = []}) -> false;
has_label_starts(#tokenizer{label_starts = [_|_]}) -> true.

-spec peek_label_start(tokenizer()) -> label_start().
%% @doc Get the most recent label start without removing it.
peek_label_start(#tokenizer{label_starts = [Start | _]}) ->
    Start;
peek_label_start(#tokenizer{label_starts = []}) ->
    error(no_label_starts).

-spec add_loose_label_start(tokenizer(), label_start()) -> tokenizer().
%% @doc Add a failed label start to the loose list.
add_loose_label_start(#tokenizer{label_starts_loose = Loose} = T, LabelStart) ->
    T#tokenizer{label_starts_loose = [LabelStart | Loose]}.

-spec add_label(tokenizer(), label()) -> tokenizer().
%% @doc Add a matched label to the list.
add_label(#tokenizer{labels = Labels} = T, Label) ->
    T#tokenizer{labels = [Label | Labels]}.

-spec get_labels(tokenizer()) -> [label()].
%% @doc Get all matched labels.
get_labels(#tokenizer{labels = Labels}) ->
    Labels.

-spec clear_labels(tokenizer()) -> tokenizer().
%% @doc Clear all matched labels.
clear_labels(T) ->
    T#tokenizer{labels = []}.

-spec clear_label_starts(tokenizer()) -> tokenizer().
%% @doc Clear all active label starts.
clear_label_starts(T) ->
    T#tokenizer{label_starts = []}.

-spec clear_loose_label_starts(tokenizer()) -> tokenizer().
%% @doc Clear all loose label starts.
clear_loose_label_starts(T) ->
    T#tokenizer{label_starts_loose = []}.

-spec mark_link_starts_inactive(tokenizer()) -> tokenizer().
%% @doc Mark all earlier link starts as inactive (prevents nested links).
mark_link_starts_inactive(#tokenizer{label_starts = Starts} = T) ->
    InactiveStarts = [S#label_start{inactive = true} || S <- Starts],
    T#tokenizer{label_starts = InactiveStarts}.

%%%=============================================================================
%%% Helper Functions - Phase 7: Definitions
%%%=============================================================================

-spec has_definition(tokenizer(), binary()) -> boolean().
%% @doc Check if a definition exists in parse_state.
has_definition(#tokenizer{parse_state = State}, Id) ->
    Defs = maps:get(definitions, State, #{}),
    maps:is_key(Id, Defs).

-spec get_definition(tokenizer(), binary()) -> definition() | undefined.
%% @doc Get a definition from parse_state.
get_definition(#tokenizer{parse_state = State}, Id) ->
    Defs = maps:get(definitions, State, #{}),
    maps:get(Id, Defs, undefined).

%%%=============================================================================
%%% Helper Functions - Phase 7: State Fields
%%%=============================================================================

-spec event_count(tokenizer()) -> non_neg_integer().
%% @doc Get the current event count.
event_count(#tokenizer{events = Events}) ->
    length(Events).

-spec set_end_index(tokenizer(), non_neg_integer()) -> tokenizer().
%% @doc Set the temporary end index.
set_end_index(T, Index) ->
    T#tokenizer{end_index = Index}.

-spec get_end_index(tokenizer()) -> non_neg_integer().
%% @doc Get the temporary end index.
get_end_index(#tokenizer{end_index = Index}) ->
    Index.

-spec set_label_size(tokenizer(), non_neg_integer()) -> tokenizer().
%% @doc Set the current label size.
set_label_size(T, Size) ->
    T#tokenizer{label_size = Size}.

-spec get_label_size(tokenizer()) -> non_neg_integer().
%% @doc Get the current label size.
get_label_size(#tokenizer{label_size = Size}) ->
    Size.

-spec inc_label_size(tokenizer()) -> tokenizer().
%% @doc Increment the label size.
inc_label_size(#tokenizer{label_size = Size} = T) ->
    T#tokenizer{label_size = Size + 1}.

-spec set_label_seen_char(tokenizer(), boolean()) -> tokenizer().
%% @doc Set whether we've seen a non-whitespace character in the label.
set_label_seen_char(T, Seen) ->
    T#tokenizer{label_seen_char = Seen}.

-spec get_label_seen_char(tokenizer()) -> boolean().
%% @doc Get whether we've seen a non-whitespace character.
get_label_seen_char(#tokenizer{label_seen_char = Seen}) ->
    Seen.

-spec set_paren_depth(tokenizer(), non_neg_integer()) -> tokenizer().
%% @doc Set the parenthesis depth.
set_paren_depth(T, Depth) ->
    T#tokenizer{paren_depth = Depth}.

-spec get_paren_depth(tokenizer()) -> non_neg_integer().
%% @doc Get the parenthesis depth.
get_paren_depth(#tokenizer{paren_depth = Depth}) ->
    Depth.

-spec inc_paren_depth(tokenizer()) -> tokenizer().
%% @doc Increment the parenthesis depth.
inc_paren_depth(#tokenizer{paren_depth = Depth} = T) ->
    T#tokenizer{paren_depth = Depth + 1}.

-spec dec_paren_depth(tokenizer()) -> tokenizer().
%% @doc Decrement the parenthesis depth.
dec_paren_depth(#tokenizer{paren_depth = Depth} = T) ->
    T#tokenizer{paren_depth = Depth - 1}.

-spec set_marker(tokenizer(), byte() | non_neg_integer()) -> tokenizer().
%% @doc Set the current marker character.
set_marker(T, Marker) ->
    T#tokenizer{marker = Marker}.

-spec get_marker(tokenizer()) -> byte() | non_neg_integer().
%% @doc Get the current marker character.
get_marker(#tokenizer{marker = Marker}) ->
    Marker.

-spec set_token_names(tokenizer(), atom() | undefined, atom() | undefined,
                      atom() | undefined, atom() | undefined,
                      atom() | undefined) -> tokenizer().
%% @doc Set the token names for partial constructs.
set_token_names(T, T1, T2, T3, T4, T5) ->
    T#tokenizer{
        token_1 = T1,
        token_2 = T2,
        token_3 = T3,
        token_4 = T4,
        token_5 = T5
    }.

-spec get_token_1(tokenizer()) -> atom() | undefined.
get_token_1(#tokenizer{token_1 = T1}) -> T1.

-spec get_token_2(tokenizer()) -> atom() | undefined.
get_token_2(#tokenizer{token_2 = T2}) -> T2.

-spec get_token_3(tokenizer()) -> atom() | undefined.
get_token_3(#tokenizer{token_3 = T3}) -> T3.

-spec get_token_4(tokenizer()) -> atom() | undefined.
get_token_4(#tokenizer{token_4 = T4}) -> T4.

-spec get_token_5(tokenizer()) -> atom() | undefined.
get_token_5(#tokenizer{token_5 = T5}) -> T5.

%%%=============================================================================
%%% Helper Functions - Phase 7: Resolvers
%%%=============================================================================

-spec register_resolver(tokenizer(), atom()) -> tokenizer().
%% @doc Register a resolver to run at the end of parsing.
register_resolver(#tokenizer{resolvers = Resolvers} = T, Resolver) ->
    T#tokenizer{resolvers = [Resolver | Resolvers]}.

-spec register_resolver_before(tokenizer(), atom()) -> tokenizer().
%% @doc Register a resolver to run before other resolvers.
register_resolver_before(#tokenizer{resolver_before = Resolvers} = T, Resolver) ->
    T#tokenizer{resolver_before = [Resolver | Resolvers]}.

-spec run_resolvers(tokenizer()) -> tokenizer().
%% @doc Run all registered resolvers.
%%
%% Runs resolvers in order:
%% 1. resolver_before list (in reverse registration order)
%% 2. resolvers list (in reverse registration order)
%%
%% Each resolver is a module name that must export resolve/1.
run_resolvers(T) ->
    %% Run before resolvers first
    BeforeResolvers = lists:reverse(T#tokenizer.resolver_before),
    T1 = lists:foldl(fun run_resolver/2, T, BeforeResolvers),

    %% Then run regular resolvers
    Resolvers = lists:reverse(T#tokenizer.resolvers),
    T2 = lists:foldl(fun run_resolver/2, T1, Resolvers),

    T2.

%% @private
%% Run a single resolver
run_resolver(ResolverName, T) ->
    %% Convert resolver name to module name
    %% e.g., label -> erlmd_resolver_label
    ModuleName = list_to_atom("erlmd_resolver_" ++ atom_to_list(ResolverName)),

    %% Call the resolver's resolve/1 function
    ModuleName:resolve(T).

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
        consumed = T#tokenizer.consumed,
        index = T#tokenizer.index,
        line = T#tokenizer.line,
        column = T#tokenizer.column,
        vs = T#tokenizer.vs,

        %% Phase 7 fields
        label_starts_len = length(T#tokenizer.label_starts),
        label_starts_loose_len = length(T#tokenizer.label_starts_loose),
        labels_len = length(T#tokenizer.labels),
        end_index = T#tokenizer.end_index,
        label_size = T#tokenizer.label_size,
        label_seen_char = T#tokenizer.label_seen_char,
        paren_depth = T#tokenizer.paren_depth,
        marker = T#tokenizer.marker,
        token_1 = T#tokenizer.token_1,
        token_2 = T#tokenizer.token_2,
        token_3 = T#tokenizer.token_3,
        token_4 = T#tokenizer.token_4,
        token_5 = T#tokenizer.token_5,
        resolvers_len = length(T#tokenizer.resolvers),
        resolver_before_len = length(T#tokenizer.resolver_before)
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

    %% Phase 7: Truncate label tracking lists
    LabelStartsToRemove = length(T#tokenizer.label_starts) - P#progress.label_starts_len,
    LabelStarts = case LabelStartsToRemove of
        0 -> T#tokenizer.label_starts;
        L when L > 0 -> lists:nthtail(L, T#tokenizer.label_starts)
    end,

    LabelStartsLooseToRemove = length(T#tokenizer.label_starts_loose) - P#progress.label_starts_loose_len,
    LabelStartsLoose = case LabelStartsLooseToRemove of
        0 -> T#tokenizer.label_starts_loose;
        LL when LL > 0 -> lists:nthtail(LL, T#tokenizer.label_starts_loose)
    end,

    LabelsToRemove = length(T#tokenizer.labels) - P#progress.labels_len,
    Labels = case LabelsToRemove of
        0 -> T#tokenizer.labels;
        La when La > 0 -> lists:nthtail(La, T#tokenizer.labels)
    end,

    ResolversToRemove = length(T#tokenizer.resolvers) - P#progress.resolvers_len,
    Resolvers = case ResolversToRemove of
        0 -> T#tokenizer.resolvers;
        R when R > 0 -> lists:nthtail(R, T#tokenizer.resolvers)
    end,

    ResolverBeforeToRemove = length(T#tokenizer.resolver_before) - P#progress.resolver_before_len,
    ResolverBefore = case ResolverBeforeToRemove of
        0 -> T#tokenizer.resolver_before;
        RB when RB > 0 -> lists:nthtail(RB, T#tokenizer.resolver_before)
    end,

    T#tokenizer{
        events = Events,
        stack = Stack,
        previous = P#progress.previous,
        current = P#progress.current,
        consumed = P#progress.consumed,
        index = P#progress.index,
        line = P#progress.line,
        column = P#progress.column,
        vs = P#progress.vs,

        %% Phase 7 fields
        label_starts = LabelStarts,
        label_starts_loose = LabelStartsLoose,
        labels = Labels,
        end_index = P#progress.end_index,
        label_size = P#progress.label_size,
        label_seen_char = P#progress.label_seen_char,
        paren_depth = P#progress.paren_depth,
        marker = P#progress.marker,
        token_1 = P#progress.token_1,
        token_2 = P#progress.token_2,
        token_3 = P#progress.token_3,
        token_4 = P#progress.token_4,
        token_5 = P#progress.token_5,
        resolvers = Resolvers,
        resolver_before = ResolverBefore
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
            feed_loop(T2#tokenizer{consumed = false}, {next, RetryState});
        {error, _Reason, _T2} = Error ->
            %% Error occurred - propagate it
            Error
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
