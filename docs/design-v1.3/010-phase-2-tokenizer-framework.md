# Phase 2: Tokenizer Framework - Detailed Implementation Plan

**Project**: erlmd (Erlang Markdown Parser)  
**Phase**: 2 - Tokenizer Framework  
**Duration**: 1 week  
**Date**: November 13, 2025  

---

## Table of Contents

1. [Context & Overview](#context--overview)
2. [Reference Materials](#reference-materials)
3. [Prerequisites](#prerequisites)
4. [Module 2.1: erlmd_tokenizer.erl](#module-21-erlmd_tokenizererl)
5. [Module 2.2: erlmd_state.erl](#module-22-erlmd_stateerl)
6. [Module 2.3: Helper Utilities](#module-23-helper-utilities)
7. [Testing Requirements](#testing-requirements)
8. [Integration & Validation](#integration--validation)
9. [Acceptance Criteria](#acceptance-criteria)
10. [Implementation Notes](#implementation-notes)

---

## Context & Overview

### Phase Objectives

Phase 2 establishes the **tokenizer framework** - the core state machine driver that orchestrates all markdown parsing. This is the heart of the parser that:

- Manages the current parsing state and position
- Consumes bytes one at a time
- Generates enter/exit events for semantic elements
- Handles backtracking via the "attempt" mechanism
- Dispatches to construct-specific state functions

**This phase does NOT implement any actual markdown constructs** - those come in later phases. We're building the *engine*, not the *features*.

### Architecture Pattern

```
Input Bytes → Tokenizer (State Machine) → State Dispatcher → Event Stream
```

The tokenizer maintains:
- **Current position** (line, column, offset, virtual steps)
- **Event list** (accumulated enter/exit pairs)
- **State stack** (for nested constructs)
- **Attempt stack** (for backtracking)

### Key Insight from Rust Implementation

The Rust `tokenizer.rs` uses a **direct function call model** with explicit state management. In Erlang, we'll mirror this with:
- Records for state (not processes!)
- Pattern matching in function heads
- Tail-recursive state transitions
- No `gen_statem` (too slow for byte-by-byte parsing)

---

## Reference Materials

### Primary References

1. **Rust Implementation**: `markdown-rs/src/tokenizer.rs` (attached)
2. **State Dispatcher**: `markdown-rs/src/state.rs` (attached)
3. **Event Types**: `markdown-rs/src/event.rs` (attached)
4. **Parser Integration**: `markdown-rs/src/parser.rs` (attached)

### Architecture Documents

- **Section 3.2** in `007-erlmd-library-rewrite.md`: Tokenizer responsibilities
- **Section 2.2** in `002-rewrite-research-erlang-markdown-implementation-patterns.md`: State machine patterns

### Erlang Patterns to Follow

From `002-rewrite-research-erlang-markdown-implementation-patterns.md`:

1. **No gen_statem** - Use direct function calls
2. **Binary pattern matching in function heads** - Preserve match context
3. **Records for state** - Fast field access
4. **Tail recursion** - Mandatory for large documents

---

## Prerequisites

Before starting Phase 2, ensure:

- [ ] Phase 0 complete: Project structure exists
- [ ] Phase 1 complete: Core types defined in `include/types.hrl`
- [ ] `include/consts.hrl` available with TAB_SIZE
- [ ] Test infrastructure ready

**Required header files**:
- `include/types.hrl` - event(), point(), link(), content_type()
- `include/consts.hrl` - TAB_SIZE, other constants

---

## Module 2.1: erlmd_tokenizer.erl

### Overview

The tokenizer is the **state machine driver**. It manages position, consumes bytes, generates events, and handles attempts.

### Core Record Definition

```erlang
-module(erlmd_tokenizer).

-include("types.hrl").
-include("consts.hrl").

-record(tokenizer, {
    %% Input and position
    bytes :: binary(),                    % Complete input
    index = 0 :: non_neg_integer(),       % Current byte position (0-indexed)
    line = 1 :: pos_integer(),            % Current line (1-indexed)
    column = 1 :: pos_integer(),          % Current column (1-indexed)
    vs = 0 :: non_neg_integer(),          % Virtual steps (for tabs)
    
    %% Byte tracking
    previous = undefined :: byte() | undefined,  % Previous byte
    current = undefined :: byte() | undefined,   % Current byte
    consumed = true :: boolean(),         % Whether current byte is consumed
    
    %% Output
    events = [] :: [event()],             % Accumulated events (reversed)
    stack = [] :: [atom()],               % Token type stack
    
    %% Backtracking
    attempts = [] :: [attempt()],         % Attempt stack
    
    %% Context
    parse_state :: map(),                 % Global parse state
    options :: map()                      % Parse options
}).

-record(attempt, {
    ok :: state_result(),                 % Success continuation
    nok :: state_result(),                % Failure continuation
    kind :: attempt | check,              % Discard on failure or always
    progress = undefined :: progress() | undefined  % Saved state
}).

-record(progress, {
    events_len :: non_neg_integer(),
    stack_len :: non_neg_integer(),
    previous :: byte() | undefined,
    current :: byte() | undefined,
    index :: non_neg_integer(),
    line :: pos_integer(),
    column :: pos_integer(),
    vs :: non_neg_integer()
}).

-type tokenizer() :: #tokenizer{}.
-type attempt() :: #attempt{}.
-type progress() :: #progress{}.
-type state_result() :: ok | nok | {next, atom()} | {retry, atom()}.

-export_type([tokenizer/0, state_result/0]).
```

### Key Functions - Part 1: Creation and Position

#### 1.1 new/2 - Create Tokenizer

```erlang
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
        parse_state = #{},  % Will be populated by parser
        options = Options
    }.
```

#### 1.2 current/1 - Get Current Byte

```erlang
-spec current(tokenizer()) -> byte() | eof.
%% @doc Get the current byte without consuming it.
%%
%% Returns 'eof' if at end of input.
%% CRITICAL: This function must be FAST as it's called constantly.
%%
%% Example:
%%   case erlmd_tokenizer:current(T) of
%%       $# -> handle_hash(T);
%%       eof -> {ok, T};
%%       _ -> continue(T)
%%   end
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
```

#### 1.3 consume/1 - Consume Current Byte

```erlang
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
        {insert, Byte} ->
            consume_insert(T, Byte);
        ignore ->
            consume_ignore(T)
    end.

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
%% Consume a virtual space (from tab expansion)
consume_insert(T, $\s) ->
    T#tokenizer{
        consumed = true,
        previous = $\s,
        current = undefined,
        column = T#tokenizer.column + 1,
        vs = T#tokenizer.vs + 1
    }.

%% @private
%% Ignore byte (e.g., CR in CRLF)
consume_ignore(T) ->
    T#tokenizer{
        consumed = true,
        index = T#tokenizer.index + 1
    }.
```

#### 1.4 byte_action/1 - Determine How to Handle Byte

```erlang
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
                    Remainder = Column rem ?TAB_SIZE,
                    VsNeeded = case Remainder of
                        0 -> 0;
                        _ -> ?TAB_SIZE - Remainder
                    end,
                    case VsNeeded of
                        0 -> {normal, $\t};
                        _ -> {insert, $\t}  % Will emit spaces
                    end;
                _ ->
                    {normal, Byte}
            end;
        {true, _} ->
            %% In virtual spaces from tab
            {insert, $\s}
    end.
```

### Key Functions - Part 2: Events

#### 2.1 enter/2 - Enter a Token

```erlang
-spec enter(tokenizer(), atom()) -> tokenizer().
%% @doc Emit an 'enter' event for a token type.
%%
%% Creates an event at the current position and pushes the token onto the stack.
%% The token remains open until exit/2 is called with the same name.
%%
%% Example:
%%   T1 = erlmd_tokenizer:enter(T, paragraph)
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
```

#### 2.2 enter_with_link/3 - Enter with Content Link

```erlang
-spec enter_with_link(tokenizer(), atom(), link()) -> tokenizer().
%% @doc Emit an 'enter' event with a link to nested content.
%%
%% Used for tokens that contain other content types (e.g., paragraph contains text).
%% The link will be processed during subtokenization.
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
```

#### 2.3 exit/2 - Exit a Token

```erlang
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
```

#### 2.4 make_point/1 - Create Point

```erlang
%% @private
%% Create a point record from current tokenizer position.
-spec make_point(tokenizer()) -> point().
make_point(#tokenizer{line = Line, column = Column, index = Index}) ->
    #point{
        line = Line,
        column = Column,
        offset = Index
    }.
```

### Key Functions - Part 3: Attempts (Backtracking)

#### 3.1 attempt/3 - Try with Revert on Failure

```erlang
-spec attempt(tokenizer(), state_result(), state_result()) -> tokenizer().
%% @doc Stack an attempt: succeed goes to Ok, failure reverts and goes to Nok.
%%
%% Used when trying to parse something that might fail. If it fails (returns 'nok'),
%% the tokenizer state is reverted to when attempt/3 was called.
%%
%% Example:
%%   T1 = erlmd_tokenizer:attempt(T, {next, heading_after}, {next, paragraph_start}),
%%   %% Now try to parse heading...
attempt(T, Ok, Nok) ->
    %% Capture current state (unless Nok is 'nok', then parent will handle)
    Progress = case Nok of
        nok -> undefined;
        _ -> capture_progress(T)
    end,
    
    Attempt = #attempt{
        kind = attempt,
        ok = Ok,
        nok = Nok,
        progress = Progress
    },
    
    T#tokenizer{
        attempts = [Attempt | T#tokenizer.attempts]
    }.
```

#### 3.2 check/3 - Try with Always Revert

```erlang
-spec check(tokenizer(), state_result(), state_result()) -> tokenizer().
%% @doc Stack a check: always reverts, goes to Ok or Nok based on result.
%%
%% Like attempt/3 but ALWAYS reverts state, even on success.
%% Used for lookahead checks.
%%
%% Example:
%%   T1 = erlmd_tokenizer:check(T, {next, found_marker}, {next, not_found})
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
```

#### 3.3 handle_attempt_result/2 - Process Ok/Nok

```erlang
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
```

#### 3.4 Progress Capture/Restore

```erlang
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
    Events = lists:nthtail(
        length(T#tokenizer.events) - P#progress.events_len,
        T#tokenizer.events
    ),
    Stack = lists:nthtail(
        length(T#tokenizer.stack) - P#progress.stack_len,
        T#tokenizer.stack
    ),
    
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
```

### Key Functions - Part 4: Main Loop

#### 4.1 feed/3 - Feed Bytes and Run State Machine

```erlang
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
    %% Get next byte via current/1
    NextByte = case Index < byte_size(Bytes) of
        true when Vs =:= 0 -> binary:at(Bytes, Index);
        true when Vs > 0 -> $\s;  % Virtual space
        false -> eof
    end,
    
    T#tokenizer{
        current = NextByte,
        consumed = false
    };
prepare_byte(#tokenizer{consumed = false}) ->
    error(byte_not_consumed).
```

### Key Functions - Part 5: Finalization

#### 5.1 finalize/1 - Get Final Events

```erlang
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
```

### Export List

```erlang
%% Exports
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
```

---

## Module 2.2: erlmd_state.erl

### Overview

The state dispatcher maps state names (atoms) to construct functions. In Phase 2, we only implement a **skeleton** - actual constructs come in later phases.

### Implementation

```erlang
-module(erlmd_state).

-include("types.hrl").

-export([call/2]).

-spec call(atom(), erlmd_tokenizer:tokenizer()) -> 
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Dispatch to a state function by name.
%%
%% This is THE central dispatch point for all state transitions.
%% Each state name maps to a construct module and function.
%%
%% In Phase 2, we only implement stubs. Real constructs come in later phases.
call(StateName, Tokenizer) ->
    case StateName of
        %% Phase 2: Only return 'ok' stub
        %% These will be implemented in later phases
        document_start -> stub(StateName, Tokenizer);
        
        %% More states will be added in future phases:
        %% flow_start -> erlmd_construct_flow:start(Tokenizer);
        %% paragraph_start -> erlmd_construct_paragraph:start(Tokenizer);
        %% etc.
        
        _ ->
            %% Unknown state
            error({unknown_state, StateName})
    end.

%% @private
%% Stub for unimplemented states
stub(StateName, Tokenizer) ->
    %% In Phase 2, just return ok
    %% In later phases, this will error if called
    io:format("STUB: State ~p called~n", [StateName]),
    {ok, Tokenizer}.
```

### Notes

- This module will grow SIGNIFICANTLY in later phases
- Each construct will add multiple state functions
- Keep exports organized by construct type

---

## Module 2.3: Helper Utilities

### erlmd_util_position.erl

Optional helper for working with positions:

```erlang
-module(erlmd_util_position).

-include("types.hrl").

-export([
    shift_point/3,
    points_equal/2,
    point_before/2
]).

%% @doc Shift a point forward by N bytes.
shift_point(Point, Bytes, N) ->
    %% Implementation similar to Rust Point::shift_to
    %% This is optional - can be added when needed
    Point.

%% @doc Check if two points are at the same position.
points_equal(#point{offset = O}, #point{offset = O}) -> true;
points_equal(_, _) -> false.

%% @doc Check if point A is before point B.
point_before(#point{offset = A}, #point{offset = B}) -> A < B.
```

---

## Testing Requirements

### Unit Tests - erlmd_tokenizer_test.erl

```erlang
-module(erlmd_tokenizer_test).
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%====================================================================
%% Creation Tests
%%====================================================================

new_tokenizer_test() ->
    T = erlmd_tokenizer:new(<<"hello">>, #{}),
    ?assertEqual(0, T#tokenizer.index),
    ?assertEqual(1, T#tokenizer.line),
    ?assertEqual(1, T#tokenizer.column),
    ?assertEqual([], T#tokenizer.events),
    ?assertEqual([], T#tokenizer.stack).

%%====================================================================
%% Current/Consume Tests
%%====================================================================

current_on_first_byte_test() ->
    T = erlmd_tokenizer:new(<<"abc">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

current_at_eof_test() ->
    T = erlmd_tokenizer:new(<<"">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    ?assertEqual(eof, erlmd_tokenizer:current(T1)).

consume_simple_test() ->
    T = erlmd_tokenizer:new(<<"abc">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),
    ?assertEqual(1, T2#tokenizer.index),
    ?assertEqual(2, T2#tokenizer.column),
    ?assertEqual($a, T2#tokenizer.previous).

consume_sequence_test() ->
    T = erlmd_tokenizer:new(<<"abc">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),
    T5 = erlmd_tokenizer:prepare_byte(T4),
    T6 = erlmd_tokenizer:consume(T5),
    
    ?assertEqual(3, T6#tokenizer.index),
    ?assertEqual(4, T6#tokenizer.column),
    ?assertEqual($c, T6#tokenizer.previous).

%%====================================================================
%% Line Ending Tests
%%====================================================================

consume_newline_test() ->
    T = erlmd_tokenizer:new(<<"a\nb">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),  % consume 'a'
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),  % consume '\n'
    
    ?assertEqual(2, T4#tokenizer.line),
    ?assertEqual(1, T4#tokenizer.column),
    ?assertEqual($\n, T4#tokenizer.previous).

consume_crlf_test() ->
    T = erlmd_tokenizer:new(<<"a\r\nb">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    T2 = erlmd_tokenizer:consume(T1),  % consume 'a'
    T3 = erlmd_tokenizer:prepare_byte(T2),
    T4 = erlmd_tokenizer:consume(T3),  % consume '\r' (ignored)
    T5 = erlmd_tokenizer:prepare_byte(T4),
    T6 = erlmd_tokenizer:consume(T5),  % consume '\n'
    
    ?assertEqual(2, T6#tokenizer.line),
    ?assertEqual(1, T6#tokenizer.column).

%%====================================================================
%% Tab Expansion Tests
%%====================================================================

consume_tab_test() ->
    T = erlmd_tokenizer:new(<<"\tX">>, #{}),
    T1 = erlmd_tokenizer:prepare_byte(T),
    
    %% Tab at column 1 expands to 4 spaces
    ?assertEqual($\t, erlmd_tokenizer:current(T1)),
    T2 = erlmd_tokenizer:consume(T1),
    ?assertEqual(5, T2#tokenizer.column),  % 1 + 4
    
    T3 = erlmd_tokenizer:prepare_byte(T2),
    ?assertEqual($X, erlmd_tokenizer:current(T3)).

%%====================================================================
%% Event Tests
%%====================================================================

enter_exit_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:exit(T1, paragraph),
    
    ?assertEqual([], T2#tokenizer.stack),
    ?assertEqual(2, length(T2#tokenizer.events)),
    
    [Exit, Enter] = T2#tokenizer.events,  % Reversed
    ?assertEqual(enter, Enter#event.kind),
    ?assertEqual(paragraph, Enter#event.name),
    ?assertEqual(exit, Exit#event.kind),
    ?assertEqual(paragraph, Exit#event.name).

nested_events_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:enter(T1, emphasis),
    T3 = erlmd_tokenizer:exit(T2, emphasis),
    T4 = erlmd_tokenizer:exit(T3, paragraph),
    
    ?assertEqual([], T4#tokenizer.stack),
    ?assertEqual(4, length(T4#tokenizer.events)).

exit_mismatch_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    ?assertError({exit_mismatch, _}, erlmd_tokenizer:exit(T1, heading_atx)).

%%====================================================================
%% Attempt Tests
%%====================================================================

attempt_success_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:attempt(T, ok, nok),
    
    ?assertEqual(1, length(T1#tokenizer.attempts)),
    
    {ok, T2} = erlmd_tokenizer:handle_attempt_result(T1, ok),
    ?assertEqual(0, length(T2#tokenizer.attempts)).

attempt_failure_reverts_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:attempt(T1, ok, nok),
    T3 = erlmd_tokenizer:enter(T2, emphasis),
    
    %% Should have 2 events and 2 stack items
    ?assertEqual(2, length(T3#tokenizer.events)),
    ?assertEqual(2, length(T3#tokenizer.stack)),
    
    %% Attempt fails - should revert to T1 state
    {nok, T4} = erlmd_tokenizer:handle_attempt_result(T3, nok),
    
    %% Should revert to 1 event and 1 stack item
    ?assertEqual(1, length(T4#tokenizer.events)),
    ?assertEqual(1, length(T4#tokenizer.stack)).

check_always_reverts_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    T1 = erlmd_tokenizer:enter(T, paragraph),
    T2 = erlmd_tokenizer:check(T1, ok, nok),
    T3 = erlmd_tokenizer:enter(T2, emphasis),
    
    %% Check succeeds but still reverts
    {ok, T4} = erlmd_tokenizer:handle_attempt_result(T3, ok),
    
    %% Should revert to T1 state even though ok
    ?assertEqual(1, length(T4#tokenizer.events)),
    ?assertEqual(1, length(T4#tokenizer.stack)).

%%====================================================================
%% Integration Tests
%%====================================================================

simple_tokenize_test() ->
    %% This will use stub from erlmd_state
    T = erlmd_tokenizer:new(<<"# Hello">>, #{}),
    {ok, T1} = erlmd_tokenizer:feed(T, document_start, <<"# Hello">>),
    {ok, _Events} = erlmd_tokenizer:finalize(T1).
```

### State Dispatcher Tests - erlmd_state_test.erl

```erlang
-module(erlmd_state_test).
-include_lib("eunit/include/eunit.hrl").

dispatch_document_start_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    {ok, _T1} = erlmd_state:call(document_start, T).

dispatch_unknown_state_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    ?assertError({unknown_state, _}, erlmd_state:call(bogus_state, T)).
```

---

## Integration & Validation

### Integration with Parser (Preview)

In later phases, the parser will use tokenizer like this:

```erlang
%% From erlmd_parser.erl (to be implemented in Phase 3+)
parse(Markdown, Options) ->
    Tokenizer = erlmd_tokenizer:new(Markdown, Options),
    {Result, T1} = erlmd_tokenizer:feed(Tokenizer, document_start, Markdown),
    case Result of
        ok -> 
            erlmd_tokenizer:finalize(T1);
        nok ->
            {error, parse_failed}
    end.
```

### Validation Checks

Run these to validate Phase 2:

```bash
# Compile
rebar3 compile

# Run unit tests
rebar3 eunit

# Check for binary optimization warnings
rebar3 compile | grep "binary"

# Run dialyzer
rebar3 dialyzer
```

---

## Acceptance Criteria

- [ ] `erlmd_tokenizer:new/2` creates a valid tokenizer
- [ ] `current/1` returns correct byte without consuming
- [ ] `consume/1` advances position correctly
- [ ] Line endings handled: `\n`, `\r\n`, `\r`
- [ ] Tabs expand correctly (TAB_SIZE = 4)
- [ ] CRLF recognized and handled as single newline
- [ ] `enter/2` and `exit/2` generate proper events
- [ ] `exit/2` validates matching enter
- [ ] Events are accumulated in reverse order
- [ ] `attempt/3` captures and restores state on failure
- [ ] `check/3` always restores state
- [ ] Nested attempts work correctly
- [ ] `erlmd_state:call/2` dispatches correctly
- [ ] Unknown states return appropriate error
- [ ] All unit tests pass
- [ ] No compiler warnings
- [ ] No dialyzer warnings
- [ ] Binary optimization info shows no sub-binary creation in hot paths
- [ ] Code is tail-recursive (no stack overflow on large input)

---

## Implementation Notes

### Critical Performance Considerations

1. **Never Create Sub-Binaries in Hot Path**
   - Use `binary:at/2` NOT pattern matching on `Bytes`
   - The tokenizer record should NEVER pattern match on `#tokenizer{bytes = <<...>>}`

2. **Tail Recursion is Mandatory**
   - Every loop must be tail recursive
   - Use accumulators
   - Never process results after recursive call

3. **Match Context Preservation**
   - Pattern match in function heads, not bodies
   - Keep binary references intact

### Common Pitfalls to Avoid

```erlang
%% DON'T DO THIS - Creates sub-binary
process(#tokenizer{bytes = <<Byte, _/binary>>} = T) ->
    %% BAD!
    ...

%% DO THIS - Use binary:at
process(T) ->
    Byte = binary:at(T#tokenizer.bytes, T#tokenizer.index),
    ...
```

```erlang
%% DON'T DO THIS - Not tail recursive
consume_n(T, 0) -> T;
consume_n(T, N) ->
    T1 = consume_n(T, N-1),  % NOT A TAIL CALL
    erlmd_tokenizer:consume(T1).

%% DO THIS - Tail recursive
consume_n(T, 0) -> T;
consume_n(T, N) ->
    T1 = erlmd_tokenizer:consume(T),
    consume_n(T1, N-1).  % TAIL CALL
```

### Debugging Tips

```erlang
%% Add to tokenizer for debugging
-define(DEBUG(Fmt, Args), io:format("[~p:~p] " ++ Fmt ++ "~n", [?MODULE, ?LINE | Args])).

%% Use in functions:
consume(T) ->
    ?DEBUG("Consuming byte ~p at ~p:~p", [T#tokenizer.current, T#tokenizer.line, T#tokenizer.column]),
    ...
```

### What NOT to Implement Yet

Phase 2 is ONLY the framework. Do NOT implement:

- ❌ Actual construct parsers (paragraphs, headings, etc.)
- ❌ Subtokenization
- ❌ Resolvers
- ❌ HTML generation
- ❌ AST generation

These come in later phases!

---

## Summary

Phase 2 delivers:

✅ **erlmd_tokenizer.erl** - Complete state machine driver  
✅ **erlmd_state.erl** - State dispatcher skeleton  
✅ **Comprehensive unit tests** - All core functions tested  
✅ **Performance patterns** - Binary optimization, tail recursion  
✅ **Backtracking support** - Attempt/check mechanism  
✅ **Event generation** - Enter/exit with validation  

**Next Phase**: Phase 3 will implement the first simple constructs (blank lines, data, whitespace) that use this tokenizer framework.

---

## Estimated Effort

- **Module 2.1 (Tokenizer)**: 3-4 days
- **Module 2.2 (State)**: 0.5 day  
- **Testing**: 1-2 days
- **Documentation & Review**: 0.5 day

**Total**: ~5-7 days of focused work

---

**End of Phase 2 Implementation Plan**
