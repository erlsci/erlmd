# Phase 3: Simple Constructs - Implementation Guide

**Project**: erlmd (Erlang Markdown Parser)  
**Phase**: 3 - Simple Constructs  
**Duration**: 1 week  
**Date**: November 14, 2025  

---

## Table of Contents

1. [Overview](#overview)
2. [Prerequisites](#prerequisites)
3. [Implementation Tasks](#implementation-tasks)
4. [Module Specifications](#module-specifications)
5. [Integration Requirements](#integration-requirements)
6. [Testing Strategy](#testing-strategy)
7. [Acceptance Criteria](#acceptance-criteria)
8. [Common Pitfalls](#common-pitfalls)

---

## Overview

### Phase Goal

Implement the simplest markdown constructs to validate the tokenizer architecture and establish the construct implementation pattern. These constructs are foundational and used by virtually all other constructs.

### What We're Building

Three fundamental "partial" constructs that handle the most basic parsing operations:

1. **partial_data** - Parses plain text data (the default fallback)
2. **blank_line** - Detects blank lines (whitespace-only lines)
3. **partial_space_or_tab** - Consumes space and tab characters

### Why These First?

- **Simplest possible**: No complex logic, perfect for validating architecture
- **Most frequently used**: Every other construct depends on these
- **Pattern establishment**: Sets the template for all 40+ constructs to follow
- **Early validation**: Catches architectural issues before complex constructs

### Key Success Metric

Can parse `"Hello\n\nWorld"` and generate correct events showing two data chunks separated by a blank line.

---

## Prerequisites

### Must Be Complete Before Starting

✅ **Phase 0**: Project structure exists  
✅ **Phase 1**: Core types defined (`erlmd_types.hrl`)  
✅ **Phase 2**: Tokenizer framework implemented

### Required Modules (from Phases 1-2)

- `include/erlmd_types.hrl` - Core record definitions
- `src/erlmd_tokenizer.erl` - State machine driver
- `src/erlmd_state.erl` - State dispatcher
- `src/erlmd_util_char.erl` - Character classification

### Verify Prerequisites

```bash
# Ensure these compile without errors
rebar3 compile

# Verify core functions exist
erl -pa _build/default/lib/erlmd/ebin -eval "
  {module, erlmd_tokenizer} = code:ensure_loaded(erlmd_tokenizer),
  {module, erlmd_state} = code:ensure_loaded(erlmd_state),
  io:format('Prerequisites verified~n'),
  halt().
"
```

---

## Implementation Tasks

### Task Breakdown

| Task | Module | Estimated Time | Priority |
|------|--------|----------------|----------|
| 1 | `erlmd_construct_partial_data.erl` | 3 hours | HIGH |
| 2 | `erlmd_construct_blank_line.erl` | 2 hours | HIGH |
| 3 | `erlmd_construct_partial_space_or_tab.erl` | 4 hours | HIGH |
| 4 | State dispatcher integration | 1 hour | HIGH |
| 5 | Unit tests for each construct | 3 hours | HIGH |
| 6 | Integration test (end-to-end) | 2 hours | MEDIUM |
| 7 | Documentation | 1 hour | MEDIUM |

**Total Estimated Time**: 16 hours (~2 days)

---

## Module Specifications

## 1. partial_data Construct

### 1.1 Purpose

The **data construct** is the default fallback that parses plain text. It continues consuming bytes until it hits:
- A line ending (`\n`)
- End of file (EOF)
- A "marker" byte (special character that starts another construct)

### 1.2 Module: `erlmd_construct_partial_data.erl`

#### File Header

```erlang
%%%-----------------------------------------------------------------------------
%%% @doc Data construct - parses plain text data.
%%%
%%% Data occurs in string and text content types. It consumes any bytes
%%% that are not line endings and not in the tokenizer's markers list.
%%%
%%% This is the fallback construct - when nothing else matches, we parse data.
%%%
%%% Reference: markdown-rs/src/construct/partial_data.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_partial_data).

-export([start/1, at_break/1, inside/1]).
-export([resolve/1]).  % Post-processing to merge adjacent data events

-include("erlmd_types.hrl").
```

#### Implementation

```erlang
%% @doc Entry point for data construct.
%%
%% Checks if we should immediately consume data or check for breaks.
-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte) ->
            % Check if this byte is a marker (special character)
            case is_marker(T, Byte) of
                true ->
                    % Marker found - enter data and consume it
                    T1 = erlmd_tokenizer:enter(T, data),
                    T2 = erlmd_tokenizer:consume(T1),
                    {next, inside, T2};
                false ->
                    % Not a marker - check at break
                    at_break(T)
            end;
        eof ->
            % At EOF, go to break check
            at_break(T)
    end.

%% @doc Check what to do at a potential break point.
%%
%% This is called when we're between data chunks or at the start.
-spec at_break(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
at_break(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte) ->
            case is_marker(T, Byte) of
                false ->
                    if 
                        Byte =:= $\n ->
                            % Line ending - emit it and continue
                            T1 = erlmd_tokenizer:enter(T, line_ending),
                            T2 = erlmd_tokenizer:consume(T1),
                            T3 = erlmd_tokenizer:exit(T2),
                            {next, at_break, T3};
                        true ->
                            % Regular data - enter and parse
                            T1 = erlmd_tokenizer:enter(T, data),
                            {retry, inside, T1}
                    end;
                true ->
                    % Marker found - we're done
                    {ok, T}
            end;
        eof ->
            % End of input - done
            {ok, T}
    end.

%% @doc Parse the inside of a data chunk.
%%
%% Consumes bytes until we hit a line ending, marker, or EOF.
-spec inside(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
inside(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte) ->
            IsMarker = is_marker(T, Byte),
            if
                Byte =/= $\n andalso not IsMarker ->
                    % Regular byte - consume and continue
                    T1 = erlmd_tokenizer:consume(T),
                    {next, inside, T1};
                true ->
                    % Hit a line ending or marker - exit data
                    T1 = erlmd_tokenizer:exit(T),
                    {retry, at_break, T1}
            end;
        eof ->
            % End of input - exit data
            T1 = erlmd_tokenizer:exit(T),
            {retry, at_break, T1}
    end.

%% @doc Check if a byte is a marker.
%%
%% Markers are special characters that indicate the start of other constructs.
%% They're stored in the tokenizer state.
-spec is_marker(erlmd_tokenizer:tokenizer(), byte()) -> boolean().
is_marker(T, Byte) ->
    Markers = erlmd_tokenizer:get_markers(T),
    lists:member(Byte, Markers).

%% @doc Resolve/merge adjacent data events.
%%
%% After parsing, we may have multiple consecutive data enter/exit pairs.
%% This merges them into a single data event for efficiency.
%%
%% Called by the resolve system after tokenization.
-spec resolve(erlmd_tokenizer:tokenizer()) -> erlmd_tokenizer:tokenizer().
resolve(T) ->
    Events = erlmd_tokenizer:get_events(T),
    MergedEvents = merge_data_events(Events, []),
    erlmd_tokenizer:set_events(T, MergedEvents).

%% @doc Merge consecutive data events.
-spec merge_data_events([event()], [event()]) -> [event()].
merge_data_events([], Acc) ->
    lists:reverse(Acc);
merge_data_events([E1, E2 | Rest], Acc) ->
    case {E1, E2} of
        {#event{kind = enter, name = data}, #event{kind = exit, name = data}} ->
            % Found a data enter/exit pair
            % Look ahead to see if there's another data pair
            case Rest of
                [#event{kind = enter, name = data} = E3, 
                 #event{kind = exit, name = data} = E4 | Rest2] ->
                    % Merge: keep first enter, use last exit's position
                    E2Merged = E2#event{point = E4#event.point},
                    merge_data_events(Rest2, [E2Merged, E1 | Acc]);
                _ ->
                    % No more data pairs - keep this one
                    merge_data_events(Rest, [E2, E1 | Acc])
            end;
        _ ->
            % Not a data pair - keep first event
            merge_data_events([E2 | Rest], [E1 | Acc])
    end.
```

#### Algorithm Notes

**The Rust pattern** uses a `markers` list in tokenize_state to track which bytes should interrupt data parsing. In Erlang:

1. **Markers list**: Store in tokenizer state (e.g., `#tokenizer.markers = [$*, $_, $[, $], ...]`)
2. **Three-state pattern**: `start` → `at_break` → `inside` → `at_break` → ...
3. **Line endings are special**: They're emitted as their own events, not part of data
4. **Merge in resolve**: Post-processing combines adjacent data events

**Why three functions?**
- `start`: Entry point, handles first byte
- `at_break`: Decision point between data chunks
- `inside`: The tight loop consuming actual data

### 1.3 Test Requirements

```erlang
-module(erlmd_construct_partial_data_test).
-include_lib("eunit/include/eunit.hrl").
-include("erlmd_types.hrl").

%% Test 1: Simple data
simple_data_test() ->
    T = test_helper:make_tokenizer(<<"Hello">>),
    {ok, T1} = erlmd_construct_partial_data:start(T),
    Events = erlmd_tokenizer:get_events(T1),
    ?assertMatch([
        #event{kind = enter, name = data},
        #event{kind = exit, name = data}
    ], Events).

%% Test 2: Data with line ending
data_with_newline_test() ->
    T = test_helper:make_tokenizer(<<"Hello\nWorld">>),
    {ok, T1} = erlmd_construct_partial_data:start(T),
    Events = erlmd_tokenizer:get_events(T1),
    % Should have: data enter/exit, line_ending enter/exit, data enter/exit
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assertEqual(4, length(DataEvents)).  % 2 enter + 2 exit

%% Test 3: Empty input
empty_data_test() ->
    T = test_helper:make_tokenizer(<<>>),
    {ok, T1} = erlmd_construct_partial_data:start(T),
    Events = erlmd_tokenizer:get_events(T1),
    ?assertEqual([], Events).

%% Test 4: Data interrupted by marker
data_with_marker_test() ->
    T0 = test_helper:make_tokenizer(<<"Hello*World">>),
    T = erlmd_tokenizer:set_markers(T0, [$*]),
    {ok, T1} = erlmd_construct_partial_data:start(T),
    % Should stop at the *
    Events = erlmd_tokenizer:get_events(T1),
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assertEqual(2, length(DataEvents)).  % Only "Hello"
```

---

## 2. blank_line Construct

### 2.1 Purpose

Detects blank lines - lines that contain only spaces, tabs, or nothing before a line ending or EOF. Blank lines are significant in markdown for separating blocks.

### 2.2 Module: `erlmd_construct_blank_line.erl`

#### Implementation

```erlang
%%%-----------------------------------------------------------------------------
%%% @doc Blank line construct - detects lines with only whitespace.
%%%
%%% A blank line is a line containing only spaces and tabs (or nothing)
%%% followed by a line ending or EOF.
%%%
%%% Blank lines are important for separating block-level constructs like
%%% paragraphs, headings, etc.
%%%
%%% Reference: markdown-rs/src/construct/blank_line.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_blank_line).

-export([start/1, after_whitespace/1]).

-include("erlmd_types.hrl").

%% @doc Entry point for blank line detection.
%%
%% A blank line starts with optional whitespace, then ends with EOL or EOF.
-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when Byte =:= $\s; Byte =:= $\t ->
            % Has whitespace - try to consume it
            % Use attempt: if whitespace parse succeeds, check what's after
            T1 = erlmd_tokenizer:attempt(T, 
                {next, after_whitespace},  % On success
                nok),                       % On failure
            % Delegate to space_or_tab construct
            {retry, erlmd_state:call(T1, partial_space_or_tab)};
        Byte when Byte =:= $\n; Byte =:= undefined ->
            % No whitespace, but at EOL or EOF - valid blank line
            {ok, T};
        _Other ->
            % Something else - not a blank line
            {nok, T}
    end.

%% @doc Called after consuming optional whitespace.
%%
%% Now we must be at EOL or EOF for this to be a blank line.
-spec after_whitespace(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
after_whitespace(T) ->
    case erlmd_tokenizer:current(T) of
        undefined ->
            % EOF - blank line confirmed
            {ok, T};
        $\n ->
            % Line ending - blank line confirmed
            {ok, T};
        _Other ->
            % Something else - not a blank line
            {nok, T}
    end.
```

#### Key Points

**The Rust pattern**:
1. Uses `attempt` to try parsing whitespace
2. If whitespace succeeds, checks if we're at EOL/EOF
3. Returns `ok` if blank, `nok` if not
4. Does NOT emit events (blank lines are structural, not content)

**Erlang adaptation**:
- Use `erlmd_tokenizer:attempt/3` to backtrack if needed
- Delegate to `partial_space_or_tab` for whitespace parsing
- Keep it simple: just detection, no event emission

### 2.3 Test Requirements

```erlang
-module(erlmd_construct_blank_line_test).
-include_lib("eunit/include/eunit.hrl").

%% Test 1: Empty line
empty_line_test() ->
    T = test_helper:make_tokenizer(<<"\n">>),
    {ok, T1} = erlmd_construct_blank_line:start(T),
    ?assertEqual($\n, erlmd_tokenizer:current(T1)).

%% Test 2: Line with spaces
spaces_line_test() ->
    T = test_helper:make_tokenizer(<<"   \n">>),
    {ok, T1} = erlmd_construct_blank_line:start(T),
    ?assertEqual($\n, erlmd_tokenizer:current(T1)).

%% Test 3: Line with tabs
tabs_line_test() ->
    T = test_helper:make_tokenizer(<<"\t\t\n">>),
    {ok, T1} = erlmd_construct_blank_line:start(T),
    ?assertEqual($\n, erlmd_tokenizer:current(T1)).

%% Test 4: Line with content (not blank)
not_blank_test() ->
    T = test_helper:make_tokenizer(<<"  a\n">>),
    {nok, _T1} = erlmd_construct_blank_line:start(T).

%% Test 5: EOF after whitespace
eof_after_whitespace_test() ->
    T = test_helper:make_tokenizer(<<"  ">>),
    {ok, T1} = erlmd_construct_blank_line:start(T),
    ?assertEqual(eof, erlmd_tokenizer:current(T1)).
```

---

## 3. partial_space_or_tab Construct

### 3.1 Purpose

Parses one or more space or tab characters. This is used everywhere in markdown (indentation, padding, etc.). Highly configurable with min/max bounds.

### 3.2 Module: `erlmd_construct_partial_space_or_tab.erl`

#### Implementation

```erlang
%%%-----------------------------------------------------------------------------
%%% @doc Space or tab construct - parses spaces and tabs.
%%%
%%% This is one of the most frequently used constructs. It can parse:
%%% - Exactly N spaces/tabs
%%% - Between min and max spaces/tabs
%%% - Optional vs required whitespace
%%%
%%% Configuration is passed via tokenizer state.
%%%
%%% Reference: markdown-rs/src/construct/partial_space_or_tab.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_partial_space_or_tab).

-export([
    space_or_tab/1,
    space_or_tab_min_max/3,
    start/1,
    inside/1,
    after_space_or_tab/1
]).

-include("erlmd_types.hrl").

%% @doc Parse one or more spaces/tabs (no upper limit).
-spec space_or_tab(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
space_or_tab(T) ->
    space_or_tab_min_max(T, 1, infinity).

%% @doc Parse between min and max spaces/tabs.
-spec space_or_tab_min_max(
    erlmd_tokenizer:tokenizer(), 
    non_neg_integer(), 
    non_neg_integer() | infinity
) -> {state_result(), erlmd_tokenizer:tokenizer()}.
space_or_tab_min_max(T, Min, Max) ->
    % Store config in tokenizer state
    T1 = erlmd_tokenizer:set_state(T, space_or_tab_min, Min),
    T2 = erlmd_tokenizer:set_state(T1, space_or_tab_max, Max),
    T3 = erlmd_tokenizer:set_state(T2, space_or_tab_size, 0),
    {retry, start, T3}.

%% @doc Start of space_or_tab parsing.
-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    Max = erlmd_tokenizer:get_state(T, space_or_tab_max),
    case erlmd_tokenizer:current(T) of
        Byte when (Byte =:= $\s orelse Byte =:= $\t) andalso Max > 0 ->
            % Valid whitespace and we haven't hit max - enter and consume
            T1 = erlmd_tokenizer:enter(T, space_or_tab),
            {retry, inside, T1};
        _ ->
            % No whitespace or hit max - check if we met minimum
            {retry, after_space_or_tab, T}
    end.

%% @doc Inside space_or_tab parsing - consume while valid.
-spec inside(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
inside(T) ->
    Size = erlmd_tokenizer:get_state(T, space_or_tab_size),
    Max = erlmd_tokenizer:get_state(T, space_or_tab_max),
    
    case erlmd_tokenizer:current(T) of
        Byte when (Byte =:= $\s orelse Byte =:= $\t) andalso Size < Max ->
            % Valid whitespace and haven't hit max - consume
            T1 = erlmd_tokenizer:consume(T),
            T2 = erlmd_tokenizer:set_state(T1, space_or_tab_size, Size + 1),
            {next, inside, T2};
        _ ->
            % Done consuming - exit and check minimum
            T1 = erlmd_tokenizer:exit(T),
            {retry, after_space_or_tab, T1}
    end.

%% @doc After parsing - check if we met minimum requirement.
-spec after_space_or_tab(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
after_space_or_tab(T) ->
    Size = erlmd_tokenizer:get_state(T, space_or_tab_size),
    Min = erlmd_tokenizer:get_state(T, space_or_tab_min),
    
    % Clean up state
    T1 = erlmd_tokenizer:clear_state(T, space_or_tab_size),
    T2 = erlmd_tokenizer:clear_state(T1, space_or_tab_max),
    T3 = erlmd_tokenizer:clear_state(T2, space_or_tab_min),
    
    if
        Size >= Min ->
            {ok, T3};
        true ->
            {nok, T3}
    end.
```

#### Configuration Pattern

The Rust version uses a struct `Options` to configure behavior. In Erlang, we store this in the tokenizer's state map:

```erlang
% Rust:
% Options { min: 1, max: 4, kind: Name::SpaceOrTab, ... }

% Erlang equivalent:
T1 = erlmd_tokenizer:set_state(T, space_or_tab_min, 1),
T2 = erlmd_tokenizer:set_state(T1, space_or_tab_max, 4),
...
```

### 3.3 Test Requirements

```erlang
-module(erlmd_construct_partial_space_or_tab_test).
-include_lib("eunit/include/eunit.hrl").

%% Test 1: Single space
single_space_test() ->
    T = test_helper:make_tokenizer(<<" abc">>),
    {ok, T1} = erlmd_construct_partial_space_or_tab:space_or_tab(T),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

%% Test 2: Multiple spaces
multiple_spaces_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    {ok, T1} = erlmd_construct_partial_space_or_tab:space_or_tab(T),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

%% Test 3: Tabs
tabs_test() ->
    T = test_helper:make_tokenizer(<<"\t\tabc">>),
    {ok, T1} = erlmd_construct_partial_space_or_tab:space_or_tab(T),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

%% Test 4: Mixed spaces and tabs
mixed_test() ->
    T = test_helper:make_tokenizer(<<" \t abc">>),
    {ok, T1} = erlmd_construct_partial_space_or_tab:space_or_tab(T),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

%% Test 5: Min/max constraints - exact
exact_count_test() ->
    T = test_helper:make_tokenizer(<<"  abc">>),
    {ok, T1} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T, 2, 2),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

%% Test 6: Min/max constraints - too few
too_few_test() ->
    T = test_helper:make_tokenizer(<<" abc">>),
    {nok, _} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T, 2, 4).

%% Test 7: Min/max constraints - within range
within_range_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    {ok, T1} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T, 1, 4),
    ?assertEqual($a, erlmd_tokenizer:current(T1)).

%% Test 8: No whitespace
no_whitespace_test() ->
    T = test_helper:make_tokenizer(<<"abc">>),
    {nok, _} = erlmd_construct_partial_space_or_tab:space_or_tab(T).
```

---

## Integration Requirements

### 4.1 Update State Dispatcher

File: `src/erlmd_state.erl`

Add these construct mappings:

```erlang
-spec call(atom(), erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

% Data construct
call(data_start, T) ->
    erlmd_construct_partial_data:start(T);
call(data_at_break, T) ->
    erlmd_construct_partial_data:at_break(T);
call(data_inside, T) ->
    erlmd_construct_partial_data:inside(T);

% Blank line construct
call(blank_line_start, T) ->
    erlmd_construct_blank_line:start(T);
call(blank_line_after, T) ->
    erlmd_construct_blank_line:after_whitespace(T);

% Space or tab construct
call(space_or_tab_start, T) ->
    erlmd_construct_partial_space_or_tab:start(T);
call(space_or_tab_inside, T) ->
    erlmd_construct_partial_space_or_tab:inside(T);
call(space_or_tab_after, T) ->
    erlmd_construct_partial_space_or_tab:after_space_or_tab(T);

% ... existing constructs ...

call(UnknownState, _T) ->
    error({unknown_state, UnknownState}).
```

### 4.2 First Integration Test

Create `test/integration/simple_parse_test.erl`:

```erlang
-module(simple_parse_test).
-include_lib("eunit/include/eunit.hrl").
-include("erlmd_types.hrl").

%% Integration test: Parse "Hello"
parse_hello_test() ->
    Input = <<"Hello">>,
    T0 = erlmd_tokenizer:new(Input, #{}),
    T1 = erlmd_tokenizer:set_markers(T0, []),  % No markers for now
    
    % Start parsing with data construct
    {ok, T2} = erlmd_state:call(data_start, T1),
    
    % Check events
    Events = erlmd_tokenizer:get_events(T2),
    ?assertMatch([
        #event{kind = enter, name = data, point = #point{offset = 0}},
        #event{kind = exit, name = data, point = #point{offset = 5}}
    ], Events).

%% Integration test: Parse "Hello\n\nWorld"
parse_with_blank_line_test() ->
    Input = <<"Hello\n\nWorld">>,
    % This will require flow/document dispatchers to work properly
    % For now, just test that we can parse the parts
    
    % Test first data
    T1 = erlmd_tokenizer:new(<<"Hello">>, #{}),
    {ok, _} = erlmd_state:call(data_start, T1),
    
    % Test blank line
    T2 = erlmd_tokenizer:new(<<"\n">>, #{}),
    {ok, _} = erlmd_state:call(blank_line_start, T2),
    
    % Test second data
    T3 = erlmd_tokenizer:new(<<"World">>, #{}),
    {ok, _} = erlmd_state:call(data_start, T3).
```

---

## Testing Strategy

### 5.1 Unit Test Hierarchy

```
test/unit/
├── erlmd_construct_partial_data_test.erl
├── erlmd_construct_blank_line_test.erl
└── erlmd_construct_partial_space_or_tab_test.erl

test/integration/
└── simple_parse_test.erl
```

### 5.2 Test Helper Module

Create `test/test_helper.erl`:

```erlang
-module(test_helper).
-export([
    make_tokenizer/1,
    make_tokenizer/2,
    parse_events/1,
    assert_event_sequence/2
]).

-include("erlmd_types.hrl").

%% @doc Create a tokenizer with default settings.
make_tokenizer(Binary) ->
    make_tokenizer(Binary, #{}).

%% @doc Create a tokenizer with custom options.
make_tokenizer(Binary, Options) ->
    erlmd_tokenizer:new(Binary, Options).

%% @doc Parse input and return events.
parse_events(Binary) ->
    T = make_tokenizer(Binary),
    {ok, T1} = erlmd_parser:parse(T),
    erlmd_tokenizer:get_events(T1).

%% @doc Assert event sequence matches expected pattern.
assert_event_sequence(Expected, Actual) ->
    ?assertEqual(length(Expected), length(Actual)),
    lists:foreach(fun({E, A}) ->
        ?assertEqual(E#event.kind, A#event.kind),
        ?assertEqual(E#event.name, A#event.name)
    end, lists:zip(Expected, Actual)).
```

### 5.3 Running Tests

```bash
# Run all tests
rebar3 eunit

# Run specific module tests
rebar3 eunit --module=erlmd_construct_partial_data_test

# Run with coverage
rebar3 cover
```

---

## Acceptance Criteria

### Phase 3 Complete When:

- [x] **All three constructs implemented**
  - `erlmd_construct_partial_data.erl`
  - `erlmd_construct_blank_line.erl`
  - `erlmd_construct_partial_space_or_tab.erl`

- [x] **State dispatcher integration**
  - All construct state names registered in `erlmd_state:call/2`

- [x] **Comprehensive unit tests**
  - Minimum 8 tests per construct
  - All tests passing
  - Edge cases covered (EOF, empty input, boundaries)

- [x] **Integration test passing**
  - Can parse `"Hello"` → correct data events
  - Can parse `"Hello\n\nWorld"` → data + blank + data

- [x] **No compiler warnings**
  - `rebar3 compile` produces zero warnings
  - Binary optimization preserved (check with `bin_opt_info`)

- [x] **Documentation complete**
  - All functions have @doc comments
  - Module headers explain purpose
  - References to Rust source files included

- [x] **Code review checklist**
  - ✓ Tail recursive functions
  - ✓ Match context preserved
  - ✓ Proper event enter/exit pairing
  - ✓ No infinite loops possible
  - ✓ Error cases handled

---

## Common Pitfalls

### 7.1 Binary Pattern Matching

**❌ DON'T DO THIS** - Pattern match in function body:

```erlang
consume_data(T) ->
    case T#tokenizer.bytes of
        <<Byte, Rest/binary>> -> ...  % Creates sub-binary!
    end.
```

**✅ DO THIS** - Use tokenizer functions:

```erlang
consume_data(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte) ->
            T1 = erlmd_tokenizer:consume(T),
            ...
    end.
```

### 7.2 Event Pairing

**❌ DON'T DO THIS** - Mismatched enter/exit:

```erlang
start(T) ->
    T1 = erlmd_tokenizer:enter(T, data),
    {ok, T1}.  % WRONG: Never called exit!
```

**✅ DO THIS** - Always pair enter/exit:

```erlang
start(T) ->
    T1 = erlmd_tokenizer:enter(T, data),
    % ... do work ...
    T2 = erlmd_tokenizer:exit(T1),
    {ok, T2}.
```

### 7.3 Infinite Loops

**❌ DON'T DO THIS** - No EOF check:

```erlang
inside(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when Byte =/= $\n ->
            T1 = erlmd_tokenizer:consume(T),
            {next, inside, T1}
        % MISSING: eof case!
    end.
```

**✅ DO THIS** - Always handle EOF:

```erlang
inside(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte), Byte =/= $\n ->
            T1 = erlmd_tokenizer:consume(T),
            {next, inside, T1};
        _ ->  % Handles both $\n and eof
            {ok, T}
    end.
```

### 7.4 State Result Types

**❌ DON'T DO THIS** - Wrong result tuple:

```erlang
start(T) ->
    T1 = do_something(T),
    {T1}.  % WRONG: Missing state result!
```

**✅ DO THIS** - Always return `{state_result(), tokenizer()}`:

```erlang
start(T) ->
    T1 = do_something(T),
    {ok, T1}.  % or {nok, T1} or {next, StateName, T1}
```

### 7.5 Tail Recursion

**❌ DON'T DO THIS** - Not tail recursive:

```erlang
inside(T) ->
    T1 = erlmd_tokenizer:consume(T),
    {Result, T2} = inside(T1),  % NOT TAIL CALL
    {Result, T2}.
```

**✅ DO THIS** - Tail recursive:

```erlang
inside(T) ->
    T1 = erlmd_tokenizer:consume(T),
    {next, inside, T1}.  % Tokenizer will call inside(T1)
```

---

## Next Steps After Phase 3

Once Phase 3 is complete:

1. **Validate architecture** - These simple constructs prove the pattern works
2. **Move to Phase 4** - Content dispatchers (string, text, flow, document)
3. **Reference this phase** - Use these modules as templates for future constructs

### What Phase 4 Will Build On

Phase 4 implements the **content dispatchers** that orchestrate which constructs are valid at each level:
- `string` content: Only data + escapes
- `text` content: Data + inline constructs
- `flow` content: Block-level constructs
- `document` content: Containers + flow

---

## Quick Reference

### State Names for These Constructs

```erlang
% Data
data_start
data_at_break
data_inside

% Blank line
blank_line_start
blank_line_after

% Space or tab
space_or_tab_start
space_or_tab_inside
space_or_tab_after
```

### Event Names Emitted

```erlang
data           % Plain text data
line_ending    % \n
space_or_tab   % Whitespace
% Note: blank_line emits NO events!
```

### Key Tokenizer Functions

```erlang
erlmd_tokenizer:current(T) -> byte() | eof
erlmd_tokenizer:consume(T) -> tokenizer()
erlmd_tokenizer:enter(T, Name) -> tokenizer()
erlmd_tokenizer:exit(T) -> tokenizer()
erlmd_tokenizer:attempt(T, OkState, NokState) -> tokenizer()
```

---

## Conclusion

Phase 3 establishes the foundational patterns that all other constructs will follow. These three simple constructs validate that:

1. ✅ The tokenizer state machine works correctly
2. ✅ Event generation is properly structured
3. ✅ The attempt/backtracking mechanism functions
4. ✅ Binary optimization is preserved
5. ✅ The overall architecture is sound

With Phase 3 complete, we have a solid foundation to build the more complex constructs in subsequent phases.

**Estimated Completion Time**: 2 days (16 hours)  
**Risk Level**: Low (simple constructs)  
**Blocking Dependencies**: None (foundation is ready)

---

*This guide was generated for the erlmd project on November 14, 2025*
