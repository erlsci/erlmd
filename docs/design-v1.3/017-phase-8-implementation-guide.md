# Phase 8 Implementation Guide: Complex Block Constructs

**Phase**: 8 - Complex Block Constructs  
**Duration**: 2.5 weeks  
**Status**: Ready for Implementation  
**Date**: November 15, 2025

---

## Table of Contents

1. [Phase Overview](#phase-overview)
2. [Module 8.1: Block Quote Construct](#module-81-block-quote-construct)
3. [Module 8.2: List Item Construct](#module-82-list-item-construct)
4. [Integration Requirements](#integration-requirements)
5. [Testing Strategy](#testing-strategy)
6. [Common Pitfalls](#common-pitfalls)
7. [Acceptance Criteria](#acceptance-criteria)

---

## Phase Overview

### Goal

Implement the two most complex block-level constructs in CommonMark:
1. **Block quotes** (`>`) - Nested container construct
2. **List items** - Both ordered (`1.`) and unordered (`*`, `-`, `+`)

These constructs are the most challenging in the specification due to:
- Container semantics (they contain other block content)
- Continuation rules (lazy continuation, blank lines)
- Indentation tracking and nesting
- Loose vs tight list detection
- Complex interaction with other constructs

### Architecture Context

Block quotes and lists are **container constructs** that:
- Are called by the `flow` dispatcher
- Parse flow content inside themselves
- Track indentation for continuation
- Support nesting (lists in quotes, quotes in lists, etc.)
- Require resolver logic for list grouping

### Dependencies

**Required Modules** (must be complete):
- ✅ `erlmd_tokenizer` - State machine operations
- ✅ `erlmd_state` - Dispatcher
- ✅ `erlmd_cnstr_flow` - Flow content dispatcher  
- ✅ `erlmd_cnstr_document` - Document container
- ✅ `erlmd_cnstr_prtl_space_or_tab` - Whitespace parsing
- ✅ `erlmd_cnstr_blank_line` - Blank line detection

**New Modules** (to create):
- 🆕 `erlmd_cnstr_block_quote` - Block quote construct
- 🆕 `erlmd_cnstr_list_item` - List item construct

### Reference Materials

- **Rust Implementation**: 
  - `markdown-rs/src/construct/block_quote.rs`
  - `markdown-rs/src/construct/list_item.rs`
- **CommonMark Spec**: 
  - Section 5.1 - Block quotes
  - Section 5.2 - List items  
  - Section 5.3 - Lists
- **Erlang Patterns**: Binary optimization, tail recursion
- **Naming Convention**: v1.1 (see `012.3-naming-quick-reference.md`)

### Deliverables

1. ✅ Block quote construct implementation
2. ✅ List item marker detection (ordered & unordered)
3. ✅ List item continuation logic
4. ✅ List resolver (groups adjacent items)
5. ✅ Comprehensive test suites
6. ✅ 100+ additional CommonMark tests passing

---

## Module 8.1: Block Quote Construct

### Context

- **Module**: `src/erlmd_cnstr_block_quote.erl`
- **Priority**: HIGH
- **Complexity**: Medium
- **Estimated Time**: 3-4 days

### CommonMark Specification

From Section 5.1:

> A block quote marker consists of 0-3 spaces initial indentation, plus (a) the character `>` together with a following space of indentation, or (b) a single character `>` not followed by a space of indentation.

Key properties:
- Marker: `>` optionally followed by a space
- Can have 0-3 spaces before marker
- Contains flow content
- Supports lazy continuation (subsequent lines without `>`)
- Can be nested

### Implementation Requirements

1. **Entry Point**: Must be called from `flow` dispatcher
2. **Prefix Parsing**: Detect `>` marker with optional space after
3. **Content Parsing**: Delegate to `flow` dispatcher recursively
4. **Continuation**: Check each new line for `>` continuation
5. **Lazy Support**: Allow lines without `>` to continue the quote
6. **Nesting**: Support block quotes inside block quotes

### Record Definitions

No new records needed. Uses existing `#tokenizer{}` state.

### Key Functions

```erlang
-module(erlmd_cnstr_block_quote).
-export([start/1, cont_start/1]).

-include("types.hrl").

%% Public API
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
-spec cont_start(tokenizer()) -> {state_result(), tokenizer()}.

%% Internal states (private)
-spec cont_before(tokenizer()) -> {state_result(), tokenizer()}.
-spec cont_after(tokenizer()) -> {state_result(), tokenizer()}.
```

### Implementation Pattern

```erlang
-module(erlmd_cnstr_block_quote).
-export([start/1, cont_start/1]).

-include("types.hrl").

%% @doc Start of block quote.
%% Called by flow dispatcher.
%%
%% ```markdown
%% > | > a
%%     ^
%% ```
start(T) ->
    T1 = erlmd_tokenizer:enter(T, block_quote),
    cont_start(T1).

%% @doc Start of block quote continuation.
%% Also used to parse the first block quote opening.
%%
%% ```markdown
%%   | > a
%% > | > b
%%     ^
%% ```
cont_start(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $\t; C =:= $\s ->
            %% Optional whitespace before marker (0-3 spaces)
            %% Attempt to consume up to 3 spaces of indentation
            erlmd_tokenizer:attempt(
                T,
                {next, cont_before},
                nok,
                fun(T2) ->
                    erlmd_cnstr_prtl_space_or_tab:start(T2, #{
                        min => 1,
                        max => 3
                    })
                end
            );
        _ ->
            cont_before(T)
    end.

%% @doc At `>`, after optional whitespace.
%%
%% ```markdown
%%   | > a
%% > | > b
%%     ^
%% ```
cont_before(T) ->
    case erlmd_tokenizer:current(T) of
        $> ->
            T1 = erlmd_tokenizer:enter(T, block_quote_prefix),
            T2 = erlmd_tokenizer:enter(T1, block_quote_marker),
            T3 = erlmd_tokenizer:consume(T2),
            T4 = erlmd_tokenizer:exit(T3),
            cont_after(T4);
        _ ->
            {nok, T}
    end.

%% @doc After `>`, before optional whitespace.
%% Consumes a single optional space/tab.
%%
%% ```markdown
%% > | > a
%%      ^
%% > | >b
%%      ^
%% ```
cont_after(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $\t; C =:= $\s ->
            T1 = erlmd_tokenizer:enter(T, space_or_tab),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            {ok, erlmd_tokenizer:exit(T3)};
        _ ->
            {ok, erlmd_tokenizer:exit(T)}
    end.
```

### State Machine Flow

```
start
  ↓
  enter(block_quote)
  ↓
cont_start
  ↓
  [optional: 0-3 spaces via attempt]
  ↓
cont_before
  ↓
  check current == '>'
  ↓
  enter(block_quote_prefix)
  enter(block_quote_marker)
  consume('>')
  exit(block_quote_marker)
  ↓
cont_after
  ↓
  [optional: consume 1 space/tab]
  exit(block_quote_prefix)
  ↓
  {ok, T}
```

### Integration with Document Container

Block quotes participate in the document container system. The tokenizer's document container stack tracks:
- Current indentation level
- Whether continuation is required
- Lazy continuation state

### Rust Reference Patterns

From `block_quote.rs`:

```rust
// Start function - simple entry point
pub fn start(tokenizer: &mut Tokenizer) -> State {
    if tokenizer.parse_state.options.constructs.block_quote {
        tokenizer.enter(Name::BlockQuote);
        State::Retry(StateName::BlockQuoteContStart)
    } else {
        State::Nok
    }
}

// Continuation start - handles optional indentation
pub fn cont_start(tokenizer: &mut Tokenizer) -> State {
    if matches!(tokenizer.current, Some(b'\t' | b' ')) {
        tokenizer.attempt(State::Next(StateName::BlockQuoteContBefore), State::Nok);
        State::Retry(space_or_tab_min_max(
            tokenizer,
            1,
            TAB_SIZE - 1  // Max 3 spaces
        ))
    } else {
        State::Retry(StateName::BlockQuoteContBefore)
    }
}

// Marker detection
pub fn cont_before(tokenizer: &mut Tokenizer) -> State {
    match tokenizer.current {
        Some(b'>') => {
            tokenizer.enter(Name::BlockQuotePrefix);
            tokenizer.enter(Name::BlockQuoteMarker);
            tokenizer.consume();
            tokenizer.exit(Name::BlockQuoteMarker);
            State::Next(StateName::BlockQuoteContAfter)
        }
        _ => State::Nok,
    }
}

// After marker - consume optional space
pub fn cont_after(tokenizer: &mut Tokenizer) -> State {
    if let Some(b'\t' | b' ') = tokenizer.current {
        tokenizer.enter(Name::SpaceOrTab);
        tokenizer.consume();
        tokenizer.exit(Name::SpaceOrTab);
    }
    tokenizer.exit(Name::BlockQuotePrefix);
    State::Ok
}
```

### Event Sequence Example

Input: `> hello`

```erlang
[
    {event, enter, document, #point{}, undefined, document},
    {event, enter, block_quote, #point{line=1, column=1, offset=0}, undefined, undefined},
    {event, enter, block_quote_prefix, #point{}, undefined, undefined},
    {event, enter, block_quote_marker, #point{}, undefined, undefined},
    {event, exit, block_quote_marker, #point{line=1, column=2, offset=1}, undefined, undefined},
    {event, enter, space_or_tab, #point{}, undefined, undefined},
    {event, exit, space_or_tab, #point{line=1, column=3, offset=2}, undefined, undefined},
    {event, exit, block_quote_prefix, #point{}, undefined, undefined},
    {event, enter, paragraph, #point{}, undefined, flow},
    {event, enter, data, #point{}, undefined, undefined},
    {event, exit, data, #point{line=1, column=8, offset=7}, undefined, undefined},
    {event, exit, paragraph, #point{}, undefined, undefined},
    {event, exit, block_quote, #point{}, undefined, undefined},
    {event, exit, document, #point{}, undefined, undefined}
]
```

### Test Requirements

```erlang
-module(erlmd_cnstr_block_quote_test).
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%% Test 1: Simple block quote
simple_block_quote_test() ->
    Input = <<"> hello">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should have block_quote enter/exit events
    BlockQuoteEvents = [E || E <- Events, 
                         element(3, E) =:= block_quote],
    ?assertEqual(2, length(BlockQuoteEvents)).

%% Test 2: Block quote with continuation
continuation_test() ->
    Input = <<"> line 1\n> line 2">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should be a single block quote
    EnterEvents = [E || E <- Events, 
                   element(2, E) =:= enter,
                   element(3, E) =:= block_quote],
    ?assertEqual(1, length(EnterEvents)).

%% Test 3: Lazy continuation
lazy_continuation_test() ->
    Input = <<"> line 1\nline 2">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should continue the block quote (lazy)
    EnterEvents = [E || E <- Events,
                   element(2, E) =:= enter,
                   element(3, E) =:= block_quote],
    ?assertEqual(1, length(EnterEvents)).

%% Test 4: Nested block quotes
nested_test() ->
    Input = <<"> > nested">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should have 2 block quote enter events
    EnterEvents = [E || E <- Events,
                   element(2, E) =:= enter,
                   element(3, E) =:= block_quote],
    ?assertEqual(2, length(EnterEvents)).

%% Test 5: Block quote with paragraph
with_paragraph_test() ->
    Input = <<"> hello world">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should have both block_quote and paragraph
    ?assert(lists:any(fun(E) -> element(3, E) =:= block_quote end, Events)),
    ?assert(lists:any(fun(E) -> element(3, E) =:= paragraph end, Events)).

%% CommonMark Example 206
commonmark_206_test() ->
    Input = <<"> # Foo\n> bar\n> baz">>,
    Expected = <<"<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 207
commonmark_207_test() ->
    Input = <<"># Foo\n>bar\n> baz">>,
    Expected = <<"<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 208
commonmark_208_test() ->
    Input = <<"   > # Foo\n   > bar\n > baz">>,
    Expected = <<"<blockquote>\n<h1>Foo</h1>\n<p>bar\nbaz</p>\n</blockquote>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

### Common Pitfalls

1. **Forgetting to exit block_quote_prefix**: Must exit the prefix token before returning
2. **Not handling optional space**: Space after `>` is optional, not required
3. **Breaking tail recursion**: Continuation checks must be tail-recursive
4. **Indentation limits**: Must respect 0-3 space limit before marker
5. **Lazy continuation**: Don't require `>` on every line

### Edge Cases

- Empty block quote: `>`
- Block quote with only spaces: `>    `
- Multiple nested quotes: `> > > text`
- Indented block quote: `   > text` (up to 3 spaces)
- Block quote interrupted by other constructs
- Lazy continuation spanning multiple lines

---

## Module 8.2: List Item Construct

### Context

- **Module**: `src/erlmd_cnstr_list_item.erl`
- **Priority**: CRITICAL
- **Complexity**: VERY HIGH
- **Estimated Time**: 8-10 days

### CommonMark Specification

From Section 5.2:

> A list marker is a bullet list marker or an ordered list marker.
> 
> A bullet list marker is a -, +, or * character.
> 
> An ordered list marker is a sequence of 1–9 arabic digits (0-9), followed by either a . character or a ) character.

Key properties:
- **Unordered markers**: `*`, `-`, `+`
- **Ordered markers**: `1.` through `999999999.` or `1)` through `999999999)`
- **Indentation**: Marker can be preceded by 0-3 spaces
- **Content indent**: Content after marker determines list item indent
- **Blank initial**: List item can start with blank line
- **Lazy continuation**: Subsequent lines can be lazy
- **Loose vs tight**: Blank lines between items determine list style

### Implementation Requirements

1. **Marker Detection**:
   - Unordered: Check for thematic break before accepting `*` or `-`
   - Ordered: Parse 1-9 digits followed by `.` or `)`
   - Validate interruption rules

2. **Prefix Parsing**:
   - Track prefix size for continuation
   - Handle blank initial case
   - Compute content indentation

3. **Continuation Logic**:
   - Blank continuation: Allow up to `size` spaces
   - Filled continuation: Require exactly `size` spaces
   - Track `blank_initial` state

4. **Resolution**:
   - Group adjacent list items with same marker
   - Inject `ListOrdered` or `ListUnordered` wrapper events
   - Handle nested lists

### Container State

List items use the document container stack to track:

```erlang
%% In tokenizer state:
-record(container, {
    kind :: atom(),          % list_item
    size :: non_neg_integer(), % Indent size for continuation
    blank_initial :: boolean() % Whether item starts with blank
}).
```

### Key Functions

```erlang
-module(erlmd_cnstr_list_item).
-export([start/1, cont_start/1, resolve/1]).

-include("types.hrl").

%% Public API
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
-spec cont_start(tokenizer()) -> {state_result(), tokenizer()}.
-spec resolve(tokenizer()) -> ok.

%% Internal states
-spec before(tokenizer()) -> {state_result(), tokenizer()}.
-spec before_unordered(tokenizer()) -> {state_result(), tokenizer()}.
-spec before_ordered(tokenizer()) -> {state_result(), tokenizer()}.
-spec value(tokenizer()) -> {state_result(), tokenizer()}.
-spec marker(tokenizer()) -> {state_result(), tokenizer()}.
-spec marker_after(tokenizer()) -> {state_result(), tokenizer()}.
-spec marker_after_filled(tokenizer()) -> {state_result(), tokenizer()}.
-spec whitespace(tokenizer()) -> {state_result(), tokenizer()}.
-spec whitespace_after(tokenizer()) -> {state_result(), tokenizer()}.
-spec prefix_other(tokenizer()) -> {state_result(), tokenizer()}.
-spec after(tokenizer()) -> {state_result(), tokenizer()}.
-spec cont_blank(tokenizer()) -> {state_result(), tokenizer()}.
-spec cont_filled(tokenizer()) -> {state_result(), tokenizer()}.
```

### Implementation Pattern - Part 1: Entry and Marker Detection

```erlang
-module(erlmd_cnstr_list_item).
-export([start/1, cont_start/1, resolve/1]).

-include("types.hrl").

%% Constants
-define(LIST_ITEM_VALUE_SIZE_MAX, 10). % 9 digits + terminator

%% @doc Start of list item.
%%
%% ```markdown
%% > | * a
%%     ^
%% ```
start(T) ->
    T1 = erlmd_tokenizer:enter(T, list_item),
    
    %% Optional indentation (0-3 spaces)
    case erlmd_tokenizer:current(T1) of
        C when C =:= $\t; C =:= $\s ->
            erlmd_tokenizer:attempt(
                T1,
                {next, before},
                nok,
                fun(T2) ->
                    erlmd_cnstr_prtl_space_or_tab:start(T2, #{
                        min => 0,
                        max => 3
                    })
                end
            );
        _ ->
            before(T1)
    end.

%% @doc After optional whitespace, at list item prefix.
%%
%% ```markdown
%% > | * a
%%     ^
%% ```
before(T) ->
    case erlmd_tokenizer:current(T) of
        %% Unordered markers - but check for thematic break first
        C when C =:= $*; C =:= $- ->
            %% These could be thematic breaks, so we need to check
            erlmd_tokenizer:check(
                T,
                nok,
                {next, before_unordered},
                fun(T2) ->
                    erlmd_cnstr_thematic_break:start(T2)
                end
            );
        $+ ->
            %% + cannot be thematic break
            before_unordered(T);
        $1 ->
            %% First digit of ordered list (always allowed)
            before_ordered(T);
        C when C >= $0, C =< $9 ->
            %% Other digits only allowed if not interrupting
            case erlmd_tokenizer:is_interrupt(T) of
                true -> {nok, T};
                false -> before_ordered(T)
            end;
        _ ->
            {nok, T}
    end.

%% @doc At unordered list item marker.
%% The line is not a thematic break.
%%
%% ```markdown
%% > | * a
%%     ^
%% ```
before_unordered(T) ->
    T1 = erlmd_tokenizer:enter(T, list_item_prefix),
    marker(T1).

%% @doc At ordered list item value.
%%
%% ```markdown
%% > | 1. a
%%     ^
%% ```
before_ordered(T) ->
    T1 = erlmd_tokenizer:enter(T, list_item_prefix),
    T2 = erlmd_tokenizer:enter(T1, list_item_value),
    value(T2, 0).

%% @doc In ordered list item value.
%% Accumulates size in tokenize_state.
%%
%% ```markdown
%% > | 1. a
%%     ^
%% ```
value(T, Size) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $.; C =:= $) ->
            %% End of value
            %% Check interruption rules: if interrupting and size < 2, reject
            IsInterrupt = erlmd_tokenizer:is_interrupt(T),
            if
                IsInterrupt andalso Size < 1 ->
                    {nok, T};
                true ->
                    T1 = erlmd_tokenizer:exit(T),
                    marker(T1)
            end;
        C when C >= $0, C =< $9 ->
            %% Another digit
            if
                Size + 1 < ?LIST_ITEM_VALUE_SIZE_MAX ->
                    T1 = erlmd_tokenizer:consume(T),
                    value(T1, Size + 1);
                true ->
                    %% Too many digits
                    {nok, T}
            end;
        _ ->
            {nok, T}
    end.

%% @doc At list item marker.
%%
%% ```markdown
%% > | * a
%%     ^
%% > | 1. b
%%      ^
%% ```
marker(T) ->
    T1 = erlmd_tokenizer:enter(T, list_item_marker),
    T2 = erlmd_tokenizer:consume(T1),
    T3 = erlmd_tokenizer:exit(T2),
    marker_after(T3).
```

### Implementation Pattern - Part 2: Whitespace and Prefix

```erlang
%% @doc After list item marker.
%% Check if followed by blank line.
%%
%% ```markdown
%% > | * a
%%      ^
%% > | 1. b
%%       ^
%% ```
marker_after(T) ->
    %% Check for blank line
    erlmd_tokenizer:check(
        T,
        {next, after_blank},
        {next, marker_after_filled},
        fun(T2) ->
            erlmd_cnstr_blank_line:start(T2)
        end
    ).

%% @doc After marker, followed by blank line.
after_blank(T) ->
    %% Set blank_initial flag
    after(T, true).

%% @doc After list item marker.
%% The marker is not followed by a blank line.
%%
%% ```markdown
%% > | * a
%%      ^
%% ```
marker_after_filled(T) ->
    %% Try to parse 1-4 spaces/tabs
    erlmd_tokenizer:attempt(
        T,
        {next, fun(T2) -> after(T2, false) end},
        {next, prefix_other},
        fun(T2) ->
            whitespace(T2)
        end
    ).

%% @doc Parse whitespace after marker (1-4 spaces/tabs).
%%
%% ```markdown
%% > | * a
%%      ^
%% ```
whitespace(T) ->
    erlmd_tokenizer:attempt(
        T,
        {next, whitespace_after},
        nok,
        fun(T2) ->
            erlmd_cnstr_prtl_space_or_tab:start(T2, #{
                min => 1,
                max => 4  % TAB_SIZE
            })
        end
    ).

%% @doc After acceptable whitespace.
%% Ensure no more whitespace follows.
%%
%% ```markdown
%% > | * a
%%      ^
%% ```
whitespace_after(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $\t; C =:= $\s ->
            %% Too much whitespace
            {nok, T};
        _ ->
            {ok, T}
    end.

%% @doc After marker, followed by no indent or more indent than needed.
%% Consume exactly one space/tab.
%%
%% ```markdown
%% > | * a
%%      ^
%% ```
prefix_other(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $\t; C =:= $\s ->
            T1 = erlmd_tokenizer:enter(T, space_or_tab),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            after(T3, false);
        _ ->
            {nok, T}
    end.

%% @doc After list item prefix.
%% Calculate prefix size and store in container.
%%
%% ```markdown
%% > | * a
%%       ^
%% ```
after(T, IsBlank) ->
    %% If blank and interrupting, reject
    IsInterrupt = erlmd_tokenizer:is_interrupt(T),
    if
        IsBlank andalso IsInterrupt ->
            {nok, T};
        true ->
            %% Calculate prefix size
            Events = erlmd_tokenizer:events(T),
            StartIdx = find_list_item_start(Events),
            StartPoint = get_event_point(Events, StartIdx),
            CurrentPoint = erlmd_tokenizer:point(T),
            
            PrefixSize = calculate_prefix_size(StartPoint, CurrentPoint),
            
            %% Adjust for blank initial
            FinalSize = if
                IsBlank -> PrefixSize + 1;
                true -> PrefixSize
            end,
            
            %% Store in container
            T1 = erlmd_tokenizer:set_container(T, #{
                kind => list_item,
                size => FinalSize,
                blank_initial => IsBlank
            }),
            
            T2 = erlmd_tokenizer:exit(T1), % Exit list_item_prefix
            
            %% Register resolver
            T3 = erlmd_tokenizer:register_resolver(T2, list_item, 
                                                    fun resolve/1),
            
            {ok, T3}
    end.
```

### Implementation Pattern - Part 3: Continuation

```erlang
%% @doc Start of list item continuation.
%%
%% ```markdown
%%   | * a
%% > |   b
%%     ^
%% ```
cont_start(T) ->
    %% Check if next line is blank
    erlmd_tokenizer:check(
        T,
        {next, cont_blank},
        {next, cont_filled},
        fun(T2) ->
            erlmd_cnstr_blank_line:start(T2)
        end
    ).

%% @doc Start of blank list item continuation.
%%
%% ```markdown
%%   | * a
%% > |
%%     ^
%%   |   b
%% ```
cont_blank(T) ->
    Container = erlmd_tokenizer:get_container(T),
    Size = maps:get(size, Container, 0),
    BlankInitial = maps:get(blank_initial, Container, false),
    
    if
        BlankInitial ->
            %% Can't have blank after blank initial
            {nok, T};
        true ->
            %% Consume up to size spaces/tabs
            case erlmd_tokenizer:current(T) of
                C when C =:= $\t; C =:= $\s ->
                    erlmd_cnstr_prtl_space_or_tab:start(T, #{
                        min => 0,
                        max => Size
                    });
                _ ->
                    {ok, T}
            end
    end.

%% @doc Start of non-blank list item continuation.
%%
%% ```markdown
%%   | * a
%% > |   b
%%     ^
%% ```
cont_filled(T) ->
    Container = erlmd_tokenizer:get_container(T),
    Size = maps:get(size, Container, 0),
    
    %% Clear blank_initial flag
    T1 = erlmd_tokenizer:set_container(T, #{
        kind => list_item,
        size => Size,
        blank_initial => false
    }),
    
    %% Consume exactly size spaces/tabs
    case erlmd_tokenizer:current(T1) of
        C when C =:= $\t; C =:= $\s ->
            erlmd_cnstr_prtl_space_or_tab:start(T1, #{
                min => Size,
                max => Size
            });
        _ ->
            {nok, T1}
    end.
```

### Implementation Pattern - Part 4: Resolver

```erlang
%% @doc Find adjacent list items with the same marker.
%% Group them into ListOrdered or ListUnordered containers.
resolve(T) ->
    Events = erlmd_tokenizer:events(T),
    
    %% Find all list items and group by marker
    Groups = group_list_items(Events),
    
    %% Inject list container events
    inject_list_events(T, Groups).

%% Helper: Group adjacent list items by marker
group_list_items(Events) ->
    group_list_items(Events, 0, 0, [], []).

group_list_items([], _Idx, _Balance, _Stack, Acc) ->
    lists:reverse(Acc);
group_list_items([Event | Rest], Idx, Balance, Stack, Acc) ->
    case Event of
        #event{kind = enter, name = list_item} ->
            %% Find matching end
            EndIdx = find_matching_end(Rest, Idx + 1, 1),
            
            %% Get marker
            MarkerIdx = find_marker(Rest, 0),
            Marker = get_marker_byte(Rest, MarkerIdx),
            
            %% Try to match with existing group
            NewStack = try_add_to_group(Stack, {Marker, Balance, Idx, EndIdx}),
            
            group_list_items(Rest, Idx + 1, Balance + 1, NewStack, Acc);
        
        #event{kind = exit, name = list_item} ->
            group_list_items(Rest, Idx + 1, Balance - 1, Stack, Acc);
        
        _ ->
            group_list_items(Rest, Idx + 1, Balance, Stack, Acc)
    end.

%% Helper: Inject ListOrdered or ListUnordered wrapper events
inject_list_events(T, Groups) ->
    lists:foldl(fun(Group, TAcc) ->
        {Marker, _Balance, StartIdx, EndIdx} = Group,
        
        ListName = case Marker of
            M when M =:= $.; M =:= $) -> list_ordered;
            _ -> list_unordered
        end,
        
        %% Insert enter event before first item
        T1 = erlmd_tokenizer:insert_event(TAcc, StartIdx, 
            #event{kind = enter, name = ListName}),
        
        %% Insert exit event after last item
        T2 = erlmd_tokenizer:insert_event(T1, EndIdx + 1,
            #event{kind = exit, name = ListName}),
        
        T2
    end, T, Groups).
```

### Rust Reference Patterns

Key patterns from `list_item.rs`:

1. **Marker Detection with Thematic Break Check**:
```rust
pub fn before(tokenizer: &mut Tokenizer) -> State {
    // Unordered.
    if matches!(tokenizer.current, Some(b'*' | b'-')) {
        tokenizer.check(State::Nok, State::Next(StateName::ListItemBeforeUnordered));
        State::Retry(StateName::ThematicBreakStart)
    } else if tokenizer.current == Some(b'+') {
        State::Retry(StateName::ListItemBeforeUnordered)
    }
    // ...
}
```

2. **Value Parsing with Size Limit**:
```rust
pub fn value(tokenizer: &mut Tokenizer) -> State {
    if matches!(tokenizer.current, Some(b'.' | b')'))
        && (!tokenizer.interrupt || tokenizer.tokenize_state.size < 2)
    {
        tokenizer.exit(Name::ListItemValue);
        State::Retry(StateName::ListItemMarker)
    } else if matches!(tokenizer.current, Some(b'0'..=b'9'))
        && tokenizer.tokenize_state.size + 1 < LIST_ITEM_VALUE_SIZE_MAX
    {
        tokenizer.tokenize_state.size += 1;
        tokenizer.consume();
        State::Next(StateName::ListItemValue)
    } else {
        tokenizer.tokenize_state.size = 0;
        State::Nok
    }
}
```

3. **Container State Management**:
```rust
pub fn after(tokenizer: &mut Tokenizer) -> State {
    let blank = tokenizer.tokenize_state.size == 1;
    tokenizer.tokenize_state.size = 0;

    if blank && tokenizer.interrupt {
        State::Nok
    } else {
        // Calculate prefix size
        let container = &mut tokenizer.tokenize_state.document_container_stack
            [tokenizer.tokenize_state.document_continued];

        container.blank_initial = blank;
        container.size = prefix;

        tokenizer.exit(Name::ListItemPrefix);
        tokenizer.register_resolver_before(ResolveName::ListItem);
        State::Ok
    }
}
```

### Event Sequence Example

Input: `* item`

```erlang
[
    {event, enter, list_unordered, #point{}, undefined, undefined},  % Injected by resolver
    {event, enter, list_item, #point{line=1, column=1, offset=0}, undefined, undefined},
    {event, enter, list_item_prefix, #point{}, undefined, undefined},
    {event, enter, list_item_marker, #point{}, undefined, undefined},
    {event, exit, list_item_marker, #point{line=1, column=2, offset=1}, undefined, undefined},
    {event, enter, space_or_tab, #point{}, undefined, undefined},
    {event, exit, space_or_tab, #point{line=1, column=3, offset=2}, undefined, undefined},
    {event, exit, list_item_prefix, #point{}, undefined, undefined},
    {event, enter, paragraph, #point{}, undefined, flow},
    {event, enter, data, #point{}, undefined, undefined},
    {event, exit, data, #point{line=1, column=7, offset=6}, undefined, undefined},
    {event, exit, paragraph, #point{}, undefined, undefined},
    {event, exit, list_item, #point{}, undefined, undefined},
    {event, exit, list_unordered, #point{}, undefined, undefined}  % Injected by resolver
]
```

### Test Requirements

```erlang
-module(erlmd_cnstr_list_item_test).
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%% Test 1: Simple unordered list
simple_unordered_test() ->
    Input = <<"* item">>,
    Events = test_helpers:parse_with_flow(Input),
    ?assert(lists:any(fun(E) -> element(3, E) =:= list_item end, Events)),
    ?assert(lists:any(fun(E) -> element(3, E) =:= list_unordered end, Events)).

%% Test 2: Simple ordered list
simple_ordered_test() ->
    Input = <<"1. item">>,
    Events = test_helpers:parse_with_flow(Input),
    ?assert(lists:any(fun(E) -> element(3, E) =:= list_item end, Events)),
    ?assert(lists:any(fun(E) -> element(3, E) =:= list_ordered end, Events)).

%% Test 3: Multiple items grouped
multiple_items_test() ->
    Input = <<"* item 1\n* item 2">>,
    Events = test_helpers:parse_with_flow(Input),
    ListItemEvents = [E || E <- Events, element(3, E) =:= list_item],
    %% 2 items = 2 enter + 2 exit
    ?assertEqual(4, length(ListItemEvents)),
    %% Should have single list_unordered wrapper
    ListEvents = [E || E <- Events, element(3, E) =:= list_unordered],
    ?assertEqual(2, length(ListEvents)). % 1 enter + 1 exit

%% Test 4: Not a thematic break
not_thematic_break_test() ->
    Input = <<"* item\nnot break">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should be list, not thematic break
    ?assert(lists:any(fun(E) -> element(3, E) =:= list_item end, Events)),
    ?assertNot(lists:any(fun(E) -> element(3, E) =:= thematic_break end, Events)).

%% Test 5: Indented content
indented_content_test() ->
    Input = <<"* item\n  continued">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should be single list item
    EnterEvents = [E || E <- Events,
                   element(2, E) =:= enter,
                   element(3, E) =:= list_item],
    ?assertEqual(1, length(EnterEvents)).

%% Test 6: Different markers don't group
different_markers_test() ->
    Input = <<"* item 1\n+ item 2">>,
    Events = test_helpers:parse_with_flow(Input),
    %% Should have 2 separate lists
    ListEvents = [E || E <- Events, element(3, E) =:= list_unordered],
    ?assertEqual(4, length(ListEvents)). % 2 lists × 2 events

%% Test 7: Ordered list value parsing
ordered_value_test() ->
    Input = <<"123. item">>,
    Events = test_helpers:parse_with_flow(Input),
    ValueEvents = [E || E <- Events, element(3, E) =:= list_item_value],
    ?assertEqual(2, length(ValueEvents)). % enter + exit

%% Test 8: Value too long
value_too_long_test() ->
    Input = <<"1234567890. item">>,
    Result = erlmd:to_html(Input),
    %% Should NOT be parsed as list (too many digits)
    ?assertNot(string:str(binary_to_list(Result), "<li>") > 0).

%% Test 9: Blank initial
blank_initial_test() ->
    Input = <<"*\n  item">>,
    Events = test_helpers:parse_with_flow(Input),
    ?assert(lists:any(fun(E) -> element(3, E) =:= list_item end, Events)).

%% CommonMark Example 264
commonmark_264_test() ->
    Input = <<"- foo\n- bar\n- baz">>,
    Expected = <<"<ul>\n<li>foo</li>\n<li>bar</li>\n<li>baz</li>\n</ul>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 271
commonmark_271_test() ->
    Input = <<"1. foo\n2. bar\n3. baz">>,
    Expected = <<"<ol>\n<li>foo</li>\n<li>bar</li>\n<li>baz</li>\n</ol>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 272  
commonmark_272_test() ->
    Input = <<"1. foo\n\n2. bar">>,
    %% Loose list (blank line between items)
    Expected = <<"<ol>\n<li>\n<p>foo</p>\n</li>\n<li>\n<p>bar</p>\n</li>\n</ol>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

### Common Pitfalls

1. **Thematic Break Confusion**: `*` and `-` can be either list markers or thematic breaks - must check
2. **Interruption Rules**: Ordered lists starting with 0-9 (not 1) can't interrupt paragraphs
3. **Blank Initial**: List items can start blank, but continuation rules differ
4. **Prefix Size**: Must calculate exact byte offset, including marker and whitespace
5. **Container State**: Must properly maintain container stack across continuations
6. **Resolver Timing**: Resolver must run AFTER all parsing complete
7. **Nested Lists**: Must handle lists within lists correctly
8. **Loose vs Tight**: Blank lines affect HTML output (paragraphs vs not)

### Edge Cases

- Empty list item: `*`
- List item with blank initial: `*\n  content`
- Indented list item: `   * item` (0-3 spaces allowed)
- List marker that's actually thematic break: `***`
- Ordered list with max digits: `999999999. item`
- List with lazy continuation
- Nested lists with different markers
- List interrupted by other constructs

---

## Integration Requirements

### Flow Dispatcher Updates

Both constructs must be added to the flow dispatcher:

```erlang
%% In erlmd_cnstr_flow.erl

-define(FLOW_CONSTRUCTS, [
    blank_line,
    thematic_break,
    code_fenced,
    code_indented,
    html_flow,
    heading_atx,
    heading_setext,
    block_quote,      % ADD THIS
    list_item,        % ADD THIS
    definition,
    paragraph         % MUST BE LAST
]).
```

**Order matters**: List items should come after block quotes but before paragraphs.

### Tokenizer Updates

Add container support:

```erlang
%% In erlmd_tokenizer.erl

%% Container management
-spec get_container(tokenizer()) -> map().
get_container(T) ->
    %% Get current container from stack
    ...

-spec set_container(tokenizer(), map()) -> tokenizer().
set_container(T, Container) ->
    %% Update current container
    ...

%% Resolver registration
-spec register_resolver(tokenizer(), atom(), function()) -> tokenizer().
register_resolver(T, Name, Fun) ->
    %% Add resolver to be called during post-processing
    ...
```

### State Dispatcher Updates

```erlang
%% In erlmd_state.erl

call(block_quote, T) ->
    erlmd_cnstr_block_quote:start(T);
call(block_quote_cont_start, T) ->
    erlmd_cnstr_block_quote:cont_start(T);

call(list_item, T) ->
    erlmd_cnstr_list_item:start(T);
call(list_item_cont_start, T) ->
    erlmd_cnstr_list_item:cont_start(T);
```

---

## Testing Strategy

### Unit Tests

For each module:
1. Basic functionality (simple cases)
2. Edge cases (empty, blank, etc.)
3. Continuation logic
4. Nesting scenarios
5. Integration with other constructs

### CommonMark Tests

Target test ranges:
- **Block quotes**: Examples 206-238 (~33 tests)
- **List items**: Examples 264-307 (~44 tests)
- **Lists (tight/loose)**: Examples 264-280 (~17 tests)

### Integration Tests

Test interactions:
1. Block quotes containing lists
2. Lists containing block quotes
3. Multiple levels of nesting
4. Lists in lists
5. Quotes in quotes in lists

### Performance Tests

```erlang
%% Deeply nested structures
deeply_nested_test() ->
    %% > > > > * * * * content
    Nesting = 8,
    Input = build_nested_structure(Nesting),
    {Time, _Result} = timer:tc(fun() ->
        erlmd:to_html(Input)
    end),
    %% Should complete in reasonable time
    ?assert(Time < 100000). % 100ms
```

### Test Helpers

```erlang
-module(test_helpers).
-export([parse_with_flow/1, feed_loop/2]).

%% Parse input with full flow dispatcher
parse_with_flow(Input) ->
    T0 = erlmd_tokenizer:new(Input, #{}),
    T1 = erlmd_tokenizer:enter(T0, document),
    {ok, T2} = erlmd_state:call(flow, T1),
    T3 = erlmd_tokenizer:exit(T2),
    erlmd_tokenizer:events(T3).

%% Simulate feed loop for construct testing
feed_loop(T, ConstructFun) ->
    case ConstructFun(T) of
        {ok, T1} -> {ok, T1};
        {nok, T1} -> {nok, T1};
        {{next, NextFun}, T1} when is_function(NextFun) ->
            feed_loop(T1, NextFun);
        {{next, NextState}, T1} when is_atom(NextState) ->
            feed_loop(T1, fun(T2) -> erlmd_state:call(NextState, T2) end);
        {{retry, RetryFun}, T1} when is_function(RetryFun) ->
            feed_loop(T1, RetryFun);
        {{retry, RetryState}, T1} when is_atom(RetryState) ->
            feed_loop(T1, fun(T2) -> erlmd_state:call(RetryState, T2) end)
    end.
```

---

## Common Pitfalls

### For Both Constructs

1. **Not Using Tail Recursion**: All state functions must be tail-recursive
2. **Binary Match Context**: Don't pattern match in function bodies
3. **Event Pairing**: Every `enter` must have matching `exit`
4. **State Result Types**: Return proper `{state_result(), tokenizer()}` tuples
5. **Attempt Mechanism**: Use `attempt` for backtracking, not manual state save/restore
6. **Container Stack**: Must properly push/pop container state
7. **Resolver Registration**: Resolvers must be registered before returning

### Block Quote Specific

1. **Lazy Continuation**: Don't require `>` on every line
2. **Optional Space**: Space after `>` is optional
3. **Indentation Limit**: Max 3 spaces before `>`
4. **Prefix Exit**: Must exit `block_quote_prefix` before continuing

### List Item Specific

1. **Thematic Break Check**: Must check `*` and `-` against thematic break
2. **Interruption Rules**: Ordered lists with 0-9 can't interrupt
3. **Value Size Limit**: Max 9 digits for ordered lists
4. **Blank Initial**: Special continuation rules for blank-starting items
5. **Prefix Calculation**: Must compute exact byte offset for continuation
6. **Marker Matching**: Only group items with identical markers
7. **Resolver Injection**: Must inject wrapper events in correct positions

---

## Acceptance Criteria

### Module 8.1: Block Quote

- [ ] Can parse simple block quote: `> text`
- [ ] Handles optional space after `>`: `>text` and `> text`
- [ ] Supports continuation: `> line1\n> line2`
- [ ] Supports lazy continuation: `> line1\nline2`
- [ ] Handles indentation: `   > text` (up to 3 spaces)
- [ ] Supports nesting: `> > nested`
- [ ] Contains flow content correctly
- [ ] CommonMark examples 206-238 pass
- [ ] No infinite loops on any input
- [ ] Proper event nesting
- [ ] No binary optimization warnings
- [ ] All unit tests pass

### Module 8.2: List Item

- [ ] Can parse unordered markers: `*`, `-`, `+`
- [ ] Can parse ordered markers: `1.`, `1)`, `999999999.`
- [ ] Rejects thematic breaks: `***` is not a list
- [ ] Handles interruption rules correctly
- [ ] Computes prefix size correctly
- [ ] Supports blank initial: `*\n  content`
- [ ] Handles blank continuation
- [ ] Handles filled continuation
- [ ] Groups adjacent items by marker
- [ ] Resolver injects list wrapper events
- [ ] Supports nested lists
- [ ] Distinguishes loose vs tight lists
- [ ] CommonMark examples 264-307 pass
- [ ] No infinite loops on any input
- [ ] Proper event nesting
- [ ] No binary optimization warnings
- [ ] All unit tests pass

### Integration

- [ ] Both constructs integrate with flow dispatcher
- [ ] Block quotes can contain lists
- [ ] Lists can contain block quotes
- [ ] Deep nesting works (8+ levels)
- [ ] Performance acceptable (<100ms for deeply nested)
- [ ] HTML output matches CommonMark spec
- [ ] All integration tests pass

### Overall Phase 8

- [ ] 100+ additional CommonMark tests passing
- [ ] Total CommonMark pass rate >60%
- [ ] No regressions in previous phases
- [ ] Code coverage >85%
- [ ] Documentation complete
- [ ] Ready for Phase 9 (processing pipeline)

---

## Implementation Checklist

### Week 1: Block Quotes (3-4 days)

- [ ] Day 1: Implement `erlmd_cnstr_block_quote.erl`
  - [ ] `start/1` function
  - [ ] `cont_start/1` function
  - [ ] `cont_before/1` function
  - [ ] `cont_after/1` function
  - [ ] Basic unit tests
- [ ] Day 2: Integration and testing
  - [ ] Add to flow dispatcher
  - [ ] Integration tests
  - [ ] CommonMark tests 206-220
- [ ] Day 3: Edge cases and nesting
  - [ ] Lazy continuation
  - [ ] Nested quotes
  - [ ] CommonMark tests 221-238
- [ ] Day 4: Polish and documentation
  - [ ] Code review
  - [ ] Documentation
  - [ ] Performance check

### Week 2: List Items - Basic (4 days)

- [ ] Day 1: Marker detection
  - [ ] Unordered markers
  - [ ] Thematic break check
  - [ ] Ordered value parsing
  - [ ] Basic unit tests
- [ ] Day 2: Prefix and whitespace
  - [ ] `marker_after/1` logic
  - [ ] Whitespace parsing
  - [ ] Prefix size calculation
  - [ ] Container state setup
- [ ] Day 3: Basic continuation
  - [ ] `cont_start/1` function
  - [ ] Blank continuation
  - [ ] Filled continuation
  - [ ] Unit tests
- [ ] Day 4: Integration
  - [ ] Add to flow dispatcher
  - [ ] Basic integration tests
  - [ ] CommonMark tests 264-280

### Week 3: List Items - Advanced (4-5 days)

- [ ] Day 1: Resolver implementation
  - [ ] Event grouping logic
  - [ ] Marker matching
  - [ ] List wrapper injection
- [ ] Day 2: Nesting support
  - [ ] Nested list tests
  - [ ] Container stack management
  - [ ] Complex nesting scenarios
- [ ] Day 3: Loose/tight detection
  - [ ] Blank line tracking
  - [ ] HTML output adjustment
  - [ ] CommonMark tests 281-295
- [ ] Day 4: Edge cases
  - [ ] Blank initial
  - [ ] Lazy continuation
  - [ ] Interruption rules
  - [ ] CommonMark tests 296-307
- [ ] Day 5: Polish and integration
  - [ ] Full integration tests
  - [ ] Performance optimization
  - [ ] Documentation
  - [ ] Final review

---

## Debugging Tips

### Event Inspection

```erlang
%% Add to your test
debug_events(Events) ->
    io:format("~nEvent Sequence:~n"),
    lists:foreach(fun(E) ->
        io:format("  ~p ~p ~p~n", [
            element(2, E),  % kind
            element(3, E),  % name
            element(4, E)   % point
        ])
    end, Events).
```

### State Tracking

```erlang
%% Add debug output in state functions
io:format("~s:~p Current='~c' Offset=~p~n", [
    ?MODULE,
    ?LINE,
    erlmd_tokenizer:current(T),
    erlmd_tokenizer:offset(T)
]).
```

### Container Inspection

```erlang
%% Check container state
Container = erlmd_tokenizer:get_container(T),
io:format("Container: ~p~n", [Container]).
```

### Common Issues

1. **Infinite Loop**: Add EOF checks in all loops
2. **Wrong Events**: Use `debug_events/1` to inspect
3. **Position Errors**: Check point calculations
4. **Container Errors**: Inspect container stack
5. **Resolver Errors**: Check event injection logic

---

## Next Steps After Phase 8

Once Phase 8 is complete:

1. **Review**: Code review, performance check
2. **Documentation**: Update architecture docs
3. **Testing**: Full regression test suite
4. **Phase 9**: Begin processing pipeline (subtokenization, resolvers)

Phase 9 will build on Phase 8 by implementing:
- Subtokenization for inline content
- Resolver framework for post-processing
- Event stream transformations

---

## Summary

Phase 8 implements the most complex block-level constructs in the CommonMark specification. Success requires:

1. **Careful attention** to continuation rules
2. **Proper container management** for nesting
3. **Thorough testing** of edge cases
4. **Performance awareness** for deep nesting
5. **Integration testing** with existing constructs

The Rust reference implementation provides a solid foundation, but Erlang-specific patterns (tail recursion, binary optimization) must be followed.

**Estimated total time**: 2.5 weeks with focused effort.

Good luck! 🚀
