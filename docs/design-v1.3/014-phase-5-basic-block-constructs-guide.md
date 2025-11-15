# Phase 5: Basic Block Constructs - Implementation Guide

**Project**: erlmd - Erlang Markdown Parser  
**Phase**: 5 of 12  
**Duration**: 1.5 weeks  
**Complexity**: Medium  
**Date**: November 14, 2025

---

## Table of Contents

1. [Phase Overview](#phase-overview)
2. [Prerequisites](#prerequisites)
3. [Module 5.1: Paragraph Construct](#module-51-paragraph-construct)
4. [Module 5.2: ATX Heading Construct](#module-52-atx-heading-construct)
5. [Module 5.3: Thematic Break Construct](#module-53-thematic-break-construct)
6. [Module 5.4: Indented Code Construct](#module-54-indented-code-construct)
7. [Module 5.5: Flow Dispatcher Updates](#module-55-flow-dispatcher-updates)
8. [Integration Testing](#integration-testing)
9. [CommonMark Compliance Testing](#commonmark-compliance-testing)
10. [Performance Validation](#performance-validation)
11. [Acceptance Criteria](#acceptance-criteria)

---

## Phase Overview

### Goals

Implement the four fundamental block-level markdown constructs that form the foundation of document structure:

1. **Paragraphs** - Default block container for inline content
2. **ATX Headings** - Hash-style headings (# through ######)
3. **Thematic Breaks** - Horizontal rules (---, ***, ___)
4. **Indented Code Blocks** - Code blocks created by indentation

### Why These Four?

These constructs represent different complexity levels and patterns:

- **Paragraph**: Simplest - delegates to text content, lowest priority
- **Thematic Break**: Simple pattern matching with markers
- **ATX Heading**: Moderate - requires counting and text content handling
- **Indented Code**: Complex - requires precise indentation tracking and continuation logic

### Architecture Context

All four constructs:
- Are parsed in the **flow** content type
- Must be registered with the flow dispatcher
- Follow the standard construct pattern (enter → parse → exit)
- Generate event streams (not direct AST)
- Must handle EOF and line endings correctly

### Dependencies

**Required Modules (from Phases 1-4)**:
- `erlmd_tokenizer.erl` - State machine driver
- `erlmd_state.erl` - State dispatcher
- `erlmd_cnstr_document.erl` - Top-level dispatcher
- `erlmd_cnstr_flow.erl` - Flow content dispatcher
- `erlmd_cnstr_text.erl` - Inline content dispatcher
- `erlmd_cnstr_blank_line.erl` - Blank line detection
- `erlmd_cnstr_prtl_data.erl` - Data content handling
- `erlmd_cnstr_prtl_space_or_tab.erl` - Whitespace handling
- `erlmd_util_char.erl` - Character classification
- `include/types.hrl` - Core type definitions
- `include/consts.hrl` - Constants

### Reference Materials

**Rust Implementation**:
- `markdown-rs/src/construct/paragraph.rs`
- `markdown-rs/src/construct/heading_atx.rs`
- `markdown-rs/src/construct/thematic_break.rs`
- `markdown-rs/src/construct/code_indented.rs`
- `markdown-rs/src/construct/flow.rs`

**CommonMark Specification**:
- §4.1 Thematic breaks
- §4.2 ATX headings
- §4.4 Indented code blocks
- §4.8 Paragraphs

**Erlang Implementation Patterns**:
- Section 3: State Machine Implementation
- Section 5: Performance Optimization Patterns
- Section 11: Common Pitfalls and Anti-Patterns

---

## Prerequisites

### Verify Phase 1-4 Completion

Before starting Phase 5, ensure all previous phases are complete:

```bash
# Test that foundation is solid
rebar3 eunit

# Verify these modules exist and compile:
ls src/erlmd_tokenizer.erl
ls src/erlmd_state.erl
ls src/erlmd_cnstr_flow.erl
ls src/erlmd_cnstr_text.erl
ls src/erlmd_cnstr_blank_line.erl
ls src/erlmd_cnstr_prtl_data.erl
ls src/erlmd_cnstr_prtl_space_or_tab.erl
```

### Key Concepts to Understand

1. **Event-Driven Architecture**: Constructs generate Enter/Exit event pairs
2. **Attempt Mechanism**: Backtracking for ambiguous syntax
3. **Content Types**: Flow → Text → String dispatching
4. **Match Context Preservation**: Binary optimization patterns
5. **Tail Recursion**: All state functions must be tail-recursive

### Constants Required

Ensure `include/consts.hrl` defines:

```erlang
-define(TAB_SIZE, 4).
-define(CODE_INDENT_SIZE, 4).
-define(HEADING_ATX_OPENING_FENCE_SIZE_MAX, 6).
-define(THEMATIC_BREAK_MARKER_COUNT_MIN, 3).
```

---

## Module 5.1: Paragraph Construct

### Context

- **Module**: `src/erlmd_cnstr_paragraph.erl`
- **Priority**: CRITICAL (most common construct)
- **Complexity**: Low-Medium
- **Rust Reference**: `markdown-rs/src/construct/paragraph.rs`
- **CommonMark Spec**: §4.8 Paragraphs

### CommonMark Specification Summary

> A sequence of non-blank lines that cannot be interpreted as other kinds of blocks forms a paragraph. The contents of the paragraph are the result of parsing the paragraph's raw content as inlines.

**Key Properties**:
- **Lowest Priority**: Paragraph is the "default" - tried last in flow dispatcher
- **Inline Content**: Contains text that must be parsed by text dispatcher
- **Termination**: Ends at blank line, EOF, or when interrupted by another block
- **Line Continuation**: Multiple lines are part of same paragraph if not separated by blank line

### Implementation Requirements

1. Must be called by flow dispatcher (lowest priority)
2. Delegates inline content parsing to text dispatcher
3. Handles line endings and continuation
4. Properly detects paragraph termination
5. Maintains proper event nesting (Enter → Exit)
6. Links data events for continuous text content

### Module Structure

```erlang
-module(erlmd_cnstr_paragraph).
-export([start/1]).

-include("types.hrl").

%% @doc Entry point for paragraph construct.
%% Called by flow dispatcher when no other block construct matches.
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(Tokenizer) ->
    %% Always start a paragraph - it's the default construct
    T1 = erlmd_tokenizer:enter(Tokenizer, paragraph),
    line_start(T1, true).

%% @doc Start of a line in a paragraph.
%% @private
-spec line_start(tokenizer(), boolean()) -> {state_result(), tokenizer()}.
line_start(Tokenizer, IsFirst) ->
    case erlmd_tokenizer:current(Tokenizer) of
        eof when IsFirst ->
            %% Empty paragraph at EOF - shouldn't happen but handle it
            T1 = erlmd_tokenizer:exit(Tokenizer),
            {nok, T1};
        eof ->
            %% End of paragraph at EOF
            T1 = erlmd_tokenizer:exit(Tokenizer),
            {ok, T1};
        _ ->
            %% Enter data with text content type for inline parsing
            T1 = erlmd_tokenizer:enter_link(Tokenizer, data, 
                #{content => text}),
            
            %% Link this data event to previous if not first line
            T2 = if
                IsFirst -> T1;
                true -> link_previous_data(T1)
            end,
            
            parse_inside(T2)
    end.

%% @doc Parse content inside paragraph.
%% @private
-spec parse_inside(tokenizer()) -> {state_result(), tokenizer()}.
parse_inside(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        eof ->
            %% End of input - close data and paragraph
            T1 = erlmd_tokenizer:exit(Tokenizer),  % Exit data
            T2 = erlmd_tokenizer:exit(T1),          % Exit paragraph
            {ok, T2};
            
        $\n ->
            %% Line ending - consume and check for continuation
            T1 = erlmd_tokenizer:consume(Tokenizer),
            T2 = erlmd_tokenizer:exit(T1),  % Exit data
            check_continuation(T2);
            
        _Byte ->
            %% Regular character - consume and continue
            T1 = erlmd_tokenizer:consume(Tokenizer),
            parse_inside(T1)
    end.

%% @doc Check if paragraph continues on next line.
%% @private
-spec check_continuation(tokenizer()) -> {state_result(), tokenizer()}.
check_continuation(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        eof ->
            %% EOF after line ending - end paragraph
            T1 = erlmd_tokenizer:exit(Tokenizer),
            {ok, T1};
            
        $\n ->
            %% Another line ending = blank line - end paragraph
            T1 = erlmd_tokenizer:exit(Tokenizer),
            {ok, T1};
            
        _Byte ->
            %% More content - continue paragraph on next line
            line_start(Tokenizer, false)
    end.

%% @doc Link current data event to previous data event.
%% This creates continuous text content across line breaks.
%% @private
-spec link_previous_data(tokenizer()) -> tokenizer().
link_previous_data(Tokenizer) ->
    Events = erlmd_tokenizer:events(Tokenizer),
    %% Find the previous data exit event and link to it
    %% Implementation depends on event linking mechanism in tokenizer
    erlmd_tokenizer:link_previous(Tokenizer, data).
```

### Critical Implementation Notes

#### 1. Why Paragraph is Last Priority

The flow dispatcher must try all other block constructs before paragraph:

```erlang
%% In flow dispatcher - THIS ORDER MATTERS
attempt_constructs([
    blank_line,          % Try blank line first
    thematic_break,      % Try thematic break
    heading_atx,         % Try ATX heading
    code_indented,       % Try indented code
    %% ... other constructs ...
    paragraph            % LAST - paragraph is default
])
```

#### 2. Event Linking Pattern

Paragraph uses event linking to connect data across line breaks:

```
Line 1: Hello
Line 2: World

Events:
  Enter paragraph
    Enter data (link: none)
    Exit data
    Enter data (link: previous data)  ← Linked!
    Exit data
  Exit paragraph
```

This allows the HTML generator to produce: `<p>Hello\nWorld</p>`

#### 3. Tail Recursion Requirements

All functions must be tail-recursive:

```erlang
%% ✅ CORRECT - Tail recursive
parse_inside(T) ->
    T1 = consume(T),
    parse_inside(T1).  % Last call

%% ❌ WRONG - Not tail recursive
parse_inside(T) ->
    T1 = parse_inside(T),
    exit(T1).  % Work after recursive call
```

### Pattern to Follow

```erlang
%% Optimal Pattern: Match in function head, tail call
parse(<<$\n, Rest/binary>>, State) ->
    handle_newline(Rest, State);
parse(<<Char, Rest/binary>>, State) ->
    handle_char(Char, Rest, State);
parse(<<>>, State) ->
    finalize(State).
```

### Pattern to Avoid

```erlang
%% Suboptimal: Matching in function body creates sub-binaries
parse(Binary, State) ->
    case Binary of
        <<$\n, Rest/binary>> ->  % Creates sub-binary!
            handle_newline(Rest, State)
    end.
```

### Test Requirements

```erlang
-module(erlmd_cnstr_paragraph_test).
-include_lib("eunit/include/eunit.hrl").

%% Test 1: Simple single-line paragraph
simple_paragraph_test() ->
    Input = <<"Hello world">>,
    Events = test_helpers:parse(Input),
    ?assertMatch([
        #event{kind = enter, name = document},
        #event{kind = enter, name = paragraph},
        #event{kind = enter, name = data},
        #event{kind = exit, name = data},
        #event{kind = exit, name = paragraph},
        #event{kind = exit, name = document}
    ], Events).

%% Test 2: Multi-line paragraph (no blank line)
multiline_paragraph_test() ->
    Input = <<"Line 1\nLine 2">>,
    Events = test_helpers:parse(Input),
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParagraphEvents)),  % 1 enter + 1 exit
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assertEqual(4, length(DataEvents)).  % 2 enter + 2 exit (two lines)

%% Test 3: Two paragraphs separated by blank line
two_paragraphs_test() ->
    Input = <<"Para 1\n\nPara 2">>,
    Events = test_helpers:parse(Input),
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(4, length(ParagraphEvents)),  % 2 enter + 2 exit
    Enters = [E || E <- ParagraphEvents, E#event.kind =:= enter],
    ?assertEqual(2, length(Enters)).

%% Test 4: Paragraph at EOF (no trailing newline)
paragraph_at_eof_test() ->
    Input = <<"Single line">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= paragraph end, Events)).

%% Test 5: Empty input (no paragraph)
empty_input_test() ->
    Input = <<>>,
    Events = test_helpers:parse(Input),
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(0, length(ParagraphEvents)).

%% CommonMark Example 189: Two simple paragraphs
commonmark_189_test() ->
    Input = <<"aaa\n\nbbb">>,
    Expected = <<"<p>aaa</p>\n<p>bbb</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 190: Multi-line paragraphs
commonmark_190_test() ->
    Input = <<"aaa\nbbb\n\nccc\nddd">>,
    Expected = <<"<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 191: Multiple blank lines
commonmark_191_test() ->
    Input = <<"aaa\n\n\nbbb">>,
    Expected = <<"<p>aaa</p>\n<p>bbb</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 192: Leading spaces
commonmark_192_test() ->
    Input = <<"  aaa\n bbb">>,
    Expected = <<"<p>aaa\nbbb</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

### Integration Points

1. **Flow Dispatcher**: Must call paragraph as last resort
2. **Text Dispatcher**: Paragraph delegates inline parsing to text
3. **Event Linking**: Must link data events across line breaks
4. **HTML Generation**: Later phase will convert to `<p>` tags

### Edge Cases to Handle

1. **Empty paragraph**: Should not occur but handle gracefully
2. **Paragraph at EOF**: No trailing newline
3. **Multiple blank lines**: Only one needed to end paragraph
4. **Leading/trailing whitespace**: Preserved in content
5. **Very long paragraphs**: Must not stack overflow

### Acceptance Criteria

- [ ] Can parse simple single-line paragraphs
- [ ] Multi-line paragraphs work correctly
- [ ] Paragraphs separated by blank lines parse as separate paragraphs
- [ ] Paragraph ends correctly at EOF
- [ ] CommonMark examples 189-196 pass
- [ ] No infinite loops on any input
- [ ] Proper event nesting (enter/exit paired)
- [ ] Data events properly linked across lines
- [ ] No compiler warnings (especially binary optimization)
- [ ] All unit tests pass
- [ ] Integrates correctly with flow dispatcher

---

## Module 5.2: ATX Heading Construct

### Context

- **Module**: `src/erlmd_cnstr_heading_atx.erl`
- **Priority**: HIGH (common and visible)
- **Complexity**: Medium
- **Rust Reference**: `markdown-rs/src/construct/heading_atx.rs`
- **CommonMark Spec**: §4.2 ATX headings

### CommonMark Specification Summary

> An ATX heading consists of a string of characters, parsed as inline content, between an opening sequence of 1-6 unescaped # characters and an optional closing sequence of any number of unescaped # characters. The opening sequence must be followed by a space or by the end of line.

**Key Properties**:
- **1-6 Hash Marks**: `#` through `######` (level 1-6)
- **Space Required**: Must have space after opening `#` sequence (unless empty heading)
- **Optional Closing**: Closing `#` sequence is optional
- **Inline Content**: Heading text parsed as text content type
- **Whitespace Handling**: Leading whitespace allowed (up to 3 spaces if code blocks enabled)

### Implementation Requirements

1. Count opening `#` sequence (1-6 marks)
2. Verify space or EOL after opening sequence
3. Parse heading text as inline content (text content type)
4. Handle optional closing `#` sequence
5. Handle optional leading whitespace
6. Register resolver for post-processing (to wrap text in HeadingAtxText)

### Module Structure

```erlang
-module(erlmd_cnstr_heading_atx).
-export([start/1, resolve/1]).

-include("types.hrl").
-include("consts.hrl").

%% @doc Entry point for ATX heading construct.
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(Tokenizer) ->
    T1 = erlmd_tokenizer:enter(Tokenizer, heading_atx),
    
    %% Handle optional leading whitespace (up to 3 spaces if code_indented enabled)
    case erlmd_tokenizer:current(T1) of
        C when C =:= $\t; C =:= $\s ->
            MaxIndent = case is_code_indented_enabled(T1) of
                true -> ?TAB_SIZE - 1;  % 3 spaces max
                false -> unlimited
            end,
            erlmd_tokenizer:attempt(T1,
                {next, fun before/1},
                {nok, fun() -> erlmd_tokenizer:exit(T1) end},
                fun() -> space_or_tab_min_max(T1, 0, MaxIndent) end);
        _ ->
            before(T1)
    end.

%% @doc After optional whitespace, expecting '#'.
%% @private
-spec before(tokenizer()) -> {state_result(), tokenizer()}.
before(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        $# ->
            T1 = erlmd_tokenizer:enter(Tokenizer, heading_atx_sequence),
            sequence_open(T1, 1);
        _ ->
            T1 = erlmd_tokenizer:exit(Tokenizer),
            {nok, T1}
    end.

%% @doc Count opening '#' sequence.
%% @private
-spec sequence_open(tokenizer(), non_neg_integer()) -> {state_result(), tokenizer()}.
sequence_open(Tokenizer, Count) when Count < ?HEADING_ATX_OPENING_FENCE_SIZE_MAX ->
    T1 = erlmd_tokenizer:consume(Tokenizer),
    case erlmd_tokenizer:current(T1) of
        $# ->
            sequence_open(T1, Count + 1);
        C when C =:= eof; C =:= $\t; C =:= $\n; C =:= $\s ->
            T2 = erlmd_tokenizer:exit(T1),  % Exit heading_atx_sequence
            at_break(T2, Count);
        _ ->
            %% Not a valid heading (no space after #)
            T2 = erlmd_tokenizer:exit(T1),
            T3 = erlmd_tokenizer:exit(T2),
            {nok, T3}
    end;
sequence_open(Tokenizer, Count) ->
    %% At max count (6)
    T1 = erlmd_tokenizer:consume(Tokenizer),
    case erlmd_tokenizer:current(T1) of
        C when C =:= eof; C =:= $\t; C =:= $\n; C =:= $\s ->
            T2 = erlmd_tokenizer:exit(T1),  % Exit heading_atx_sequence
            at_break(T2, Count);
        _ ->
            %% More than 6 '#' marks - not a heading
            T2 = erlmd_tokenizer:exit(T1),
            T3 = erlmd_tokenizer:exit(T2),
            {nok, T3}
    end.

%% @doc At a break point (after sequence, whitespace, or content).
%% @private
-spec at_break(tokenizer(), non_neg_integer()) -> {state_result(), tokenizer()}.
at_break(Tokenizer, Level) ->
    case erlmd_tokenizer:current(Tokenizer) of
        eof ->
            %% Empty heading at EOF
            T1 = erlmd_tokenizer:exit(Tokenizer),
            erlmd_tokenizer:register_resolver(T1, heading_atx),
            erlmd_tokenizer:set_interrupt(T1, false),
            {ok, T1};
            
        $\n ->
            %% Empty heading or end of heading
            T1 = erlmd_tokenizer:exit(Tokenizer),
            erlmd_tokenizer:register_resolver(T1, heading_atx),
            erlmd_tokenizer:set_interrupt(T1, false),
            {ok, T1};
            
        C when C =:= $\t; C =:= $\s ->
            %% Whitespace - try to skip it
            erlmd_tokenizer:attempt(Tokenizer,
                {next, fun(T) -> at_break(T, Level) end},
                {nok, fun() -> erlmd_tokenizer:exit(Tokenizer) end},
                fun() -> space_or_tab(Tokenizer) end);
                
        $# ->
            %% Possible closing sequence
            T1 = erlmd_tokenizer:enter(Tokenizer, heading_atx_sequence),
            sequence_further(T1, Level);
            
        _ ->
            %% Text content
            T1 = erlmd_tokenizer:enter_link(Tokenizer, data,
                #{content => text}),
            parse_data(T1, Level)
    end.

%% @doc Parse further '#' sequence (could be closing or just text).
%% @private
-spec sequence_further(tokenizer(), non_neg_integer()) -> {state_result(), tokenizer()}.
sequence_further(Tokenizer, Level) ->
    case erlmd_tokenizer:current(Tokenizer) of
        $# ->
            T1 = erlmd_tokenizer:consume(Tokenizer),
            sequence_further(T1, Level);
        _ ->
            T1 = erlmd_tokenizer:exit(Tokenizer),  % Exit heading_atx_sequence
            at_break(T1, Level)
    end.

%% @doc Parse text data content.
%% @private
-spec parse_data(tokenizer(), non_neg_integer()) -> {state_result(), tokenizer()}.
parse_data(Tokenizer, Level) ->
    case erlmd_tokenizer:current(Tokenizer) of
        C when C =:= eof; C =:= $\t; C =:= $\n; C =:= $\s ->
            T1 = erlmd_tokenizer:exit(Tokenizer),  % Exit data
            at_break(T1, Level);
        _ ->
            T1 = erlmd_tokenizer:consume(Tokenizer),
            parse_data(T1, Level)
    end.

%% @doc Resolve heading by wrapping text in HeadingAtxText token.
%% Called during post-processing phase.
-spec resolve(tokenizer()) -> tokenizer().
resolve(Tokenizer) ->
    Events = erlmd_tokenizer:events(Tokenizer),
    %% Find all HeadingAtx enter/exit pairs
    %% For each pair, wrap all Data events in HeadingAtxText
    NewEvents = process_heading_events(Events, []),
    erlmd_tokenizer:set_events(Tokenizer, NewEvents).

%% @private
process_heading_events([], Acc) ->
    lists:reverse(Acc);
process_heading_events([Event | Rest], Acc) ->
    case Event#event.name of
        heading_atx when Event#event.kind =:= enter ->
            %% Process this heading
            {HeadingEvents, Remaining} = collect_heading(Rest, [Event]),
            Wrapped = wrap_text_in_heading_events(HeadingEvents),
            process_heading_events(Remaining, lists:reverse(Wrapped) ++ Acc);
        _ ->
            process_heading_events(Rest, [Event | Acc])
    end.

%% @private
collect_heading([], Acc) ->
    {lists:reverse(Acc), []};
collect_heading([Event | Rest], Acc) ->
    NewAcc = [Event | Acc],
    case Event#event.name of
        heading_atx when Event#event.kind =:= exit ->
            {lists:reverse(NewAcc), Rest};
        _ ->
            collect_heading(Rest, NewAcc)
    end.

%% @private
wrap_text_in_heading_events(Events) ->
    %% Find first and last data events
    %% Insert HeadingAtxText enter before first, exit after last
    %% Implementation details depend on event structure
    %% This is a simplified version - actual implementation more complex
    Events.  % TODO: Implement proper wrapping
```

### Critical Implementation Notes

#### 1. Heading Level Storage

The heading level (1-6) should be stored in tokenizer state:

```erlang
%% Store level for later use in HTML generation
Tokenizer#tokenizer{
    tokenize_state = TokenizeState#{heading_level => Count}
}
```

#### 2. Closing Sequence Handling

Closing `#` marks are optional and must be preceded by whitespace:

```
# Heading #      ← Valid closing
# Heading#       ← Not closing, part of text
# Heading ##     ← Valid closing (any number of #)
```

#### 3. Resolver Pattern

The resolver wraps all text content in a `HeadingAtxText` token:

```
Before resolve:
  Enter HeadingAtx
    Enter HeadingAtxSequence (#)
    Exit HeadingAtxSequence
    Enter Data (text)
    Exit Data
  Exit HeadingAtx

After resolve:
  Enter HeadingAtx
    Enter HeadingAtxSequence (#)
    Exit HeadingAtxSequence
    Enter HeadingAtxText      ← Added by resolver
      Enter Data (text)
      Exit Data
    Exit HeadingAtxText       ← Added by resolver
  Exit HeadingAtx
```

### Test Requirements

```erlang
-module(erlmd_cnstr_heading_atx_test).
-include_lib("eunit/include/eunit.hrl").

%% Test 1: Simple h1 heading
simple_h1_test() ->
    Input = <<"# Heading">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= heading_atx end, Events)).

%% Test 2: All heading levels
all_levels_test() ->
    lists:foreach(fun(Level) ->
        Hashes = binary:copy(<<"#">>, Level),
        Input = <<Hashes/binary, " Heading", Level/integer>>,
        Events = test_helpers:parse(Input),
        ?assert(lists:any(fun(E) -> E#event.name =:= heading_atx end, Events))
    end, lists:seq(1, 6)).

%% Test 3: Heading with closing sequence
with_closing_test() ->
    Input = <<"# Heading #">>,
    Expected = <<"<h1>Heading</h1>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Test 4: Seven hashes (not a heading)
seven_hashes_test() ->
    Input = <<"####### Not a heading">>,
    Events = test_helpers:parse(Input),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(0, length(HeadingEvents)).

%% Test 5: No space after # (not a heading)
no_space_test() ->
    Input = <<"#No heading">>,
    Events = test_helpers:parse(Input),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(0, length(HeadingEvents)).

%% CommonMark Example 32: Simple heading
commonmark_32_test() ->
    Input = <<"# foo">>,
    Expected = <<"<h1>foo</h1>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 33: Multiple #
commonmark_33_test() ->
    Input = <<"## foo">>,
    Expected = <<"<h2>foo</h2>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 34: All levels
commonmark_34_test() ->
    Input = <<"### foo\n#### foo\n##### foo\n###### foo">>,
    ?assertMatch(<<"<h3>foo</h3>\n<h4>foo</h4>\n<h5>foo</h5>\n<h6>foo</h6>\n">>, 
                 erlmd:to_html(Input)).
```

### Acceptance Criteria

- [ ] Can parse headings level 1-6
- [ ] Requires space after opening sequence
- [ ] Rejects 7+ hash marks
- [ ] Handles optional closing sequence correctly
- [ ] Handles leading whitespace (up to 3 spaces)
- [ ] Empty headings work correctly
- [ ] Resolver wraps text in HeadingAtxText
- [ ] CommonMark examples 32-49 pass
- [ ] No infinite loops
- [ ] Proper event nesting
- [ ] No compiler warnings

---

## Module 5.3: Thematic Break Construct

### Context

- **Module**: `src/erlmd_cnstr_thematic_break.erl`
- **Priority**: MEDIUM
- **Complexity**: Low-Medium
- **Rust Reference**: `markdown-rs/src/construct/thematic_break.rs`
- **CommonMark Spec**: §4.1 Thematic breaks

### CommonMark Specification Summary

> A line consisting of 0-3 spaces of indentation, followed by a sequence of three or more matching -, _, or * characters, each followed optionally by any number of spaces or tabs, forms a thematic break.

**Key Properties**:
- **Three Markers**: At least 3 of the same character (`*`, `-`, or `_`)
- **Consistent Marker**: All markers must be the same character
- **Whitespace Allowed**: Spaces/tabs can appear between markers
- **Leading Whitespace**: Up to 3 spaces of indentation allowed
- **No Text**: Cannot have any other text on the line

### Implementation Requirements

1. Check for valid marker character (`*`, `-`, `_`)
2. Count markers (must be >= 3)
3. Allow spaces/tabs between markers
4. Ensure all markers are the same character
5. Verify line ends with EOL or EOF (no other text)

### Module Structure

```erlang
-module(erlmd_cnstr_thematic_break).
-export([start/1]).

-include("types.hrl").
-include("consts.hrl").

%% @doc Entry point for thematic break construct.
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(Tokenizer) ->
    T1 = erlmd_tokenizer:enter(Tokenizer, thematic_break),
    
    %% Handle optional leading whitespace (up to 3 spaces)
    case erlmd_tokenizer:current(T1) of
        C when C =:= $\t; C =:= $\s ->
            MaxIndent = case is_code_indented_enabled(T1) of
                true -> ?TAB_SIZE - 1;
                false -> unlimited
            end,
            erlmd_tokenizer:attempt(T1,
                {next, fun before/1},
                {nok, fun() -> erlmd_tokenizer:exit(T1) end},
                fun() -> space_or_tab_min_max(T1, 0, MaxIndent) end);
        _ ->
            before(T1)
    end.

%% @doc After optional whitespace, expecting marker.
%% @private
-spec before(tokenizer()) -> {state_result(), tokenizer()}.
before(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        C when C =:= $*; C =:= $-; C =:= $_ ->
            %% Store the marker type
            T1 = erlmd_tokenizer:set_marker(Tokenizer, C),
            at_break(T1, 0);
        _ ->
            T1 = erlmd_tokenizer:exit(Tokenizer),
            {nok, T1}
    end.

%% @doc At a break point - could be marker or whitespace.
%% @private
-spec at_break(tokenizer(), non_neg_integer()) -> {state_result(), tokenizer()}.
at_break(Tokenizer, Count) ->
    Marker = erlmd_tokenizer:get_marker(Tokenizer),
    case erlmd_tokenizer:current(Tokenizer) of
        M when M =:= Marker ->
            %% Found a marker
            T1 = erlmd_tokenizer:enter(Tokenizer, thematic_break_sequence),
            sequence(T1, Count);
            
        C when C =:= eof; C =:= $\n ->
            %% End of line - check if we have enough markers
            if
                Count >= ?THEMATIC_BREAK_MARKER_COUNT_MIN ->
                    T1 = erlmd_tokenizer:exit(Tokenizer),
                    erlmd_tokenizer:set_interrupt(T1, false),
                    {ok, T1};
                true ->
                    T1 = erlmd_tokenizer:exit(Tokenizer),
                    {nok, T1}
            end;
            
        C when C =:= $\t; C =:= $\s ->
            %% Whitespace between markers
            erlmd_tokenizer:attempt(Tokenizer,
                {next, fun(T) -> at_break(T, Count) end},
                {nok, fun() -> erlmd_tokenizer:exit(Tokenizer) end},
                fun() -> space_or_tab(Tokenizer) end);
                
        _ ->
            %% Other character - not a thematic break
            T1 = erlmd_tokenizer:exit(Tokenizer),
            {nok, T1}
    end.

%% @doc Inside marker sequence.
%% @private
-spec sequence(tokenizer(), non_neg_integer()) -> {state_result(), tokenizer()}.
sequence(Tokenizer, Count) ->
    Marker = erlmd_tokenizer:get_marker(Tokenizer),
    case erlmd_tokenizer:current(Tokenizer) of
        M when M =:= Marker ->
            T1 = erlmd_tokenizer:consume(Tokenizer),
            sequence(T1, Count + 1);
            
        C when C =:= $\t; C =:= $\s ->
            %% Whitespace - exit sequence and try to continue
            T1 = erlmd_tokenizer:exit(Tokenizer),
            erlmd_tokenizer:attempt(T1,
                {next, fun(T) -> at_break(T, Count) end},
                {nok, fun() -> erlmd_tokenizer:exit(T1) end},
                fun() -> space_or_tab(T1) end);
            
        _ ->
            %% End of sequence
            T1 = erlmd_tokenizer:exit(Tokenizer),
            at_break(T1, Count)
    end.
```

### Critical Implementation Notes

#### 1. Marker Consistency

All markers on the line must be the same character:

```
***       ← Valid (all *)
* * *     ← Valid (all *, with spaces)
*-*       ← Invalid (mixed)
```

#### 2. Minimum Count

Must have at least 3 markers:

```
***       ← Valid (3 markers)
****      ← Valid (4 markers)
**        ← Invalid (only 2)
```

#### 3. Whitespace Handling

Whitespace can appear:
- Before the first marker (up to 3 spaces)
- Between markers
- After the last marker

```
   * * *     ← Valid (3 spaces leading, spaces between)
***          ← Valid (no spaces)
* *          ← Invalid (only 2 markers)
```

### Test Requirements

```erlang
-module(erlmd_cnstr_thematic_break_test).
-include_lib("eunit/include/eunit.hrl").

%% Test 1: Three asterisks
three_asterisks_test() ->
    Input = <<"***">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= thematic_break end, Events)).

%% Test 2: Three hyphens
three_hyphens_test() ->
    Input = <<"---">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= thematic_break end, Events)).

%% Test 3: Three underscores
three_underscores_test() ->
    Input = <<"___">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= thematic_break end, Events)).

%% Test 4: With spaces between markers
with_spaces_test() ->
    Input = <<"* * *">>,
    Expected = <<"<hr />\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Test 5: Mixed markers (invalid)
mixed_markers_test() ->
    Input = <<"*-*">>,
    Events = test_helpers:parse(Input),
    BreakEvents = [E || E <- Events, E#event.name =:= thematic_break],
    ?assertEqual(0, length(BreakEvents)).

%% Test 6: Only two markers (invalid)
two_markers_test() ->
    Input = <<"**">>,
    Events = test_helpers:parse(Input),
    BreakEvents = [E || E <- Events, E#event.name =:= thematic_break],
    ?assertEqual(0, length(BreakEvents)).

%% CommonMark Example 13: Asterisks
commonmark_13_test() ->
    Input = <<"***">>,
    Expected = <<"<hr />\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 19: With spaces
commonmark_19_test() ->
    Input = <<" ***">>,
    Expected = <<"<hr />\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

### Acceptance Criteria

- [ ] Recognizes `***`, `---`, and `___`
- [ ] Requires at least 3 markers
- [ ] All markers must be the same character
- [ ] Handles spaces between markers
- [ ] Handles leading whitespace (up to 3 spaces)
- [ ] Rejects mixed markers
- [ ] Rejects fewer than 3 markers
- [ ] CommonMark examples 13-28 pass
- [ ] No infinite loops
- [ ] Proper event nesting
- [ ] No compiler warnings

---

## Module 5.4: Indented Code Construct

### Context

- **Module**: `src/erlmd_cnstr_code_indented.erl`
- **Priority**: MEDIUM
- **Complexity**: High
- **Rust Reference**: `markdown-rs/src/construct/code_indented.rs`
- **CommonMark Spec**: §4.4 Indented code blocks

### CommonMark Specification Summary

> An indented code block is composed of one or more indented chunks separated by blank lines. An indented chunk is a sequence of non-blank lines, each preceded by four or more spaces of indentation.

**Key Properties**:
- **Four Space Indent**: Each line must start with 4+ spaces (or 1 tab)
- **No Interruption**: Cannot interrupt paragraphs
- **Blank Lines**: Blank lines within code block are preserved
- **Continuation**: Continues until less than 4 spaces of indentation
- **Lazy Lines**: Does not support lazy continuation

### Implementation Requirements

1. Check for 4 spaces/1 tab indentation
2. Do not interrupt paragraphs
3. Handle blank lines within code block
4. Continue parsing until indentation drops below 4 spaces
5. Track lazy/pierce state correctly

### Module Structure

```erlang
-module(erlmd_cnstr_code_indented).
-export([start/1]).

-include("types.hrl").
-include("consts.hrl").

%% @doc Entry point for indented code construct.
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(Tokenizer) ->
    %% Do not interrupt paragraphs
    case erlmd_tokenizer:is_interrupt(Tokenizer) of
        true ->
            {nok, Tokenizer};
        false ->
            case erlmd_tokenizer:current(Tokenizer) of
                C when C =:= $\t; C =:= $\s ->
                    T1 = erlmd_tokenizer:enter(Tokenizer, code_indented),
                    erlmd_tokenizer:attempt(T1,
                        {next, fun at_break/1},
                        {nok, fun() -> erlmd_tokenizer:exit(T1) end},
                        fun() -> space_or_tab_min_max(T1, ?TAB_SIZE, ?TAB_SIZE) end);
                _ ->
                    {nok, Tokenizer}
            end
    end.

%% @doc At a break between lines or chunks.
%% @private
-spec at_break(tokenizer()) -> {state_result(), tokenizer()}.
at_break(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        eof ->
            after_code(Tokenizer);
            
        $\n ->
            %% Try to continue on next line
            erlmd_tokenizer:attempt(Tokenizer,
                {next, fun at_break/1},
                {next, fun after_code/1},
                fun() -> further_start(Tokenizer) end);
            
        _ ->
            %% Content line
            T1 = erlmd_tokenizer:enter(Tokenizer, code_flow_chunk),
            inside(T1)
    end.

%% @doc Inside code content.
%% @private
-spec inside(tokenizer()) -> {state_result(), tokenizer()}.
inside(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        C when C =:= eof; C =:= $\n ->
            T1 = erlmd_tokenizer:exit(Tokenizer),  % Exit code_flow_chunk
            at_break(T1);
        _ ->
            T1 = erlmd_tokenizer:consume(Tokenizer),
            inside(T1)
    end.

%% @doc After indented code block.
%% @private
-spec after_code(tokenizer()) -> {state_result(), tokenizer()}.
after_code(Tokenizer) ->
    T1 = erlmd_tokenizer:exit(Tokenizer),  % Exit code_indented
    erlmd_tokenizer:set_interrupt(T1, false),
    {ok, T1}.

%% @doc Try to parse further line (continuation or end).
%% @private
-spec further_start(tokenizer()) -> {state_result(), tokenizer()}.
further_start(Tokenizer) ->
    %% Don't continue if lazy or pierce
    case {erlmd_tokenizer:is_lazy(Tokenizer), 
          erlmd_tokenizer:is_pierce(Tokenizer)} of
        {true, _} -> {nok, Tokenizer};
        {_, true} -> {nok, Tokenizer};
        _ ->
            case erlmd_tokenizer:current(Tokenizer) of
                $\n ->
                    %% Blank line - consume and continue
                    T1 = erlmd_tokenizer:enter(Tokenizer, line_ending),
                    T2 = erlmd_tokenizer:consume(T1),
                    T3 = erlmd_tokenizer:exit(T2),
                    further_start(T3);
                _ ->
                    %% Try to match required indentation
                    erlmd_tokenizer:attempt(Tokenizer,
                        {ok, fun() -> Tokenizer end},
                        {next, fun further_begin/1},
                        fun() -> space_or_tab_min_max(Tokenizer, ?TAB_SIZE, ?TAB_SIZE) end)
            end
    end.

%% @doc At line that might not have enough indentation.
%% @private
-spec further_begin(tokenizer()) -> {state_result(), tokenizer()}.
further_begin(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        C when C =:= $\t; C =:= $\s ->
            %% Some whitespace - check if it's a blank line
            erlmd_tokenizer:attempt(Tokenizer,
                {next, fun further_after/1},
                {nok, fun() -> Tokenizer end},
                fun() -> space_or_tab(Tokenizer) end);
        _ ->
            {nok, Tokenizer}
    end.

%% @doc After whitespace on insufficiently indented line.
%% @private
-spec further_after(tokenizer()) -> {state_result(), tokenizer()}.
further_after(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        $\n ->
            %% Blank line - continue
            further_start(Tokenizer);
        _ ->
            %% Not blank and not enough indent - end code block
            {nok, Tokenizer}
    end.
```

### Critical Implementation Notes

#### 1. Paragraph Interruption

Indented code cannot interrupt paragraphs:

```markdown
Paragraph text
    Not code - continues paragraph

Paragraph text

    Code - blank line allows interruption
```

#### 2. Blank Line Handling

Blank lines within code block are preserved:

```markdown
    Code line 1
    
    Code line 2
```

Generates: `<pre><code>Code line 1\n\nCode line 2\n</code></pre>`

#### 3. Indentation Checking

Must have exactly 4 spaces or 1 tab:

```markdown
    Code       ← 4 spaces
	Code       ← 1 tab
   Not code    ← 3 spaces (not enough)
     Code      ← 5 spaces (first 4 consumed, rest is content)
```

### Test Requirements

```erlang
-module(erlmd_cnstr_code_indented_test).
-include_lib("eunit/include/eunit.hrl").

%% Test 1: Simple indented code
simple_code_test() ->
    Input = <<"    code">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= code_indented end, Events)).

%% Test 2: Multiple lines
multiple_lines_test() ->
    Input = <<"    line 1\n    line 2">>,
    Expected = <<"<pre><code>line 1\nline 2\n</code></pre>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Test 3: With blank lines
with_blank_lines_test() ->
    Input = <<"    line 1\n\n    line 2">>,
    Expected = <<"<pre><code>line 1\n\nline 2\n</code></pre>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Test 4: Cannot interrupt paragraph
cannot_interrupt_test() ->
    Input = <<"Paragraph\n    not code">>,
    Events = test_helpers:parse(Input),
    CodeEvents = [E || E <- Events, E#event.name =:= code_indented],
    ?assertEqual(0, length(CodeEvents)).

%% Test 5: Can follow blank line
after_blank_test() ->
    Input = <<"Paragraph\n\n    code">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= code_indented end, Events)).

%% CommonMark Example 77: Simple code
commonmark_77_test() ->
    Input = <<"    a simple\n      indented code block">>,
    Expected = <<"<pre><code>a simple\n  indented code block\n</code></pre>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 80: Blank lines in code
commonmark_80_test() ->
    Input = <<"    chunk1\n\n    chunk2\n  \n \n \n    chunk3">>,
    ?assertMatch(<<"<pre><code>", _/binary>>, erlmd:to_html(Input)).
```

### Acceptance Criteria

- [ ] Requires 4 spaces or 1 tab indentation
- [ ] Cannot interrupt paragraphs
- [ ] Preserves blank lines within code block
- [ ] Ends when indentation drops below 4 spaces
- [ ] Handles lazy/pierce correctly
- [ ] CommonMark examples 77-88 pass
- [ ] No infinite loops
- [ ] Proper event nesting
- [ ] No compiler warnings

---

## Module 5.5: Flow Dispatcher Updates

### Context

Now that we have four block constructs, we need to update the flow dispatcher to try each construct in the correct order.

### Priority Order

The flow dispatcher must try constructs in this specific order:

1. **Blank line** - Must be first (terminates many constructs)
2. **Code indented** - Before other content (specific indentation pattern)
3. **Thematic break** - Before headings (to avoid ambiguity)
4. **ATX heading** - Before setext (clearer pattern)
5. **Paragraph** - LAST (default/fallback)

### Updated Flow Dispatcher

```erlang
-module(erlmd_cnstr_flow).
-export([start/1]).

-include("types.hrl").

%% @doc Entry point for flow content dispatcher.
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        $# ->
            %% Might be ATX heading
            erlmd_tokenizer:attempt(Tokenizer,
                {next, fun after_construct/1},
                {next, fun before_content/1},
                fun() -> erlmd_cnstr_heading_atx:start(Tokenizer) end);
                
        C when C =:= $*; C =:= $_ ->
            %% Might be thematic break
            erlmd_tokenizer:attempt(Tokenizer,
                {next, fun after_construct/1},
                {next, fun before_content/1},
                fun() -> erlmd_cnstr_thematic_break:start(Tokenizer) end);
                
        _ ->
            %% Try other constructs in order
            blank_line_before(Tokenizer)
    end.

%% @doc Try blank line.
%% @private
blank_line_before(Tokenizer) ->
    erlmd_tokenizer:attempt(Tokenizer,
        {next, fun blank_line_after/1},
        {next, fun before_code_indented/1},
        fun() -> erlmd_cnstr_blank_line:start(Tokenizer) end).

%% @doc Try code indented.
%% @private
before_code_indented(Tokenizer) ->
    erlmd_tokenizer:attempt(Tokenizer,
        {next, fun after_construct/1},
        {next, fun before_thematic_break/1},
        fun() -> erlmd_cnstr_code_indented:start(Tokenizer) end).

%% @doc Try thematic break.
%% @private
before_thematic_break(Tokenizer) ->
    erlmd_tokenizer:attempt(Tokenizer,
        {next, fun after_construct/1},
        {next, fun before_heading_atx/1},
        fun() -> erlmd_cnstr_thematic_break:start(Tokenizer) end).

%% @doc Try ATX heading.
%% @private
before_heading_atx(Tokenizer) ->
    erlmd_tokenizer:attempt(Tokenizer,
        {next, fun after_construct/1},
        {next, fun before_content/1},
        fun() -> erlmd_cnstr_heading_atx:start(Tokenizer) end).

%% @doc Try paragraph (fallback).
%% @private
before_content(Tokenizer) ->
    erlmd_tokenizer:attempt(Tokenizer,
        {next, fun after_construct/1},
        {nok, fun() -> Tokenizer end},
        fun() -> erlmd_cnstr_paragraph:start(Tokenizer) end).

%% @doc After blank line.
%% @private
blank_line_after(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        eof ->
            {ok, Tokenizer};
        $\n ->
            T1 = erlmd_tokenizer:enter(Tokenizer, blank_line_ending),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            erlmd_tokenizer:set_interrupt(T3, false),
            start(T3);
        _ ->
            error(expected_eol_or_eof)
    end.

%% @doc After successful construct.
%% @private
after_construct(Tokenizer) ->
    case erlmd_tokenizer:current(Tokenizer) of
        eof ->
            {ok, Tokenizer};
        $\n ->
            T1 = erlmd_tokenizer:enter(Tokenizer, line_ending),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            start(T3);
        _ ->
            error(expected_eol_or_eof)
    end.
```

### State Dispatcher Updates

Update `erlmd_state.erl` to route to new constructs:

```erlang
%% Add to call/2 function:

call(paragraph, Tokenizer) ->
    erlmd_cnstr_paragraph:start(Tokenizer);
    
call(heading_atx, Tokenizer) ->
    erlmd_cnstr_heading_atx:start(Tokenizer);
    
call(thematic_break, Tokenizer) ->
    erlmd_cnstr_thematic_break:start(Tokenizer);
    
call(code_indented, Tokenizer) ->
    erlmd_cnstr_code_indented:start(Tokenizer);
```

---

## Integration Testing

### Test Strategy

1. **Unit Tests**: Each construct module has its own test suite
2. **Integration Tests**: Test constructs working together
3. **CommonMark Tests**: Run official spec tests
4. **Edge Case Tests**: Boundary conditions and corner cases

### Integration Test Suite

```erlang
-module(erlmd_phase5_integration_test).
-include_lib("eunit/include/eunit.hrl").

%% Test mixing different block constructs
mixed_blocks_test() ->
    Input = <<"# Heading\n\nParagraph text.\n\n---\n\n    code">>,
    HTML = erlmd:to_html(Input),
    ?assertMatch(<<"<h1>Heading</h1>", _/binary>>, HTML),
    ?assertMatch(<<_, "<p>Paragraph text.</p>", _/binary>>, HTML),
    ?assertMatch(<<_, "<hr />", _/binary>>, HTML),
    ?assertMatch(<<_, "<pre><code>code", _/binary>>, HTML).

%% Test paragraph doesn't break on single newline
paragraph_continuation_test() ->
    Input = <<"Line 1\nLine 2">>,
    Expected = <<"<p>Line 1\nLine 2</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Test thematic break vs paragraph
thematic_vs_paragraph_test() ->
    Input = <<"***\n\ntext">>,
    HTML = erlmd:to_html(Input),
    ?assertMatch(<<"<hr />", _/binary>>, HTML),
    ?assertMatch(<<_, "<p>text</p>", _/binary>>, HTML).

%% Test heading vs paragraph
heading_vs_paragraph_test() ->
    Input = <<"# Heading\ntext">>,
    HTML = erlmd:to_html(Input),
    ?assertMatch(<<"<h1>Heading</h1>", _/binary>>, HTML),
    ?assertMatch(<<_, "<p>text</p>", _/binary>>, HTML).

%% Test code cannot interrupt paragraph
code_interrupt_test() ->
    Input = <<"Para\n    not code">>,
    HTML = erlmd:to_html(Input),
    %% Should be all in one paragraph
    ?assertMatch(<<"<p>Para\n    not code</p>", _/binary>>, HTML),
    %% Should NOT have code block
    ?assertEqual(nomatch, binary:match(HTML, <<"<pre>">>)).
```

---

## CommonMark Compliance Testing

### Target Examples

Phase 5 should pass these CommonMark spec sections:

- **§4.1 Thematic breaks**: Examples 13-31 (19 examples)
- **§4.2 ATX headings**: Examples 32-49 (18 examples)
- **§4.4 Indented code**: Examples 77-88 (12 examples)
- **§4.8 Paragraphs**: Examples 189-196 (8 examples)

**Total**: 57 examples

### Running CommonMark Tests

```erlang
-module(erlmd_commonmark_phase5_test).
-include_lib("eunit/include/eunit.hrl").

run_commonmark_tests_test() ->
    {ok, Spec} = file:read_file("test/commonmark_spec/spec.json"),
    {ok, Tests} = jsone:decode(Spec),
    
    %% Filter for Phase 5 relevant tests
    Phase5Tests = lists:filter(fun(Test) ->
        Section = maps:get(<<"section">>, Test),
        lists:member(Section, [
            <<"Thematic breaks">>,
            <<"ATX headings">>,
            <<"Indented code blocks">>,
            <<"Paragraphs">>
        ])
    end, Tests),
    
    %% Run each test
    Results = lists:map(fun run_test/1, Phase5Tests),
    
    Passed = length([R || R <- Results, R =:= pass]),
    Total = length(Results),
    
    io:format("CommonMark Phase 5: ~p/~p passed (~.1f%)~n",
              [Passed, Total, (Passed/Total)*100]),
    
    ?assert(Passed >= 50).  % Expect at least 50/57 to pass initially

run_test(Test) ->
    Markdown = maps:get(<<"markdown">>, Test),
    Expected = maps:get(<<"html">>, Test),
    
    try
        Result = erlmd:to_html(Markdown),
        case Result of
            Expected -> pass;
            _ -> fail
        end
    catch
        _:_ -> error
    end.
```

---

## Performance Validation

### Performance Targets

- **Throughput**: >= 100 MB/s for typical markdown
- **Memory**: Linear growth with input size
- **No Stack Overflow**: Handle very long paragraphs
- **No Infinite Loops**: All constructs must terminate

### Benchmarking

```erlang
-module(erlmd_phase5_bench).

benchmark() ->
    %% Generate test documents
    Sizes = [1_000, 10_000, 100_000, 1_000_000],
    
    lists:foreach(fun(Size) ->
        Doc = generate_mixed_document(Size),
        
        {Time, _Result} = timer:tc(fun() ->
            erlmd:to_html(Doc)
        end),
        
        Throughput = Size / (Time / 1_000_000),  % bytes/sec
        MBps = Throughput / (1024 * 1024),
        
        io:format("~p bytes: ~.2f MB/s~n", [Size, MBps])
    end, Sizes).

generate_mixed_document(TargetSize) ->
    %% Generate document with mix of constructs
    Chunks = [
        <<"# Heading\n\n">>,
        <<"Paragraph text here.\n\n">>,
        <<"---\n\n">>,
        <<"    code block\n\n">>
    ],
    
    Repeated = binary:copy(iolist_to_binary(Chunks), 
                          TargetSize div byte_size(iolist_to_binary(Chunks)) + 1),
    
    binary:part(Repeated, 0, min(TargetSize, byte_size(Repeated))).
```

---

## Acceptance Criteria

### Module Completion

- [ ] `erlmd_cnstr_paragraph.erl` implemented and tested
- [ ] `erlmd_cnstr_heading_atx.erl` implemented and tested
- [ ] `erlmd_cnstr_thematic_break.erl` implemented and tested
- [ ] `erlmd_cnstr_code_indented.erl` implemented and tested
- [ ] `erlmd_cnstr_flow.erl` updated with correct dispatch order
- [ ] `erlmd_state.erl` routes to all new constructs

### Test Coverage

- [ ] All unit tests pass
- [ ] Integration tests pass
- [ ] At least 50/57 CommonMark examples pass (target: 57/57)
- [ ] No infinite loops on any test case
- [ ] No stack overflows on large inputs

### Code Quality

- [ ] No compiler warnings
- [ ] Binary optimization preserved (check with `bin_opt_info`)
- [ ] All functions are tail-recursive where required
- [ ] Proper event nesting in all cases
- [ ] Error handling for edge cases

### Performance

- [ ] Parsing throughput >= 100 MB/s
- [ ] Memory usage is linear with input size
- [ ] No memory leaks detected

### Documentation

- [ ] All public functions have `-spec` types
- [ ] Module documentation (edoc) is complete
- [ ] Inline comments explain complex logic
- [ ] Test cases document expected behavior

---

## Next Steps After Phase 5

Once Phase 5 is complete:

1. **Phase 6**: Implement basic inline constructs (escapes, entities, inline code)
2. **Continue Testing**: Keep CommonMark compliance score improving
3. **Performance Profiling**: Use `fprof` to identify any bottlenecks
4. **Documentation**: Keep updating as patterns emerge

---

## Summary

Phase 5 implements the four fundamental block constructs that form the backbone of markdown document structure. These constructs demonstrate different patterns:

- **Paragraph**: Lowest priority, delegates to text
- **ATX Heading**: Level tracking, optional closing sequence  
- **Thematic Break**: Marker consistency checking
- **Indented Code**: Precise indentation tracking, continuation logic

Successfully completing Phase 5 means:
- ✅ Can parse basic markdown documents
- ✅ Flow dispatcher working correctly
- ✅ Event stream architecture validated
- ✅ Ready for inline constructs (Phase 6)

**Estimated Completion**: 1.5 weeks with focused development

Good luck! 🚀