# Phase 1: Foundation Types - Implementation Instructions

**Project**: erlmd (Erlang Markdown Parser)
**Phase**: 1 - Foundation Types (Week 1)
**Duration**: 1 week (20-30 hours)
**Date**: November 2025

---

## Context

- **Phase**: Phase 1 of 12 (Foundation Types)
- **Module Group**: Core type definitions and basic utilities
- **Dependencies**: None (this is Layer 0 - the foundation)
- **Deliverables**: Core records, character utilities, constants, error types

## Reference Documents

### Rust Implementation
- `markdown-rs/src/unist.rs` - Point/Position structures
- `markdown-rs/src/event.rs` - Event types and Names (250+ event names)
- `markdown-rs/src/message.rs` - Error/message types
- `markdown-rs/src/util/char.rs` - Character classification
- `markdown-rs/src/util/constant.rs` - Constants and magic numbers

### Erlang Patterns
- Section 2: "Data Structure Choices: Records vs Maps"
- Section 3: "Binary Pattern Matching: The Foundation"
- Section 5: "Performance Optimization Patterns"

### Architecture Documentation
- Section 2: "Core Data Structures" (erlmd-library-rewrite.md)
- Event system architecture
- Position tracking requirements

---

## Requirements

Phase 1 establishes the **foundational types** that all other modules will depend on. You must implement:

### 1. Core Record Definitions (`src/types.hrl`)

Define records that represent the fundamental data structures:

- **Point**: Represents a single location in source (line, column, offset)
- **Position**: Represents a span (start point, end point)
- **Event**: The core unit of parsing (kind, name, point, link, content)
- **Link**: Connects events for nested content
- **Message**: Error/warning messages with location
- **Content Type**: Enum for content hierarchy (flow, text, string, content)

### 2. Character Classification (`src/erlmd_util_char.erl`)

Implement functions to classify bytes/chars for parsing:

- ASCII whitespace detection
- ASCII punctuation detection
- ASCII alphanumeric detection
- Unicode punctuation detection (for emphasis rules)
- Character kind classification (whitespace, punctuation, other)

### 3. Constants Module (`src/consts.hrl`)

Define all magic numbers and constants:

- Tab size (4)
- Code indent size (4)
- Heading max level (6)
- Character reference size limits
- List item value max (10 digits)
- HTML tag name lists (block names, raw names)
- Safe protocol lists

### 4. Basic Type Guards and Utilities

Helper functions for working with the core types:

- Type guards for records
- Point/Position construction helpers
- Event filtering and validation
- Pretty printing for debugging

---

## Implementation Hints

### 1. Use Records for Internal Performance

Records compile to tuples and provide O(1) field access:

```erlang
-record(point, {
    line = 1 :: pos_integer(),      % 1-indexed
    column = 1 :: pos_integer(),    % 1-indexed
    offset = 0 :: non_neg_integer() % 0-indexed byte position
}).

-record(position, {
    start :: point(),
    end :: point()
}).

-record(event, {
    kind :: enter | exit,
    name :: atom(),                 % Event name (paragraph, emphasis, etc.)
    point :: point(),
    link = undefined :: link() | undefined,
    content = undefined :: content_type() | undefined
}).
```

**Why records?**
- Fast field access via `element/2` (O(1))
- Pattern matching in function heads
- Type specifications
- Will convert to maps at API boundary

### 2. Event Names as Atoms

The Rust implementation has 250+ event names as an enum. In Erlang, represent these as atoms:

```erlang
% From event.rs Name enum
-type event_name() :: 
    % Core structural
    paragraph | heading_atx | heading_setext | thematic_break |
    code_fenced | code_indented | code_text |
    block_quote | list_item | list_ordered | list_unordered |
    
    % Inline
    emphasis | strong | link | image | data |
    character_escape | character_reference | code_text |
    
    % Markers
    heading_atx_sequence | emphasis_sequence | strong_sequence |
    thematic_break_sequence | line_ending | space_or_tab |
    
    % GFM extensions
    gfm_table | gfm_strikethrough | gfm_autolink_literal_protocol |
    gfm_footnote_call | gfm_task_list_item_check |
    
    % ... (see complete list in event.rs)
    .
```

**Implementation Strategy**:
1. Start with ~20 core event names needed for Phase 1-6
2. Add more as you implement constructs
3. Keep a complete list in comments from event.rs for reference

### 3. Character Classification Using Binary Patterns

Leverage Erlang's binary pattern matching for performance:

```erlang
%% Fast ASCII checks using guards
-spec is_ascii_whitespace(byte()) -> boolean().
is_ascii_whitespace(Byte) when Byte =:= $\s; Byte =:= $\t; 
                                 Byte =:= $\n; Byte =:= $\r -> true;
is_ascii_whitespace(_) -> false.

-spec is_ascii_punctuation(byte()) -> boolean().
is_ascii_punctuation(Byte) when Byte >= $! andalso Byte =< $/ -> true;
is_ascii_punctuation(Byte) when Byte >= $: andalso Byte =< $@ -> true;
is_ascii_punctuation(Byte) when Byte >= $[ andalso Byte =< $` -> true;
is_ascii_punctuation(Byte) when Byte >= ${ andalso Byte =< $~ -> true;
is_ascii_punctuation(_) -> false.

%% For unicode, fall back to string classification
-spec classify_char(char()) -> whitespace | punctuation | other.
classify_char(Char) ->
    if
        Char =:= $\s; Char =:= $\t; Char =:= $\n; Char =:= $\r ->
            whitespace;
        true ->
            % Check if unicode whitespace
            case unicode_util:is_whitespace([Char]) of
                true -> whitespace;
                false ->
                    % Check ASCII punctuation first
                    case is_ascii_punctuation(Char) of
                        true -> punctuation;
                        false ->
                            % Check unicode punctuation
                            case is_unicode_punctuation(Char) of
                                true -> punctuation;
                                false -> other
                            end
                    end
            end
    end.
```

**Unicode Punctuation**:
- Import the PUNCTUATION list from markdown-rs's unicode.rs
- Store as a sorted list or ETS table for O(log n) lookup
- Use `lists:member/2` or `ets:lookup/2`

### 4. Constants as Macros

Define constants as macros for compile-time substitution:

```erlang
%% Core sizes
-define(TAB_SIZE, 4).
-define(CODE_INDENT_SIZE, 4).
-define(HEADING_ATX_OPENING_FENCE_SIZE_MAX, 6).

%% Character reference limits
-define(CHARACTER_REFERENCE_DECIMAL_SIZE_MAX, 7).
-define(CHARACTER_REFERENCE_HEXADECIMAL_SIZE_MAX, 6).
-define(CHARACTER_REFERENCE_NAMED_SIZE_MAX, 31).

%% Code fence
-define(CODE_FENCED_SEQUENCE_SIZE_MIN, 3).

%% Thematic break
-define(THEMATIC_BREAK_MARKER_COUNT_MIN, 3).

%% List items
-define(LIST_ITEM_VALUE_SIZE_MAX, 10).
```

### 5. HTML Tag Lists

Store as lists of atoms (convert Rust string slices):

```erlang
-define(HTML_BLOCK_NAMES, [
    address, article, aside, base, basefont, blockquote, body,
    caption, center, col, colgroup, dd, details, dialog, dir,
    div, dl, dt, fieldset, figcaption, figure, footer, form,
    frame, frameset, h1, h2, h3, h4, h5, h6, head, header,
    hr, html, iframe, legend, li, link, main, menu, menuitem,
    nav, noframes, ol, optgroup, option, p, param, search,
    section, summary, table, tbody, td, tfoot, th, thead,
    title, tr, track, ul
]).

-define(HTML_RAW_NAMES, [pre, script, style, textarea]).

-define(GFM_HTML_TAGFILTER_NAMES, [
    iframe, noembed, noframes, plaintext, script,
    style, textarea, title, xmp
]).
```

### 6. Link Record for Nested Content

```erlang
-record(link, {
    previous = undefined :: non_neg_integer() | undefined,
    next = undefined :: non_neg_integer() | undefined,
    content :: content_type()
}).

-type content_type() :: flow | content | string | text.
```

**Purpose**: Links connect events that contain nested content (like a paragraph containing inline text).

### 7. Message/Error Records

```erlang
-record(message, {
    place = undefined :: place() | undefined,
    reason :: binary() | string(),
    rule_id :: binary() | atom(),
    source :: binary() | atom()
}).

-type place() :: {position, position()} | {point, point()}.
```

### 8. Position Tracking Utilities

```erlang
%% Create a point
-spec new_point(pos_integer(), pos_integer(), non_neg_integer()) -> point().
new_point(Line, Column, Offset) ->
    #point{line = Line, column = Column, offset = Offset}.

%% Create a position from two points
-spec new_position(point(), point()) -> position().
new_position(Start, End) ->
    #position{start = Start, end = End}.

%% Advance a point by one byte (not newline)
-spec advance_column(point()) -> point().
advance_column(#point{column = Col, offset = Off} = P) ->
    P#point{column = Col + 1, offset = Off + 1}.

%% Advance a point for a newline
-spec advance_line(point()) -> point().
advance_line(#point{line = Line, offset = Off} = P) ->
    P#point{line = Line + 1, column = 1, offset = Off + 1}.

%% Advance by N bytes (for multi-byte matches like "```")
-spec advance_by(point(), pos_integer()) -> point().
advance_by(#point{column = Col, offset = Off} = P, N) ->
    P#point{column = Col + N, offset = Off + N}.
```

---

## Detailed Module Specifications

### Module 1: `src/types.hrl`

**Purpose**: Define all core record types

**Contents**:
```erlang
%%% Core position tracking
-record(point, {
    line = 1 :: pos_integer(),
    column = 1 :: pos_integer(),
    offset = 0 :: non_neg_integer()
}).

-record(position, {
    start :: point(),
    end :: point()
}).

%%% Event system
-record(event, {
    kind :: enter | exit,
    name :: atom(),
    point :: point(),
    link = undefined :: link() | undefined,
    content = undefined :: content_type() | undefined
}).

-record(link, {
    previous = undefined :: non_neg_integer() | undefined,
    next = undefined :: non_neg_integer() | undefined,
    content :: content_type()
}).

-type content_type() :: flow | content | string | text.

%%% Error handling
-record(message, {
    place = undefined :: place() | undefined,
    reason :: binary() | string(),
    rule_id :: binary() | atom(),
    source :: binary() | atom()
}).

-type place() :: {position, position()} | {point, point()}.

%%% Type exports
-type point() :: #point{}.
-type position() :: #position{}.
-type event() :: #event{}.
-type link() :: #link{}.
-type message() :: #message{}.
```

**Testing**:
```erlang
-module(erlmd_types_test).
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

point_creation_test() ->
    P = #point{line = 5, column = 10, offset = 42},
    ?assertEqual(5, P#point.line),
    ?assertEqual(10, P#point.column),
    ?assertEqual(42, P#point.offset).

position_span_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 1, column = 5, offset = 4},
    Pos = #position{start = Start, end = End},
    ?assertEqual(Start, Pos#position.start),
    ?assertEqual(End, Pos#position.end).

event_creation_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    E = #event{kind = enter, name = paragraph, point = P},
    ?assertEqual(enter, E#event.kind),
    ?assertEqual(paragraph, E#event.name),
    ?assertEqual(undefined, E#event.link).
```

---

### Module 2: `src/erlmd_util_char.erl`

**Purpose**: Character classification for parsing

**API**:
```erlang
-module(erlmd_util_char).
-export([
    is_ascii_whitespace/1,
    is_ascii_punctuation/1,
    is_ascii_alphanumeric/1,
    is_unicode_whitespace/1,
    is_unicode_punctuation/1,
    classify/1,
    classify_byte/1,
    kind_after_index/2
]).

-type char_kind() :: whitespace | punctuation | other.
-export_type([char_kind/0]).
```

**Implementation**:
```erlang
-spec is_ascii_whitespace(byte()) -> boolean().
is_ascii_whitespace($ ) -> true;
is_ascii_whitespace($\t) -> true;
is_ascii_whitespace($\n) -> true;
is_ascii_whitespace($\r) -> true;
is_ascii_whitespace(_) -> false.

-spec is_ascii_punctuation(byte()) -> boolean().
is_ascii_punctuation(Byte) when Byte >= $! andalso Byte =< $/ -> true;
is_ascii_punctuation(Byte) when Byte >= $: andalso Byte =< $@ -> true;
is_ascii_punctuation(Byte) when Byte >= $[ andalso Byte =< $` -> true;
is_ascii_punctuation(Byte) when Byte >= ${ andalso Byte =< $~ -> true;
is_ascii_punctuation(_) -> false.

-spec is_ascii_alphanumeric(byte()) -> boolean().
is_ascii_alphanumeric(Byte) when Byte >= $0 andalso Byte =< $9 -> true;
is_ascii_alphanumeric(Byte) when Byte >= $A andalso Byte =< $Z -> true;
is_ascii_alphanumeric(Byte) when Byte >= $a andalso Byte =< $z -> true;
is_ascii_alphanumeric(_) -> false.

%% Classify a byte at index in a binary
-spec kind_after_index(binary(), non_neg_integer()) -> char_kind().
kind_after_index(Bytes, Index) when Index >= byte_size(Bytes) ->
    whitespace;  % EOF is treated as whitespace
kind_after_index(Bytes, Index) ->
    Byte = binary:at(Bytes, Index),
    classify_byte(Byte).

-spec classify_byte(byte()) -> char_kind().
classify_byte(Byte) when Byte < 128 ->
    % ASCII fast path
    case is_ascii_whitespace(Byte) of
        true -> whitespace;
        false ->
            case is_ascii_punctuation(Byte) of
                true -> punctuation;
                false ->
                    case is_ascii_alphanumeric(Byte) of
                        true -> other;
                        false -> other  % Control chars
                    end
            end
    end;
classify_byte(_Byte) ->
    % Non-ASCII - would need unicode checking
    % For Phase 1, treat as 'other'
    other.

%% Classify a unicode character (for future use)
-spec classify(char()) -> char_kind().
classify(Char) when Char < 128 ->
    classify_byte(Char);
classify(Char) ->
    % Check unicode whitespace
    case unicode_util:is_whitespace([Char]) of
        true -> whitespace;
        false ->
            % Check unicode punctuation (simplified for Phase 1)
            case is_unicode_punctuation(Char) of
                true -> punctuation;
                false -> other
            end
    end.

%% Unicode punctuation check (to be expanded in later phases)
-spec is_unicode_punctuation(char()) -> boolean().
is_unicode_punctuation(_Char) ->
    % TODO: Implement full unicode punctuation check
    % For now, return false
    false.
```

**Reference from Rust**:
- The `util/char.rs` file shows classification logic
- Uses Unicode categories for punctuation
- Fast path for ASCII

**Testing**:
```erlang
-module(erlmd_util_char_test).
-include_lib("eunit/include/eunit.hrl").

whitespace_test() ->
    ?assert(erlmd_util_char:is_ascii_whitespace($ )),
    ?assert(erlmd_util_char:is_ascii_whitespace($\t)),
    ?assert(erlmd_util_char:is_ascii_whitespace($\n)),
    ?assert(erlmd_util_char:is_ascii_whitespace($\r)),
    ?assertNot(erlmd_util_char:is_ascii_whitespace($a)).

punctuation_test() ->
    ?assert(erlmd_util_char:is_ascii_punctuation($!)),
    ?assert(erlmd_util_char:is_ascii_punctuation($*)),
    ?assert(erlmd_util_char:is_ascii_punctuation($_)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($a)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($5)).

classify_test() ->
    ?assertEqual(whitespace, erlmd_util_char:classify($ )),
    ?assertEqual(punctuation, erlmd_util_char:classify($*)),
    ?assertEqual(other, erlmd_util_char:classify($a)).

kind_after_index_test() ->
    Bytes = <<"Hello *world*">>,
    ?assertEqual(other, erlmd_util_char:kind_after_index(Bytes, 0)),     % H
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 5)), % space
    ?assertEqual(punctuation, erlmd_util_char:kind_after_index(Bytes, 6)), % *
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 99)). % EOF
```

---

### Module 3: `src/consts.hrl`

**Purpose**: Define all constants used in parsing

**API**:
```erlang
%% Core sizes
-define(TAB_SIZE, 4).
-define(CODE_INDENT_SIZE, 4).

-define(HEADING_ATX_MAX_LEVEL, 6).
-define(THEMATIC_BREAK_MIN_MARKERS, 3).
-define(CODE_FENCED_MIN_SEQUENCE, 3).
-define(LIST_ITEM_VALUE_MAX_SIZE, 10).

html_block_names() -> [
    address, article, aside, base, basefont, blockquote, body,
    caption, center, col, colgroup, dd, details, dialog, dir,
    div, dl, dt, fieldset, figcaption, figure, footer, form,
    frame, frameset, h1, h2, h3, h4, h5, h6, head, header,
    hr, html, iframe, legend, li, link, main, menu, menuitem,
    nav, noframes, ol, optgroup, option, p, param, search,
    section, summary, table, tbody, td, tfoot, th, thead,
    title, tr, track, ul
].

html_raw_names() -> [pre, script, style, textarea].
```

**Full Implementation of `src/consts.hrl`**:
```erlang
%% Macros for compile-time constants
-define(TAB_SIZE, 4).
-define(CODE_INDENT_SIZE, 4).
-define(HEADING_ATX_OPENING_FENCE_SIZE_MAX, 6).
-define(THEMATIC_BREAK_MARKER_COUNT_MIN, 3).
-define(CODE_FENCED_SEQUENCE_SIZE_MIN, 3).
-define(FRONTMATTER_SEQUENCE_SIZE, 3).
-define(MATH_FLOW_SEQUENCE_SIZE_MIN, 2).
-define(LIST_ITEM_VALUE_SIZE_MAX, 10).
-define(HARD_BREAK_PREFIX_SIZE_MIN, 2).
-define(LINK_REFERENCE_SIZE_MAX, 999).
-define(RESOURCE_DESTINATION_BALANCE_MAX, 32).

%% Character reference sizes
-define(CHARACTER_REFERENCE_DECIMAL_SIZE_MAX, 7).
-define(CHARACTER_REFERENCE_HEXADECIMAL_SIZE_MAX, 6).
-define(CHARACTER_REFERENCE_NAMED_SIZE_MAX, 31).

%% Autolink sizes
-define(AUTOLINK_SCHEME_SIZE_MAX, 32).
-define(AUTOLINK_DOMAIN_SIZE_MAX, 63).

%% GFM
-define(GFM_HTML_TAGFILTER_SIZE_MAX, 9).
-define(HTML_RAW_SIZE_MAX, 8).

%% Runtime accessors
-export([
    tab_size/0,
    code_indent_size/0,
    heading_atx_max_level/0,
    thematic_break_min_markers/0,
    code_fenced_min_sequence/0,
    frontmatter_sequence_size/0,
    math_flow_min_sequence/0,
    list_item_value_max_size/0,
    hard_break_prefix_min_size/0,
    link_reference_max_size/0,
    resource_destination_balance_max/0,
    character_reference_decimal_max/0,
    character_reference_hex_max/0,
    character_reference_named_max/0,
    autolink_scheme_max/0,
    autolink_domain_max/0,
    gfm_tagfilter_max/0,
    html_raw_max/0,
    html_block_names/0,
    html_raw_names/0,
    gfm_html_tagfilter_names/0,
    html_cdata_prefix/0,
    safe_protocol_href/0,
    safe_protocol_src/0
]).

html_raw_max() -> ?HTML_RAW_SIZE_MAX.

%% HTML tag name lists
html_block_names() -> [
    address, article, aside, base, basefont, blockquote, body,
    caption, center, col, colgroup, dd, details, dialog, dir,
    div, dl, dt, fieldset, figcaption, figure, footer, form,
    frame, frameset, h1, h2, h3, h4, h5, h6, head, header,
    hr, html, iframe, legend, li, link, main, menu, menuitem,
    nav, noframes, ol, optgroup, option, p, param, search,
    section, summary, table, tbody, td, tfoot, th, thead,
    title, tr, track, ul
].

html_raw_names() -> [pre, script, style, textarea].

gfm_html_tagfilter_names() -> [
    iframe, noembed, noframes, plaintext, script,
    style, textarea, title, xmp
].

%% HTML special sequences
-define(HTML_CDATA_PREFIX, <<"CDATA[">>).
-define(SAFE_PROTOCOL_HREF, [
    <<"http">>, <<"https">>, <<"irc">>, 
    <<"ircs">>, <<"mailto">>, <<"xmpp">>
]).
-define(SAFE_PROTOCOL_SRC, [
    <<"http">>, <<"https">>
]).

```

**Testing**:
```erlang
-module(erlmd_consts_test).
-include_lib("eunit/include/eunit.hrl").
-include("consts.hrl").

tab_size_test() ->
    ?assertEqual(4, ?TAB_SIZE).

heading_max_test() ->
    ?assertEqual(6, ?HEADING_ATX_MAX_LEVEL).

html_block_names_test() ->
    Names = erlmd_util_constant:html_block_names(),
    ?assert(lists:member(div, Names)),
    ?assert(lists:member(p, Names)),
    ?assertEqual(62, length(Names)).

html_raw_names_test() ->
    Names = erlmd_util_constant:html_raw_names(),
    ?assertEqual([pre, script, style, textarea], Names).
```

---

### Module 4: `src/erlmd_util_position.erl`

**Purpose**: Utilities for working with points and positions

**API**:
```erlang
-module(erlmd_util_position).
-include("types.hrl").

-export([
    new_point/3,
    new_position/2,
    new_position/6,
    advance_column/1,
    advance_line/1,
    advance_by/2,
    advance_by_tab/1,
    to_unist_point/1,
    to_unist_position/1,
    format_point/1,
    format_position/1
]).
```

**Implementation**:
```erlang
-module(erlmd_util_position).
-include("types.hrl").

-export([
    new_point/3,
    new_position/2,
    new_position/6,
    advance_column/1,
    advance_line/1,
    advance_by/2,
    advance_by_tab/1,
    to_unist_point/1,
    to_unist_position/1,
    format_point/1,
    format_position/1
]).

-spec new_point(pos_integer(), pos_integer(), non_neg_integer()) -> point().
new_point(Line, Column, Offset) ->
    #point{line = Line, column = Column, offset = Offset}.

-spec new_position(point(), point()) -> position().
new_position(Start, End) ->
    #position{start = Start, end = End}.

-spec new_position(pos_integer(), pos_integer(), non_neg_integer(),
                   pos_integer(), pos_integer(), non_neg_integer()) -> position().
new_position(StartLine, StartCol, StartOff, EndLine, EndCol, EndOff) ->
    #position{
        start = new_point(StartLine, StartCol, StartOff),
        end = new_point(EndLine, EndCol, EndOff)
    }.

%% Advance by one character (not newline, not tab)
-spec advance_column(point()) -> point().
advance_column(#point{column = Col, offset = Off} = P) ->
    P#point{column = Col + 1, offset = Off + 1}.

%% Advance for newline
-spec advance_line(point()) -> point().
advance_line(#point{line = Line, offset = Off} = P) ->
    P#point{line = Line + 1, column = 1, offset = Off + 1}.

%% Advance by N bytes
-spec advance_by(point(), pos_integer()) -> point().
advance_by(#point{column = Col, offset = Off} = P, N) ->
    P#point{column = Col + N, offset = Off + N}.

%% Advance by tab (respecting tab stops)
-spec advance_by_tab(point()) -> point().
advance_by_tab(#point{column = Col, offset = Off} = P) ->
    TabSize = erlmd_util_constant:tab_size(),
    Remainder = Col rem TabSize,
    Advance = if
        Remainder =:= 0 -> TabSize;
        true -> TabSize - Remainder
    end,
    P#point{column = Col + Advance, offset = Off + 1}.

%% Convert to unist-compatible map
-spec to_unist_point(point()) -> map().
to_unist_point(#point{line = Line, column = Col, offset = Off}) ->
    #{line => Line, column => Col, offset => Off}.

-spec to_unist_position(position()) -> map().
to_unist_position(#position{start = Start, end = End}) ->
    #{
        start => to_unist_point(Start),
        'end' => to_unist_point(End)
    }.

%% Format for debugging
-spec format_point(point()) -> binary().
format_point(#point{line = Line, column = Col, offset = Off}) ->
    iolist_to_binary(io_lib:format("~p:~p (~p)", [Line, Col, Off])).

-spec format_position(position()) -> binary().
format_position(#position{start = Start, end = End}) ->
    iolist_to_binary(io_lib:format("~s-~s", 
        [format_point(Start), format_point(End)])).
```

**Testing**:
```erlang
-module(erlmd_util_position_test).
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

new_point_test() ->
    P = erlmd_util_position:new_point(5, 10, 42),
    ?assertEqual(5, P#point.line),
    ?assertEqual(10, P#point.column),
    ?assertEqual(42, P#point.offset).

advance_column_test() ->
    P = #point{line = 1, column = 5, offset = 10},
    P2 = erlmd_util_position:advance_column(P),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(6, P2#point.column),
    ?assertEqual(11, P2#point.offset).

advance_line_test() ->
    P = #point{line = 5, column = 20, offset = 100},
    P2 = erlmd_util_position:advance_line(P),
    ?assertEqual(6, P2#point.line),
    ?assertEqual(1, P2#point.column),
    ?assertEqual(101, P2#point.offset).

advance_by_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_by(P, 3),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(4, P2#point.column),
    ?assertEqual(3, P2#point.offset).

advance_by_tab_test() ->
    % Column 1: tab should advance by 4
    P1 = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_by_tab(P1),
    ?assertEqual(5, P2#point.column),
    
    % Column 3: tab should advance by 2 (to next tab stop at 5)
    P3 = #point{line = 1, column = 3, offset = 2},
    P4 = erlmd_util_position:advance_by_tab(P3),
    ?assertEqual(5, P4#point.column).

format_test() ->
    P = #point{line = 10, column = 5, offset = 42},
    Formatted = erlmd_util_position:format_point(P),
    ?assertEqual(<<"10:5 (42)">>, Formatted).
```

---

## Test Requirements

### Comprehensive Testing

For Phase 1, you need extensive unit tests since these are foundation types:

```erlang
%%% test/erlmd_types_test.erl
-module(erlmd_types_test).
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%% Test point creation and field access
point_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    ?assertEqual(1, P#point.line),
    ?assertEqual(1, P#point.column),
    ?assertEqual(0, P#point.offset).

%%% Test position spans
position_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 1, column = 10, offset = 9},
    Pos = #position{start = Start, end = End},
    ?assertEqual(Start, Pos#position.start),
    ?assertEqual(End, Pos#position.end).

%%% Test event creation
event_enter_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    E = #event{kind = enter, name = paragraph, point = P},
    ?assertEqual(enter, E#event.kind),
    ?assertEqual(paragraph, E#event.name).

event_exit_test() ->
    P = #point{line = 5, column = 20, offset = 100},
    E = #event{kind = exit, name = paragraph, point = P},
    ?assertEqual(exit, E#event.kind).

%%% Test link creation
link_test() ->
    L = #link{previous = 10, next = 20, content = text},
    ?assertEqual(10, L#link.previous),
    ?assertEqual(20, L#link.next),
    ?assertEqual(text, L#link.content).

%%% Test message creation
message_test() ->
    P = #point{line = 5, column = 10, offset = 50},
    Place = {point, P},
    M = #message{
        place = Place,
        reason = <<"Unexpected character">>,
        rule_id = <<"MD001">>,
        source = <<"erlmd">>
    },
    ?assertEqual(Place, M#message.place),
    ?assertEqual(<<"Unexpected character">>, M#message.reason).
```

### Test Coverage Goals

- **Point operations**: 100% coverage
- **Position operations**: 100% coverage
- **Character classification**: 100% coverage
- **Constants access**: 100% coverage

### Test Execution

```bash
# Run all tests
rebar3 eunit

# Run specific module tests
rebar3 eunit --module=erlmd_util_char_test

# Run with coverage
rebar3 cover
```

---

## Acceptance Criteria

Phase 1 is complete when:

- [ ] All record types defined in `types.hrl`
- [ ] Character classification functions implemented
- [ ] All constants defined and accessible
- [ ] Position utilities work correctly
- [ ] All unit tests pass (>95% coverage)
- [ ] No compiler warnings with `bin_opt_info`
- [ ] Documentation complete (EDoc)
- [ ] Code compiles with dialyzer with no errors

### Validation Tests

```erlang
%%% Smoke test - can we create all types?
smoke_test() ->
    % Point
    P = #point{line = 1, column = 1, offset = 0},
    ?assert(is_record(P, point)),
    
    % Position
    Pos = #position{start = P, end = P},
    ?assert(is_record(Pos, position)),
    
    % Event
    E = #event{kind = enter, name = paragraph, point = P},
    ?assert(is_record(E, event)),
    
    % Link
    L = #link{content = text},
    ?assert(is_record(L, link)),
    
    % Message
    M = #message{reason = <<"test">>},
    ?assert(is_record(M, message)),
    
    % Character classification
    ?assertEqual(whitespace, erlmd_util_char:classify($ )),
    
    % Constants
    ?assertEqual(4, erlmd_util_constant:tab_size()),
    
    ok.
```

---

## Common Pitfalls to Avoid

### Pitfall 1: Forgetting Match Context Optimization

**WRONG**:
```erlang
% Creates sub-binaries!
classify_bytes(Binary) ->
    case Binary of
        <<Byte, Rest/binary>> -> [classify_byte(Byte) | classify_bytes(Rest)]
    end.
```

**RIGHT**:
```erlang
% Preserves match context
classify_bytes(<<Byte, Rest/binary>>) ->
    [classify_byte(Byte) | classify_bytes(Rest)];
classify_bytes(<<>>) ->
    [].
```

### Pitfall 2: Using Maps for Internal State

**WRONG** (for hot path):
```erlang
Point = #{line => 1, column => 1, offset => 0},
NewPoint = Point#{column := 2}.  % Slower than record
```

**RIGHT**:
```erlang
Point = #point{line = 1, column = 1, offset = 0},
NewPoint = Point#point{column = 2}.  % O(1) field access
```

### Pitfall 3: Not Handling All Line Endings

**WRONG**:
```erlang
advance_line(<<$\n, _/binary>>, Point) -> ...
% Forgets \r and \r\n
```

**RIGHT**:
```erlang
advance_line(<<$\r, $\n, _/binary>>, Point) -> % Windows CRLF
    Point#point{line = Point#point.line + 1, ...};
advance_line(<<$\n, _/binary>>, Point) ->      % Unix LF
    Point#point{line = Point#point.line + 1, ...};
advance_line(<<$\r, _/binary>>, Point) ->      % Old Mac CR
    Point#point{line = Point#point.line + 1, ...}.
```

---

## Progress Tracking

Use this checklist to track progress:

```markdown
## Phase 1 Progress

### Module Implementation
- [ ] types.hrl - Core records defined
- [ ] erlmd_util_char.erl - Character classification
- [ ] consts.hrl - All constants defined
- [ ] erlmd_util_position.erl - Position utilities

### Testing
- [ ] Unit tests written for all modules
- [ ] Test coverage > 95%
- [ ] All tests passing
- [ ] Manual smoke tests pass

### Documentation
- [ ] EDoc comments for all public functions
- [ ] Type specifications complete
- [ ] Usage examples in documentation
- [ ] README updated

### Code Quality
- [ ] No compiler warnings
- [ ] Dialyzer passes with no errors
- [ ] bin_opt_info checked (no binary copying)
- [ ] Code review complete
```

---

## Next Steps (After Phase 1)

Once Phase 1 is complete, you will move to **Phase 2: Tokenizer Framework**:
- Implement the core state machine driver
- Add state transition logic
- Implement attempt/backtracking
- Begin basic parsing tests

But for now, focus on getting these foundation types rock-solid!

---

## Quick Reference

### Key Files from markdown-rs
- `src/unist.rs` - Point/Position definitions
- `src/event.rs` - Event types (250+ names)
- `src/message.rs` - Error handling
- `src/util/char.rs` - Character classification
- `src/util/constant.rs` - All magic numbers

### Key Erlang Patterns
- Records for internal performance
- Binary pattern matching in function heads
- Tail recursion for all loops
- Match context preservation
- Guards for fast type checks

### Testing Command
```bash
rebar3 eunit
rebar3 cover
rebar3 dialyzer
```

---

**Good luck with Phase 1 implementation! Focus on correctness and comprehensive testing. Performance optimization comes later.**
