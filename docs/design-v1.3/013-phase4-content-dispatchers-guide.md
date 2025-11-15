# erlmd Phase 4: Content Dispatchers Implementation Guide

**Version**: 1.0  
**Date**: November 14, 2025  
**Phase**: 4 (Content Dispatchers)  
**Duration**: 1.5 weeks  
**Priority**: HIGH - Foundation for all construct parsing

---

## Table of Contents

1. [Phase Overview](#phase-overview)
2. [Context & Architecture](#context--architecture)
3. [Reference Materials](#reference-materials)
4. [Module Specifications](#module-specifications)
5. [Implementation Patterns](#implementation-patterns)
6. [Test Requirements](#test-requirements)
7. [Acceptance Criteria](#acceptance-criteria)
8. [Common Pitfalls](#common-pitfalls)
9. [Integration Notes](#integration-notes)

---

## Phase Overview

### Goal

Implement the hierarchical content type system that determines which markdown constructs are valid at each parsing level. This is the foundation for all construct parsing.

### Deliverables

1. **`erlmd_cnstr_string.erl`** - String content dispatcher (simplest)
2. **`erlmd_cnstr_text.erl`** - Text/inline content dispatcher
3. **`erlmd_cnstr_flow.erl`** - Flow/block content dispatcher
4. **`erlmd_cnstr_document.erl`** - Document/top-level dispatcher

### Key Concept: Content Type Hierarchy

Content dispatchers form a hierarchy where each level determines which constructs can appear:

```
Document (top-level)
    â"‚
    â"œâ"€â"€> Flow (block-level)
    â"‚       â"‚
    â"‚       â""â"€â"€> Content (definitions + paragraph)
    â"‚               â""â"€â"€> Text (inline)
    â"‚                       â""â"€â"€> String (literals)
    â"‚
    â""â"€â"€> Containers (block quotes, lists, etc.)
            â""â"€â"€> [loops back to Flow]
```

**Content Types**:
- **Document**: Top-level containers (block quotes, list items, footnote definitions)
- **Flow**: Block-level constructs (headings, paragraphs, code blocks, thematic breaks)
- **Text**: Inline constructs (emphasis, links, images, code spans, escapes)
- **String**: Literal strings (in code info, URLs, titles) - only escapes and entities allowed

### Why This Matters

The content dispatcher pattern is critical because:
1. **Construct Priority**: Determines the order constructs are tried (blank lines before paragraphs)
2. **Backtracking**: Uses `attempt` mechanism to try constructs without committing
3. **Context Awareness**: Different constructs are valid in different contexts
4. **Performance**: Tries most likely constructs first, avoiding unnecessary work

---

## Context & Architecture

### Dependencies

**Required Modules** (must exist from previous phases):
- `erlmd_tokenizer.erl` - State machine driver with attempt/enter/exit/consume
- `erlmd_state.erl` - State dispatcher that routes to construct modules
- `erlmd_cnstr_prtl_data.erl` - Data construct (fallback for all dispatchers)
- `include/types.hrl` - Core record definitions

**Stub Modules** (referenced but implemented later):
- Block constructs: `blank_line`, `code_indented`, `code_fenced`, `heading_atx`, etc.
- Inline constructs: `character_escape`, `character_reference`, `autolink`, etc.
- Container constructs: `block_quote`, `list_item`, etc.

### Architecture Pattern

All content dispatchers follow the **attempt-and-try pattern**:

1. **Maintain a construct list** defining valid constructs in priority order
2. **Try each construct** using `attempt` (allows backtracking on failure)
3. **On success**: Continue dispatching (may need more content)
4. **On failure**: Try next construct in list
5. **All failed**: Return `nok` (or delegate to data as ultimate fallback)

---

## Reference Materials

### Rust Implementation Files

- **`markdown-rs/src/construct/string.rs`** - String content reference
- **`markdown-rs/src/construct/text.rs`** - Text/inline content reference  
- **`markdown-rs/src/construct/flow.rs`** - Flow/block content reference
- **`markdown-rs/src/construct/document.rs`** - Document content reference
- **`markdown-rs/src/construct/content.rs`** - Content (definitions + paragraph) reference

### Key Rust Patterns to Translate

**Markers Array** (Rust):
```rust
const MARKERS: [u8; 16] = [
    b'!', b'$', b'&', b'*', b'<', b'H', b'W', b'[', 
    b'\\', b']', b'_', b'`', b'h', b'w', b'{', b'~',
];
```

**Erlang Translation**: We don't use markers arrays in Erlang. Instead, we pattern match in function heads or use case statements. The tokenizer's attempt mechanism handles trying constructs.

**Attempt Pattern** (Rust):
```rust
tokenizer.attempt(
    State::Next(StateName::TextBefore),
    State::Next(StateName::TextBeforeData),
);
State::Retry(StateName::CharacterReferenceStart)
```

**Erlang Translation**:
```erlang
erlmd_tokenizer:attempt(T, character_reference, nok)
```

### Erlang Patterns Documentation

See **"State Machine Implementation Without gen_statem"** in `002-rewrite-research-erlang-markdown-implementation-patterns.md`:

- Direct function calls (not gen_statem behavior)
- Tail recursion for state transitions
- Match context preservation in function heads
- Records for state, pattern matching on binary

---

## Module Specifications

### Module 1: `erlmd_cnstr_string.erl`

**Purpose**: Simplest content dispatcher - only allows character escapes and character references.

**Used in**: Code fence info strings, link titles, link destinations, reference definitions.

**Constructs Allowed**:
1. `character_escape` - Backslash escapes (`\*`, `\[`, etc.)
2. `character_reference` - HTML entities (`&amp;`, `&#35;`, etc.)
3. `data` - Fallback for everything else (literal characters)

#### Implementation

```erlang
-module(erlmd_cnstr_string).
-export([start/1]).

-include("types.hrl").

%% String content constructs in priority order
-define(STRING_CONSTRUCTS, [
    character_escape,
    character_reference
]).

%% @doc Entry point for string content parsing.
%% String is the simplest content type - only escapes, entities, and data.
-spec start(erlmd_tokenizer:tokenizer()) -> {erlmd_types:state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    try_constructs(T, ?STRING_CONSTRUCTS).

%% @doc Try each construct in order until one succeeds.
%% Pattern: attempt construct, on success continue with string, on failure try next.
try_constructs(T, []) ->
    %% No constructs succeeded, try data as ultimate fallback
    erlmd_tokenizer:attempt(T, data, nok);

try_constructs(T, [Construct | Rest]) ->
    case erlmd_tokenizer:attempt(T, Construct, nok) of
        {ok, T1} ->
            %% Construct succeeded, continue parsing string content
            start(T1);
        {nok, T1} ->
            %% Construct failed, try next in list
            try_constructs(T1, Rest)
    end.
```

#### Key Points

- **No enter/exit**: String dispatchers don't generate events themselves
- **Tail recursive**: `try_constructs` is tail-recursive for each construct attempt
- **Fallback**: Data construct catches everything that escapes/entities don't match
- **Simplest pattern**: This is the template for all other dispatchers

---

### Module 2: `erlmd_cnstr_text.erl`

**Purpose**: Inline/phrasing content dispatcher - handles emphasis, links, code spans, etc.

**Used in**: Paragraph content, heading content, table cells, etc.

**Constructs Allowed** (in priority order):
1. `gfm_task_list_item_check` - GFM task list checkboxes (special case: only at start)
2. `label_start_image` - Image syntax `![`
3. `raw_text` - Code spans and math (text) `` `code` ``
4. `character_reference` - HTML entities
5. `attention` - Emphasis/strong/strikethrough (`*`, `_`, `~`)
6. `autolink` - Autolinks `<http://example.com>`
7. `html_text` - HTML tags in text
8. `mdx_jsx_text` - MDX JSX components (if enabled)
9. `gfm_autolink_literal` - GFM: Bare URLs (if enabled)
10. `gfm_label_start_footnote` - GFM: Footnote references `[^1]`
11. `character_escape` - Backslash escapes
12. `hard_break_escape` - Hard line break `\` + EOL
13. `label_start_link` - Link syntax `[`
14. `label_end` - Link/image closer `]`
15. `mdx_expression_text` - MDX expressions (if enabled)
16. `data` - Fallback for literal text

#### Implementation

```erlang
-module(erlmd_cnstr_text).
-export([start/1]).

-include("types.hrl").

%% Text content constructs in priority order
%% Note: This is the order from markdown-rs/src/construct/text.rs
-define(TEXT_CONSTRUCTS, [
    %% Task list item check is special - only at start of first paragraph
    gfm_task_list_item_check,
    %% Images before links (same opener `[`)
    label_start_image,
    %% Code/math spans
    raw_text,
    %% Entities
    character_reference,
    %% Emphasis/strong
    attention,
    %% Autolinks
    autolink,
    html_text,
    %% MDX (if enabled - will fail gracefully if not implemented)
    mdx_jsx_text,
    %% GFM bare URLs
    gfm_autolink_literal,
    %% GFM footnotes
    gfm_label_start_footnote,
    %% Escapes
    character_escape,
    hard_break_escape,
    %% Links (after images)
    label_start_link,
    label_end,
    %% MDX expressions
    mdx_expression_text
]).

%% @doc Entry point for text (inline) content parsing.
-spec start(erlmd_tokenizer:tokenizer()) -> {erlmd_types:state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    try_constructs(T, ?TEXT_CONSTRUCTS).

%% @doc Try each construct in order until one succeeds.
try_constructs(T, []) ->
    %% No constructs succeeded, try data as ultimate fallback
    erlmd_tokenizer:attempt(T, data, nok);

try_constructs(T, [Construct | Rest]) ->
    case erlmd_tokenizer:attempt(T, Construct, nok) of
        {ok, T1} ->
            %% Construct succeeded, continue parsing text content
            start(T1);
        {nok, T1} ->
            %% Construct failed, try next in list
            try_constructs(T1, Rest)
    end.
```

#### Key Points

- **Task list check first**: Special case - only valid at paragraph start
- **Images before links**: Both start with `[`, images checked first
- **GFM constructs**: Will fail gracefully if not implemented yet
- **MDX constructs**: Will fail gracefully if not implemented
- **Same pattern**: Identical structure to string dispatcher, just more constructs

---

### Module 3: `erlmd_cnstr_flow.erl`

**Purpose**: Block-level content dispatcher - handles headings, paragraphs, code blocks, etc.

**Used in**: Document body, container content (block quotes, list items).

**Constructs Allowed** (in priority order):
1. `blank_line` - Empty lines (highest priority - can interrupt)
2. `code_indented` - Indented code blocks (4 spaces)
3. `raw_flow` - Fenced code/math blocks (`` ``` ``, `~~~`)
4. `heading_atx` - ATX headings (`#`)
5. `html_flow` - HTML blocks (`<div>`, etc.)
6. `mdx_esm` - MDX import/export (if enabled)
7. `mdx_expression_flow` - MDX block expressions (if enabled)
8. `mdx_jsx_flow` - MDX JSX blocks (if enabled)
9. `thematic_break` - Horizontal rules (`---`, `***`)
10. `heading_setext` - Setext headings (underline style)
11. `gfm_table` - GFM tables (if enabled)
12. `definition` - Link reference definitions
13. `paragraph` - Default block construct (MUST BE LAST)

#### Implementation

```erlang
-module(erlmd_cnstr_flow).
-export([start/1]).

-include("types.hrl").

%% Flow (block-level) content constructs in priority order
%% Based on markdown-rs/src/construct/flow.rs
-define(FLOW_CONSTRUCTS, [
    %% Blank line has highest priority - can interrupt anything
    blank_line,
    %% Code blocks
    code_indented,
    raw_flow,  % Fenced code/math
    %% Headings
    heading_atx,
    heading_setext,
    %% HTML
    html_flow,
    %% MDX (if enabled - will fail gracefully)
    mdx_esm,
    mdx_expression_flow,
    mdx_jsx_flow,
    %% Thematic break
    thematic_break,
    %% GFM table (if enabled)
    gfm_table,
    %% Definitions and content
    definition,
    %% Paragraph MUST BE LAST - it's the default/fallback
    paragraph
]).

%% @doc Entry point for flow (block-level) content parsing.
-spec start(erlmd_tokenizer:tokenizer()) -> {erlmd_types:state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    try_constructs(T, ?FLOW_CONSTRUCTS).

%% @doc Try each construct in order until one succeeds.
try_constructs(T, []) ->
    %% Should never happen - paragraph should always succeed as fallback
    {nok, T};

try_constructs(T, [Construct | Rest]) ->
    case erlmd_tokenizer:attempt(T, Construct, nok) of
        {ok, T1} ->
            %% Construct succeeded
            {ok, T1};
        {nok, T1} ->
            %% Construct failed, try next in list
            try_constructs(T1, Rest)
    end.
```

#### Key Points

- **Blank line first**: Can interrupt most other constructs
- **Paragraph last**: Must be last - it's the fallback that always succeeds
- **No recursion to start**: Unlike string/text, flow returns after ONE construct
- **Called repeatedly**: Document dispatcher calls flow repeatedly until EOF

#### Special Note: Flow vs Content

The Rust implementation has both `flow.rs` and `content.rs`:
- **flow.rs**: Tries all block-level constructs
- **content.rs**: Handles the special case of definitions followed by paragraph

For Phase 4, we're implementing **flow only**. The `content` pattern (definitions + paragraph) will be integrated into flow dispatcher or handled by a separate resolver in later phases.

---

### Module 4: `erlmd_cnstr_document.erl`

**Purpose**: Top-level document dispatcher - manages containers and flow content.

**Used in**: Parser entry point, container recursion.

**Responsibility**:
1. Handle optional BOM (Byte Order Mark) at document start
2. Handle optional frontmatter (YAML/TOML front matter)
3. Try existing containers (block quotes, list items) for continuation
4. Try new containers
5. Dispatch to flow for actual content
6. Handle line endings and EOF

#### Implementation (Simplified for Phase 4)

For Phase 4, we'll implement a **simplified version** that focuses on flow dispatching. Full container handling (block quotes, lists) will be added in Phase 8.

```erlang
-module(erlmd_cnstr_document).
-export([start/1]).

-include("types.hrl").

%% @doc Entry point for document-level parsing.
%% For Phase 4, this is simplified - no container handling yet.
-spec start(erlmd_tokenizer:tokenizer()) -> {erlmd_types:state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    %% Enter document token
    T1 = erlmd_tokenizer:enter(T, document),
    %% Try optional BOM
    case erlmd_tokenizer:attempt(T1, bom, nok) of
        {ok, T2} ->
            parse_flow_loop(T2);
        {nok, T2} ->
            parse_flow_loop(T2)
    end.

%% @doc Main loop: repeatedly parse flow constructs until EOF.
parse_flow_loop(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            %% End of document
            T1 = erlmd_tokenizer:exit(T),
            {ok, T1};
        _ ->
            %% Try to parse flow content
            case erlmd_state:call(flow, T) of
                {ok, T1} ->
                    %% Flow construct succeeded, continue
                    parse_flow_loop(T1);
                {nok, T1} ->
                    %% Flow construct failed - shouldn't happen if paragraph is last
                    %% This is an error condition
                    T2 = erlmd_tokenizer:exit(T1),
                    {error, {unexpected_content, erlmd_tokenizer:current(T1)}, T2}
            end
    end.
```

#### Key Points

- **Simplified**: Full version in Phase 8 will handle containers
- **Enter/Exit document**: Generates document token events
- **Loop until EOF**: Keeps parsing flow constructs
- **BOM handling**: Tries optional BOM at start
- **Error handling**: Returns error if flow fails (shouldn't happen with paragraph fallback)

#### Full Version (Reference for Phase 8)

The complete `document.rs` implementation includes:
- Container stack management
- Lazy continuation detection
- Pierce mode for interrupting containers
- Exit container generation
- Complex state tracking

We'll defer this complexity to Phase 8 (Complex Block Constructs).

---

## Implementation Patterns

### Pattern 1: The Attempt-and-Try Loop

All dispatchers follow this pattern:

```erlang
try_constructs(T, []) ->
    %% Base case: no constructs left, use fallback
    fallback_action(T);

try_constructs(T, [Construct | Rest]) ->
    case erlmd_tokenizer:attempt(T, Construct, nok) of
        {ok, T1} ->
            %% Success: continue with dispatcher (may need more content)
            dispatcher_start(T1);
        {nok, T1} ->
            %% Failure: try next construct
            try_constructs(T1, Rest)
    end.
```

### Pattern 2: Tail Recursion is Critical

All dispatcher functions MUST be tail-recursive:

```erlang
%% âœ" CORRECT - tail recursive
try_constructs(T, [C | Rest]) ->
    case erlmd_tokenizer:attempt(T, C, nok) of
        {ok, T1} -> start(T1);        % Tail call
        {nok, T1} -> try_constructs(T1, Rest)  % Tail call
    end.

%% âŒ WRONG - not tail recursive
try_constructs(T, [C | Rest]) ->
    case erlmd_tokenizer:attempt(T, C, nok) of
        {ok, T1} -> 
            Result = start(T1),       % Not a tail call!
            process_result(Result);   % Additional work after
        {nok, T1} -> try_constructs(T1, Rest)
    end.
```

### Pattern 3: Construct Priority Matters

The order of constructs in the list is critical:

1. **Interrupting constructs first**: `blank_line` before everything
2. **More specific before general**: `label_start_image` before `label_start_link`
3. **Fallback last**: `paragraph` must be last in flow, `data` last in text/string

### Pattern 4: Graceful Failure for Unimplemented Constructs

When a construct hasn't been implemented yet:

```erlang
%% In erlmd_state.erl dispatcher:
call(mdx_jsx_text, T) ->
    %% Not implemented yet, will be added in Phase 11
    {nok, T};

call(gfm_table, T) ->
    %% Not implemented yet, will be added in Phase 11
    {nok, T}.
```

This allows the dispatcher to try the construct, fail gracefully, and move to the next one.

### Pattern 5: No Events in Dispatchers

Content dispatchers DO NOT generate events themselves:

```erlang
%% âœ" CORRECT - dispatcher doesn't generate events
start(T) ->
    try_constructs(T, ?CONSTRUCTS).

%% âŒ WRONG - dispatcher shouldn't generate events
start(T) ->
    T1 = erlmd_tokenizer:enter(T, text_content),  % Don't do this!
    T2 = try_constructs(T1, ?CONSTRUCTS),
    erlmd_tokenizer:exit(T2).
```

Exception: `document` dispatcher DOES generate enter/exit events because it represents the document container.

---

## Test Requirements

### Test Structure

Create test files following this pattern:

```
test/
â"œâ"€â"€ erlmd_cnstr_string_test.erl
â"œâ"€â"€ erlmd_cnstr_text_test.erl
â"œâ"€â"€ erlmd_cnstr_flow_test.erl
â""â"€â"€ erlmd_cnstr_document_test.erl
```

### Module 1 Tests: String Dispatcher

```erlang
-module(erlmd_cnstr_string_test).
-include_lib("eunit/include/eunit.hrl").

%% Test helper - create tokenizer and parse with string dispatcher
parse_string(Input) ->
    T = erlmd_tokenizer:new(Input, #{}),
    {Result, T1} = erlmd_cnstr_string:start(T),
    {Result, erlmd_tokenizer:events(T1)}.

%% Test 1: Plain text becomes data
plain_text_test() ->
    {ok, Events} = parse_string(<<"hello">>),
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assertEqual(2, length(DataEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(DataEvents))#event.kind).

%% Test 2: Character escape
character_escape_test() ->
    {ok, Events} = parse_string(<<"hello\\*world">>),
    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assertEqual(2, length(EscapeEvents)),  % Enter + Exit
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assert(length(DataEvents) >= 2).  % Data before and after escape

%% Test 3: Character reference
character_reference_test() ->
    {ok, Events} = parse_string(<<"&amp;">>),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(RefEvents))#event.kind).

%% Test 4: Mixed content
mixed_content_test() ->
    {ok, Events} = parse_string(<<"foo\\*bar&amp;baz">>),
    %% Should have data, escape, data, reference, data events
    Names = [E#event.name || E <- Events],
    ?assert(lists:member(data, Names)),
    ?assert(lists:member(character_escape, Names)),
    ?assert(lists:member(character_reference, Names)).

%% Test 5: Empty string
empty_string_test() ->
    {ok, Events} = parse_string(<<>>),
    %% Should have no events or just empty data
    ?assert(length(Events) =< 2).
```

### Module 2 Tests: Text Dispatcher

```erlang
-module(erlmd_cnstr_text_test).
-include_lib("eunit/include/eunit.hrl").

parse_text(Input) ->
    T = erlmd_tokenizer:new(Input, #{}),
    {Result, T1} = erlmd_cnstr_text:start(T),
    {Result, erlmd_tokenizer:events(T1)}.

%% Test 1: Plain text
plain_text_test() ->
    {ok, Events} = parse_text(<<"hello world">>),
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assert(length(DataEvents) >= 2).  % At least Enter + Exit

%% Test 2: Code span
code_span_test() ->
    {ok, Events} = parse_text(<<"`code`">>),
    CodeEvents = [E || E <- Events, 
                  E#event.name =:= code_text orelse
                  E#event.name =:= raw_text],
    ?assert(length(CodeEvents) >= 2).

%% Test 3: Emphasis
emphasis_test() ->
    {ok, Events} = parse_text(<<"*emphasis*">>),
    AttentionEvents = [E || E <- Events, E#event.name =:= attention],
    ?assert(length(AttentionEvents) >= 2).

%% Test 4: Link
link_test() ->
    {ok, Events} = parse_text(<<"[link](url)">>),
    LabelEvents = [E || E <- Events, 
                   lists:member(E#event.name, [label_start_link, label_end])],
    ?assert(length(LabelEvents) > 0).

%% Test 5: Escape in text
escape_test() ->
    {ok, Events} = parse_text(<<"foo\\*bar">>),
    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assert(length(EscapeEvents) >= 2).
```

### Module 3 Tests: Flow Dispatcher

```erlang
-module(erlmd_cnstr_flow_test).
-include_lib("eunit/include/eunit.hrl").

parse_flow(Input) ->
    T = erlmd_tokenizer:new(Input, #{}),
    {Result, T1} = erlmd_cnstr_flow:start(T),
    {Result, erlmd_tokenizer:events(T1)}.

%% Test 1: Paragraph (fallback)
paragraph_test() ->
    {ok, Events} = parse_flow(<<"hello">>),
    ParaEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParaEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(ParaEvents))#event.kind).

%% Test 2: Blank line
blank_line_test() ->
    {ok, Events} = parse_flow(<<"  \n">>),
    BlankEvents = [E || E <- Events, E#event.name =:= blank_line],
    ?assert(length(BlankEvents) >= 1).

%% Test 3: ATX heading
heading_atx_test() ->
    {ok, Events} = parse_flow(<<"# Heading">>),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 4: Thematic break
thematic_break_test() ->
    {ok, Events} = parse_flow(<<"---">>),
    ThematicEvents = [E || E <- Events, E#event.name =:= thematic_break],
    ?assertEqual(2, length(ThematicEvents)).

%% Test 5: Code block (indented)
code_indented_test() ->
    {ok, Events} = parse_flow(<<"    code">>),
    CodeEvents = [E || E <- Events, E#event.name =:= code_indented],
    ?assert(length(CodeEvents) >= 2).

%% Test 6: Construct priority - blank line before paragraph
priority_test() ->
    {ok, Events} = parse_flow(<<"  ">>),
    %% Should match blank_line, not paragraph
    Names = [E#event.name || E <- Events],
    ?assert(lists:member(blank_line, Names)).
```

### Module 4 Tests: Document Dispatcher

```erlang
-module(erlmd_cnstr_document_test).
-include_lib("eunit/include/eunit.hrl").

parse_document(Input) ->
    T = erlmd_tokenizer:new(Input, #{}),
    {Result, T1} = erlmd_cnstr_document:start(T),
    {Result, erlmd_tokenizer:events(T1)}.

%% Test 1: Empty document
empty_document_test() ->
    {ok, Events} = parse_document(<<>>),
    DocEvents = [E || E <- Events, E#event.name =:= document],
    ?assertEqual(2, length(DocEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(DocEvents))#event.kind),
    ?assertEqual(exit, (lists:last(DocEvents))#event.kind).

%% Test 2: Simple paragraph document
simple_paragraph_test() ->
    {ok, Events} = parse_document(<<"Hello world">>),
    DocEvents = [E || E <- Events, E#event.name =:= document],
    ?assertEqual(2, length(DocEvents)),
    ParaEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assert(length(ParaEvents) >= 2).

%% Test 3: Multiple paragraphs
multiple_paragraphs_test() ->
    {ok, Events} = parse_document(<<"Para 1\n\nPara 2">>),
    ParaEvents = [E || E <- Events, E#event.name =:= paragraph],
    %% Should have 2 paragraphs (4 events: 2 enters + 2 exits)
    ?assertEqual(4, length(ParaEvents)).

%% Test 4: Mixed block constructs
mixed_constructs_test() ->
    {ok, Events} = parse_document(<<"# Heading\n\nParagraph\n\n---">>),
    Names = [E#event.name || E <- Events],
    ?assert(lists:member(document, Names)),
    ?assert(lists:member(heading_atx, Names)),
    ?assert(lists:member(paragraph, Names)),
    ?assert(lists:member(thematic_break, Names)).

%% Test 5: Document structure
document_structure_test() ->
    {ok, Events} = parse_document(<<"# Hi\n\nText">>),
    %% Document should be outermost token
    ?assertEqual(document, (hd(Events))#event.name),
    ?assertEqual(enter, (hd(Events))#event.kind),
    ?assertEqual(document, (lists:last(Events))#event.name),
    ?assertEqual(exit, (lists:last(Events))#event.kind).
```

### Integration Tests

```erlang
-module(erlmd_dispatcher_integration_test).
-include_lib("eunit/include/eunit.hrl").

%% Test complete parsing through all dispatchers
full_parse_test() ->
    Input = <<"# Heading\n\nThis is *emphasis* and `code`.\n\n---">>,
    Result = erlmd:parse(Input),
    Events = erlmd:events(Result),
    
    %% Verify we have all expected event types
    Names = sets:from_list([E#event.name || E <- Events]),
    Expected = sets:from_list([
        document, heading_atx, paragraph, data,
        attention, raw_text, thematic_break
    ]),
    
    ?assert(sets:is_subset(Expected, Names)).

%% Test dispatcher nesting
nesting_test() ->
    Input = <<"Paragraph with `code` and *emphasis*">>,
    Result = erlmd:parse(Input),
    Events = erlmd:events(Result),
    
    %% Document -> Flow (paragraph) -> Text (attention, raw_text) -> String/Data
    %% Verify proper nesting by checking enter/exit pairs
    Stack = lists:foldl(fun(Event, Acc) ->
        case Event#event.kind of
            enter -> [Event#event.name | Acc];
            exit -> 
                [Top | Rest] = Acc,
                ?assertEqual(Top, Event#event.name),  % Proper nesting
                Rest
        end
    end, [], Events),
    
    ?assertEqual([], Stack),  % All tokens closed
    ok.
```

### Test Helpers

Create `test/test_helpers.erl`:

```erlang
-module(test_helpers).
-export([parse/1, parse_events/1, event_names/1, find_events/2]).

-include("types.hrl").

%% Parse input and return full result
parse(Input) when is_binary(Input) ->
    erlmd:parse(Input).

%% Parse and return just events
parse_events(Input) ->
    Result = parse(Input),
    erlmd:events(Result).

%% Extract event names from event list
event_names(Events) ->
    [E#event.name || E <- Events].

%% Find all events with given name
find_events(Events, Name) ->
    [E || E <- Events, E#event.name =:= Name].
```

---

## Acceptance Criteria

### Phase 4 Completion Checklist

- [ ] **Module Implementation**
  - [ ] `erlmd_cnstr_string.erl` implemented and tested
  - [ ] `erlmd_cnstr_text.erl` implemented and tested
  - [ ] `erlmd_cnstr_flow.erl` implemented and tested
  - [ ] `erlmd_cnstr_document.erl` implemented and tested

- [ ] **State Dispatcher Integration**
  - [ ] All dispatchers registered in `erlmd_state.erl`
  - [ ] State dispatcher can route to `string`, `text`, `flow`, `document`
  - [ ] Graceful failure for unimplemented constructs

- [ ] **Construct Priority**
  - [ ] Flow dispatcher tries `blank_line` before `paragraph`
  - [ ] Flow dispatcher has `paragraph` as last (fallback) construct
  - [ ] Text dispatcher tries `label_start_image` before `label_start_link`
  - [ ] All dispatchers follow correct priority order

- [ ] **Test Coverage**
  - [ ] All unit tests pass (30+ tests across 4 modules)
  - [ ] Integration tests pass
  - [ ] Event nesting is correct (enter/exit pairs)
  - [ ] Edge cases handled (empty input, EOF, etc.)

- [ ] **Code Quality**
  - [ ] No compiler warnings
  - [ ] All functions are tail-recursive
  - [ ] Binary match context preserved (check with `bin_opt_info`)
  - [ ] Dialyzer passes with no warnings
  - [ ] Code follows naming conventions from `012.3-naming-quick-reference.md`

- [ ] **Documentation**
  - [ ] Module documentation with @doc tags
  - [ ] Function specs with -spec types
  - [ ] Comments explain dispatch logic
  - [ ] Examples in documentation

- [ ] **Integration Ready**
  - [ ] Document dispatcher can parse simple documents
  - [ ] Flow dispatcher successfully delegates to paragraph
  - [ ] Text dispatcher successfully delegates to data
  - [ ] String dispatcher successfully delegates to data
  - [ ] Ready for Phase 5 (Basic Block Constructs)

---

## Common Pitfalls

### Pitfall 1: Not Tail-Recursive

**Problem**:
```erlang
%% âŒ BAD - not tail recursive
try_constructs(T, [C | Rest]) ->
    case erlmd_tokenizer:attempt(T, C, nok) of
        {ok, T1} -> 
            Result = start(T1),
            log_result(Result),  % Breaks tail recursion!
            Result;
        {nok, T1} -> try_constructs(T1, Rest)
    end.
```

**Solution**:
```erlang
%% âœ" GOOD - tail recursive
try_constructs(T, [C | Rest]) ->
    case erlmd_tokenizer:attempt(T, C, nok) of
        {ok, T1} -> start(T1);  % Tail call
        {nok, T1} -> try_constructs(T1, Rest)  % Tail call
    end.
```

### Pitfall 2: Wrong Construct Order

**Problem**:
```erlang
%% âŒ BAD - paragraph before blank_line
-define(FLOW_CONSTRUCTS, [
    paragraph,  % Will match everything!
    blank_line,  % Never reached
    heading_atx
]).
```

**Solution**:
```erlang
%% âœ" GOOD - specific before general
-define(FLOW_CONSTRUCTS, [
    blank_line,  % Can interrupt
    heading_atx,  % Specific
    paragraph  % Fallback - MUST BE LAST
]).
```

### Pitfall 3: Generating Events in Dispatcher

**Problem**:
```erlang
%% âŒ BAD - dispatcher generates events
start(T) ->
    T1 = erlmd_tokenizer:enter(T, text_content),
    T2 = try_constructs(T1, ?CONSTRUCTS),
    erlmd_tokenizer:exit(T2).
```

**Solution**:
```erlang
%% âœ" GOOD - only constructs generate events
start(T) ->
    try_constructs(T, ?CONSTRUCTS).
```

Exception: `document` dispatcher DOES generate events for document container.

### Pitfall 4: Forgetting Data Fallback

**Problem**:
```erlang
%% âŒ BAD - no fallback, returns nok
try_constructs(T, []) ->
    {nok, T}.  % What happens to the byte?
```

**Solution**:
```erlang
%% âœ" GOOD - data is fallback
try_constructs(T, []) ->
    erlmd_tokenizer:attempt(T, data, nok).
```

### Pitfall 5: Not Handling EOF

**Problem**:
```erlang
%% âŒ BAD - doesn't check for EOF
parse_flow_loop(T) ->
    case erlmd_state:call(flow, T) of
        {ok, T1} -> parse_flow_loop(T1);  % Infinite loop!
        {nok, T1} -> {error, T1}
    end.
```

**Solution**:
```erlang
%% âœ" GOOD - checks EOF
parse_flow_loop(T) ->
    case erlmd_tokenizer:current(T) of
        eof -> {ok, erlmd_tokenizer:exit(T)};
        _ ->
            case erlmd_state:call(flow, T) of
                {ok, T1} -> parse_flow_loop(T1);
                {nok, T1} -> {error, unexpected_content, T1}
            end
    end.
```

### Pitfall 6: Breaking Match Context

**Problem**:
```erlang
%% âŒ BAD - pattern matches on full binary
start(T) ->
    case T#tokenizer.bytes of
        <<$#, Rest/binary>> -> ...  % Creates sub-binary!
    end.
```

**Solution**:
```erlang
%% âœ" GOOD - use tokenizer operations
start(T) ->
    case erlmd_tokenizer:current(T) of
        $# -> ...
        _ -> ...
    end.
```

### Pitfall 7: Infinite Loop on Unrecognized Content

**Problem**:
```erlang
%% âŒ BAD - can loop infinitely if construct doesn't consume
try_constructs(T, [C | Rest]) ->
    case erlmd_tokenizer:attempt(T, C, nok) of
        {ok, T1} -> try_constructs(T1, ?CONSTRUCTS);  % Retry ALL constructs
        {nok, T1} -> try_constructs(T1, Rest)
    end.
```

**Solution**:
```erlang
%% âœ" GOOD - on success, call start (not try_constructs)
try_constructs(T, [C | Rest]) ->
    case erlmd_tokenizer:attempt(T, C, nok) of
        {ok, T1} -> start(T1);  % Continue from top
        {nok, T1} -> try_constructs(T1, Rest)
    end.
```

---

## Integration Notes

### State Dispatcher Updates

After implementing these modules, update `erlmd_state.erl`:

```erlang
%% In erlmd_state.erl, add these cases:

call(string, T) ->
    erlmd_cnstr_string:start(T);

call(text, T) ->
    erlmd_cnstr_text:start(T);

call(flow, T) ->
    erlmd_cnstr_flow:start(T);

call(document, T) ->
    erlmd_cnstr_document:start(T);

%% Stub handlers for constructs not yet implemented
call(paragraph, T) ->
    %% TODO: Implement in Phase 5
    {nok, T};

call(heading_atx, T) ->
    %% TODO: Implement in Phase 5
    {nok, T};

call(code_indented, T) ->
    %% TODO: Implement in Phase 5
    {nok, T};

%% ... etc for all constructs in the lists
```

### Parser Integration

Update `erlmd_parser.erl` to use document dispatcher:

```erlang
-module(erlmd_parser).
-export([parse/2]).

parse(Input, Options) when is_binary(Input) ->
    %% Create tokenizer
    T = erlmd_tokenizer:new(Input, Options),
    
    %% Parse document
    case erlmd_state:call(document, T) of
        {ok, T1} ->
            Events = erlmd_tokenizer:events(T1),
            {ok, #{events => Events}};
        {error, Reason, T1} ->
            {error, Reason, erlmd_tokenizer:events(T1)}
    end.
```

### Testing the Integration

```erlang
%% In rebar3 shell:
1> c("src/erlmd_cnstr_string.erl").
2> c("src/erlmd_cnstr_text.erl").
3> c("src/erlmd_cnstr_flow.erl").
4> c("src/erlmd_cnstr_document.erl").
5> c("src/erlmd_state.erl").
6> c("src/erlmd_parser.erl").

%% Test simple document
7> erlmd_parser:parse(<<"Hello world">>, #{}).
{ok, #{events => [...]}}

%% Check events
8> {ok, Result} = erlmd_parser:parse(<<"Hello">>, #{}).
9> Events = maps:get(events, Result).
10> [E#event.name || E <- Events].
[document, paragraph, data, ...]
```

### Debugging Tips

**Enable debug logging**:
```erlang
%% Add to dispatcher start function:
start(T) ->
    io:format("~s:start/1 - Current byte: ~p~n", 
              [?MODULE, erlmd_tokenizer:current(T)]),
    try_constructs(T, ?CONSTRUCTS).
```

**Check event nesting**:
```erlang
check_nesting(Events) ->
    Stack = lists:foldl(fun(E, Acc) ->
        case E#event.kind of
            enter -> 
                io:format("Enter: ~p~n", [E#event.name]),
                [E#event.name | Acc];
            exit ->
                io:format("Exit: ~p~n", [E#event.name]),
                [_ | Rest] = Acc,
                Rest
        end
    end, [], Events),
    case Stack of
        [] -> io:format("âœ" Nesting OK~n"), ok;
        _ -> io:format("âŒ Nesting error: unclosed ~p~n", [Stack]), error
    end.
```

**Trace construct attempts**:
```erlang
%% Add to try_constructs:
try_constructs(T, [C | Rest]) ->
    io:format("Trying construct: ~p~n", [C]),
    case erlmd_tokenizer:attempt(T, C, nok) of
        {ok, T1} -> 
            io:format("  âœ" ~p succeeded~n", [C]),
            start(T1);
        {nok, T1} -> 
            io:format("  âŒ ~p failed~n", [C]),
            try_constructs(T1, Rest)
    end.
```

---

## Next Steps After Phase 4

Once Phase 4 is complete:

1. **Phase 5**: Implement basic block constructs
   - `erlmd_cnstr_paragraph.erl`
   - `erlmd_cnstr_heading_atx.erl`
   - `erlmd_cnstr_thematic_break.erl`
   - `erlmd_cnstr_code_indented.erl`

2. **Verify dispatcher integration**: All Phase 5 constructs should be called by flow dispatcher

3. **Begin end-to-end testing**: Simple documents should parse completely

4. **Performance baseline**: Measure parsing speed for Phase 6 optimizations

---

## Summary

Phase 4 establishes the **content dispatcher architecture** - the foundation for all construct parsing. By implementing the four dispatchers (string, text, flow, document), we create the hierarchical content type system that determines which constructs are valid at each level.

**Key achievements**:
- âœ… Content type hierarchy (document â†' flow â†' text â†' string)
- âœ… Construct priority system (blank lines first, paragraph last)
- âœ… Attempt-and-try pattern for backtracking
- âœ… Graceful handling of unimplemented constructs
- âœ… Foundation for all future construct implementations

**Pattern to remember**:
```erlang
%% All dispatchers follow this pattern:
try_constructs(T, []) -> fallback(T);
try_constructs(T, [C|Rest]) ->
    case attempt(T, C) of
        {ok, T1} -> continue(T1);
        {nok, T1} -> try_constructs(T1, Rest)
    end.
```

This pattern will be used throughout the remaining phases!

---

**End of Phase 4 Implementation Guide**

For questions or clarification, refer to:
- `002-rewrite-research-erlang-markdown-implementation-patterns.md` (Sections 3, 7)
- `007-erlmd-library-rewrite.md` (Section 5: Content Types)
- `erlmd-implementation-plan.md` (Phase 4 details)
- Rust source: `markdown-rs/src/construct/{string,text,flow,document}.rs`
