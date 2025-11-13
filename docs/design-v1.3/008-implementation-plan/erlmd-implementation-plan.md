# erlmd Implementation Plan: Phased Approach

**Project**: Rewrite erlmd (Erlang Markdown Parser) from scratch
**Reference**: markdown-rs (Rust implementation)
**Target**: CommonMark + GFM compliance
**Estimated Duration**: 16-20 weeks
**Date**: November 12, 2025

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Phase Breakdown](#phase-breakdown)
3. [Detailed Phase Specifications](#detailed-phase-specifications)
4. [AI Prompt Templates](#ai-prompt-templates)
5. [Success Criteria & Testing Strategy](#success-criteria--testing-strategy)
6. [Risk Management](#risk-management)

---

## Project Overview

### Architecture Summary

The erlmd parser follows an **event-driven state machine** architecture:

```
Markdown Input → Tokenizer (State Machine) → Event Stream → Processors → Output (HTML/AST)
```

**Key Principles**:

- Events are generated first (not direct AST building)
- Content types determine which constructs are active
- Backtracking via "attempt" mechanism for ambiguous syntax
- Nested content handled via subtokenization
- Post-processing via resolvers

### Technology Stack

- **Language**: Erlang/OTP 26+
- **Build Tool**: rebar3
- **Testing**: EUnit + CommonMark spec tests
- **Documentation**: EDoc

### Repository Structure

```
erlmd/
├── src/
│   ├── erlmd.erl              % Public API
│   ├── erlmd_parser.erl       % Parse orchestration
│   ├── erlmd_tokenizer.erl    % State machine driver
│   ├── erlmd_state.erl        % State dispatcher
│   ├── erlmd_event.erl        % Event types
│   ├── erlmd_cnstr_*          % Markdown constructs (40+ files)
│   ├── erlmd_util_*           % Utilities
│   ├── erlmd_html.erl
│   └── erlmd_ast.erl
├── test/
│   ├── commonmark_spec/       % CommonMark test suite
│   └── unit/                  % Unit tests
├── doc/
└── rebar.config
```

---

## Phase Breakdown

### Phase Structure

Each phase is designed to:

- ✅ Be **completable in 1-2 weeks**
- ✅ Have **clear, testable outcomes**
- ✅ Build **incrementally** on previous phases
- ✅ Map to **specific AI prompts**

### Phase Overview Table

| Phase | Duration | Focus | Deliverable | AI Prompts |
|-------|----------|-------|-------------|------------|
| 0 | 2-3 days | Project Setup | Working rebar3 project | 1 |
| 1 | 1 week | Foundation Types | Core records & utilities | 2-3 |
| 2 | 1 week | Tokenizer Framework | State machine skeleton | 2-3 |
| 3 | 1 week | Simple Constructs | Blank lines, data, whitespace | 3-4 |
| 4 | 1.5 weeks | Content Dispatchers | String, text, flow, document | 3-4 |
| 5 | 1.5 weeks | Basic Block Constructs | Paragraphs, headings, code | 4-5 |
| 6 | 1.5 weeks | Basic Inline Constructs | Escapes, entities, code | 4-5 |
| 7 | 2 weeks | Complex Inline | Emphasis, links, images | 5-6 |
| 8 | 2.5 weeks | Complex Block | Block quotes, lists | 5-6 |
| 9 | 2 weeks | Processing Pipeline | Subtokenization, resolvers | 4-5 |
| 10 | 1.5 weeks | Output Generation | HTML & AST compilers | 3-4 |
| 11 | 2 weeks | GFM Extensions | Tables, strikethrough, etc. | 6-8 |
| 12 | 2 weeks | Polish & Optimization | Performance, docs | 3-4 |

**Total**: 18-20 weeks

---

## Detailed Phase Specifications

### Phase 0: Project Setup (2-3 days)

**Goal**: Establish project structure and build infrastructure.

**Deliverables**:

1. rebar3 project initialized
2. Directory structure created
3. Basic CI/CD (GitHub Actions)
4. CommonMark spec test framework stubbed

**Tasks**:

- [ ] Initialize rebar3 project: `rebar3 new lib erlmd`
- [ ] Configure rebar.config with OTP 26+ requirement
- [ ] Set up directory structure
- [ ] Download CommonMark spec tests (JSON format)
- [ ] Create test runner skeleton
- [ ] Set up GitHub repository
- [ ] Create README with project goals

**Test**: `rebar3 compile` succeeds, test framework runs (0 tests pass).

**AI Prompt Count**: 1 prompt

- "Set up erlmd project structure with rebar3"

---

### Phase 1: Foundation Types (1 week)

**Goal**: Implement core data structures and basic utilities.

**Deliverables**:

1. Core record definitions
2. Point/Position tracking
3. Character classification utilities
4. Constants module
5. Error types

**Modules to Create**:

#### 1.1 `erlmd_types.hrl` - Core Records

```erlang
-record(event, {
    kind :: enter | exit,
    name :: atom(),
    point :: point(),
    link = undefined :: link() | undefined,
    content = undefined :: content_type() | undefined
}).

-record(point, {
    line :: pos_integer(),      % 1-indexed
    column :: pos_integer(),    % 1-indexed
    offset :: non_neg_integer() % 0-indexed byte position
}).

-record(position, {
    start :: point(),
    end :: point()
}).

-record(message, {
    reason :: atom() | binary(),
    position :: position() | undefined
}).
```

#### 1.2 `erlmd_util_char.erl` - Character Classification

**Key Functions**:

```erlang
-spec is_whitespace(byte()) -> boolean().
-spec is_ascii_alpha(byte()) -> boolean().
-spec is_ascii_alphanumeric(byte()) -> boolean().
-spec is_ascii_punctuation(byte()) -> boolean().
-spec is_unicode_whitespace(binary()) -> boolean().
-spec is_unicode_punctuation(binary()) -> boolean().
```

**Implementation Notes**:

- Use binary pattern matching for ASCII
- Reference Unicode categories for extended checks
- Optimize hot paths (whitespace, alphanumeric)

#### 1.3 `erlmd_util_const.erl` - Constants

```erlang
-define(TAB_SIZE, 4).
-define(CODE_INDENT_SIZE, 4).
-define(LIST_ITEM_PREFIX_SIZE_MAX, 4).

%% Character sets
-define(WHITESPACE_CHARS, [$ , $\t, $\n, $\r]).
-define(PUNCTUATION_CHARS, [...]).
```

**Tasks**:

- [ ] Define all record types in header file
- [ ] Implement character classification (ASCII first)
- [ ] Implement Unicode character checks (refer to spec)
- [ ] Create constants module
- [ ] Write comprehensive unit tests for character utils
- [ ] Document character classification rules

**Test Success Criteria**:

- All character classification tests pass
- Point/Position records can be created and serialized
- Constants accessible from all modules

**AI Prompt Count**: 2-3 prompts

- "Implement erlmd core record types"
- "Implement character classification utilities"
- "Create constants and configuration types"

---

### Phase 2: Tokenizer Framework (1 week)

**Goal**: Build the state machine driver that orchestrates parsing.

**Deliverables**:

1. Tokenizer state management
2. Event generation API
3. State transition mechanisms
4. Attempt/backtracking framework

**Modules to Create**:

#### 2.1 `erlmd_tokenizer.erl` - State Machine Driver

**Core State Record**:

```erlang
-record(tokenizer, {
    bytes :: binary(),           % Input
    index = 0 :: non_neg_integer(), % Current position
    line = 1 :: pos_integer(),
    column = 1 :: pos_integer(),
    events = [] :: [event()],    % Accumulated events
    stack = [] :: [atom()],      % Token stack
    attempts = [] :: [attempt()], % Backtrack checkpoints
    definitions = #{} :: map(),   % Link definitions
    options :: map()             % Parse options
}).
```

**Key Functions**:

```erlang
%% State machine operations
-spec new(binary(), map()) -> tokenizer().
-spec current(tokenizer()) -> byte() | eof.
-spec consume(tokenizer()) -> tokenizer().
-spec move_to(tokenizer(), non_neg_integer()) -> tokenizer().

%% Event generation
-spec enter(tokenizer(), atom()) -> tokenizer().
-spec exit(tokenizer()) -> tokenizer().

%% Backtracking
-spec attempt(tokenizer(), atom(), atom()) -> {ok | nok, tokenizer()}.

%% State transitions
-spec call(tokenizer(), atom()) -> {state_result(), tokenizer()}.
```

**Implementation Pattern** (from Erlang patterns doc):

```erlang
%% Use direct function calls, not gen_statem
%% Preserve match context in function heads
consume(#tokenizer{bytes = <<_, Rest/binary>>, index = Idx} = T) ->
    T#tokenizer{
        bytes = Rest,
        index = Idx + 1,
        column = T#tokenizer.column + 1
    }.
```

#### 2.2 `erlmd_state.erl` - State Dispatcher

**Purpose**: Maps state names (atoms) to construct functions.

```erlang
-spec call(atom(), tokenizer()) -> {state_result(), tokenizer()}.
call(document, Tokenizer) ->
    erlmd_construct_document:start(Tokenizer);
call(flow, Tokenizer) ->
    erlmd_construct_flow:start(Tokenizer);
call(blank_line, Tokenizer) ->
    erlmd_construct_blank_line:start(Tokenizer);
% ... etc for all constructs
call(UnknownState, Tokenizer) ->
    {error, {unknown_state, UnknownState}, Tokenizer}.
```

**Tasks**:

- [ ] Implement tokenizer record and basic ops
- [ ] Implement consume/move operations
- [ ] Implement enter/exit event generation
- [ ] Implement attempt mechanism (save/restore)
- [ ] Create state dispatcher skeleton
- [ ] Write unit tests for tokenizer operations
- [ ] Document state transition model

**Test Success Criteria**:

- Can create tokenizer from binary
- Can consume bytes and track position
- Can generate enter/exit events
- Attempt can save and restore state
- State dispatcher can route to dummy functions

**AI Prompt Count**: 2-3 prompts

- "Implement erlmd tokenizer state machine driver"
- "Implement attempt/backtracking mechanism"
- "Create state dispatcher framework"

---

### Phase 3: Simple Constructs (1 week)

**Goal**: Implement the simplest constructs to validate the architecture.

**Deliverables**:

1. Data (plain text) construct
2. Blank line detection
3. Whitespace handling
4. First end-to-end parse test

**Modules to Create**:

#### 3.1 `erlmd_construct_partial_data.erl`

**Purpose**: Parse plain text data (default fallback).

```erlang
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    T1 = erlmd_tokenizer:enter(T, data),
    consume_data(T1).

consume_data(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            {ok, erlmd_tokenizer:exit(T)};
        Byte when Byte =:= $\n; Byte =:= $\r ->
            {ok, erlmd_tokenizer:exit(T)};
        _Other ->
            T1 = erlmd_tokenizer:consume(T),
            consume_data(T1)
    end.
```

#### 3.2 `erlmd_construct_blank_line.erl`

**Purpose**: Detect blank lines (whitespace-only lines).

```erlang
start(T) ->
    check_whitespace(T).

check_whitespace(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when Byte =:= $ ; Byte =:= $\t ->
            check_whitespace(erlmd_tokenizer:consume(T));
        Byte when Byte =:= $\n; Byte =:= $\r; Byte =:= eof ->
            {ok, T};
        _Other ->
            {nok, T}
    end.
```

#### 3.3 `erlmd_construct_partial_whitespace.erl`

Similar to data, but only consumes whitespace.

**Tasks**:

- [ ] Implement partial_data construct
- [ ] Implement blank_line construct
- [ ] Implement partial_whitespace construct
- [ ] Create first integration test: "Hello" → [Enter(Data), Exit(Data)]
- [ ] Add constructs to state dispatcher
- [ ] Document construct interface pattern

**Test Success Criteria**:

- Can parse "Hello" → Data events
- Can detect blank lines
- Can parse "Hello\n\nWorld" correctly (two data, one blank)

**AI Prompt Count**: 3-4 prompts

- "Implement data construct"
- "Implement blank line construct"
- "Create first integration test"

---

### Phase 4: Content Dispatchers (1.5 weeks)

**Goal**: Implement the hierarchical content type system.

**Deliverables**:

1. String content dispatcher
2. Text content dispatcher
3. Flow content dispatcher
4. Document content dispatcher

**Key Concept**: Content dispatchers determine which constructs are valid at each level:

- **Document**: Top-level (flow constructs, definitions)
- **Flow**: Block-level (paragraphs, headings, lists, quotes)
- **Text**: Inline (emphasis, links, code, escapes)
- **String**: Literal strings (in code, attributes)

**Modules to Create**:

#### 4.1 `erlmd_construct_string.erl`

Simplest: only allows data and escapes.

```erlang
-define(STRING_CONSTRUCTS, [
    character_escape,
    character_reference,
    partial_data
]).

start(T) ->
    try_construct(T, ?STRING_CONSTRUCTS).

try_construct(T, []) ->
    {nok, T};
try_construct(T, [Construct | Rest]) ->
    case erlmd_tokenizer:attempt(T, Construct, nok) of
        {ok, T1} ->
            case erlmd_state:call(string, T1) of
                {ok, T2} -> {ok, T2};
                {nok, T2} -> {ok, T2}
            end;
        {nok, T1} ->
            try_construct(T1, Rest)
    end.
```

#### 4.2 `erlmd_construct_text.erl`

Inline constructs: data, escapes, code, emphasis, links, etc.

```erlang
-define(TEXT_CONSTRUCTS, [
    character_escape,
    character_reference,
    code_text,
    autolink,
    hard_break_escape,
    attention,  % emphasis/strong
    label_start_link,
    label_start_image,
    partial_data
]).
```

#### 4.3 `erlmd_construct_flow.erl`

Block-level constructs.

```erlang
-define(FLOW_CONSTRUCTS, [
    blank_line,
    thematic_break,
    code_indented,
    code_fenced,
    heading_atx,
    heading_setext,
    html_flow,
    definition,
    block_quote,
    list_item,
    paragraph  % Must be last (lowest priority)
]).
```

#### 4.4 `erlmd_construct_document.erl`

Top-level entry point.

```erlang
start(T) ->
    T1 = erlmd_tokenizer:enter(T, document),
    parse_flow(T1).

parse_flow(T) ->
    case erlmd_state:call(flow, T) of
        {ok, T1} ->
            case erlmd_tokenizer:current(T1) of
                eof ->
                    {ok, erlmd_tokenizer:exit(T1)};
                _ ->
                    parse_flow(T1)
            end;
        {nok, T1} ->
            {error, unexpected_content, T1}
    end.
```

**Tasks**:

- [ ] Implement string dispatcher
- [ ] Implement text dispatcher (stub constructs)
- [ ] Implement flow dispatcher (stub constructs)
- [ ] Implement document dispatcher
- [ ] Create construct priority/ordering tests
- [ ] Document content type hierarchy

**Test Success Criteria**:

- Document can dispatch to flow
- Flow can dispatch to paragraph (stub)
- Text can dispatch to data
- Construct ordering is correct (blank_line before paragraph)

**AI Prompt Count**: 3-4 prompts

- "Implement string and text dispatchers"
- "Implement flow dispatcher"
- "Implement document dispatcher"

---

### Phase 5: Basic Block Constructs (1.5 weeks)

**Goal**: Implement simple block-level elements.

**Deliverables**:

1. Paragraphs
2. ATX headings (# Heading)
3. Thematic breaks (---)
4. Indented code blocks

**Modules to Create**:

#### 5.1 `erlmd_construct_paragraph.erl`

```erlang
start(T) ->
    T1 = erlmd_tokenizer:enter(T, paragraph),
    parse_content(T1).

parse_content(T) ->
    case erlmd_state:call(text, T) of
        {ok, T1} ->
            case erlmd_tokenizer:current(T1) of
                Byte when Byte =:= $\n; Byte =:= eof ->
                    {ok, erlmd_tokenizer:exit(T1)};
                _ ->
                    parse_content(T1)
            end;
        {nok, T1} ->
            {nok, erlmd_tokenizer:exit(T1)}
    end.
```

#### 5.2 `erlmd_construct_heading_atx.erl`

```erlang
%% Match 1-6 # symbols followed by space
start(T = #tokenizer{bytes = <<$#, Rest/binary>>}) ->
    count_hashes(erlmd_tokenizer:consume(T), 1);
start(T) ->
    {nok, T}.

count_hashes(T = #tokenizer{bytes = <<$#, _/binary>>}, N) when N < 6 ->
    count_hashes(erlmd_tokenizer:consume(T), N + 1);
count_hashes(T = #tokenizer{bytes = <<$ , _/binary>>}, N) ->
    T1 = erlmd_tokenizer:enter(T, heading_atx),
    T2 = erlmd_tokenizer:enter(T1, heading_atx_text),
    parse_heading_content(erlmd_tokenizer:consume(T2), N);
count_hashes(T, _N) ->
    {nok, T}.

parse_heading_content(T, Level) ->
    %% Parse text content until newline
    case erlmd_state:call(text, T) of
        {ok, T1} ->
            case erlmd_tokenizer:current(T1) of
                $\n ->
                    T2 = erlmd_tokenizer:exit(T1),
                    {ok, erlmd_tokenizer:exit(T2)};
                _ ->
                    parse_heading_content(T1, Level)
            end;
        {nok, T1} ->
            {nok, T1}
    end.
```

#### 5.3 `erlmd_construct_thematic_break.erl`

```erlang
%% Match ***, ---, or ___ (with optional spaces)
start(T = #tokenizer{bytes = <<Char, _/binary>>})
    when Char =:= $*; Char =:= $-; Char =:= $_ ->
    check_break(T, Char, 0);
start(T) ->
    {nok, T}.

check_break(T = #tokenizer{bytes = <<Char, Rest/binary>>}, Char, Count) ->
    check_break(erlmd_tokenizer:consume(T), Char, Count + 1);
check_break(T = #tokenizer{bytes = <<$ , _/binary>>}, Char, Count) ->
    check_break(erlmd_tokenizer:consume(T), Char, Count);
check_break(T = #tokenizer{bytes = <<$\n, _/binary>>}, _Char, Count)
    when Count >= 3 ->
    T1 = erlmd_tokenizer:enter(T, thematic_break),
    {ok, erlmd_tokenizer:exit(T1)};
check_break(T, _Char, _Count) ->
    {nok, T}.
```

#### 5.4 `erlmd_construct_code_indented.erl`

**Tasks**:

- [ ] Implement paragraph construct
- [ ] Implement ATX heading construct
- [ ] Implement thematic break construct
- [ ] Implement indented code construct
- [ ] Update flow dispatcher with new constructs
- [ ] Add CommonMark tests for each construct
- [ ] Document block construct patterns

**Test Success Criteria**:

- Can parse "# Hello" → Heading(1, "Hello")
- Can parse "Hello\n\nWorld" → Paragraph, Paragraph
- Can parse "---" → ThematicBreak
- Can parse indented code (4 spaces)
- 20+ CommonMark spec tests pass

**AI Prompt Count**: 4-5 prompts

- "Implement paragraph construct"
- "Implement ATX heading construct"
- "Implement thematic break"
- "Implement indented code"

---

### Phase 6: Basic Inline Constructs (1.5 weeks)

**Goal**: Implement simple inline elements.

**Deliverables**:

1. Character escapes (\*)
2. Character references (&amp;)
3. Inline code (`code`)
4. Hard breaks

**Modules to Create**:

#### 6.1 `erlmd_construct_character_escape.erl`

```erlang
start(T = #tokenizer{bytes = <<$\\, Char, Rest/binary>>}) ->
    case is_escapable(Char) of
        true ->
            T1 = erlmd_tokenizer:enter(T, character_escape),
            T2 = erlmd_tokenizer:consume(T1),  % Consume \
            T3 = erlmd_tokenizer:consume(T2),  % Consume char
            {ok, erlmd_tokenizer:exit(T3)};
        false ->
            {nok, T}
    end;
start(T) ->
    {nok, T}.

is_escapable(Char) ->
    lists:member(Char, "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~").
```

#### 6.2 `erlmd_construct_character_reference.erl`

```erlang
%% Match &#123; or &#xAB; or &amp;
start(T = #tokenizer{bytes = <<$&, Rest/binary>>}) ->
    parse_reference(erlmd_tokenizer:consume(T));
start(T) ->
    {nok, T}.

parse_reference(T = #tokenizer{bytes = <<$#, $x, Rest/binary>>}) ->
    %% Hex numeric reference
    parse_hex_digits(erlmd_tokenizer:consume(erlmd_tokenizer:consume(T)), []);
parse_reference(T = #tokenizer{bytes = <<$#, Rest/binary>>}) ->
    %% Decimal numeric reference
    parse_decimal_digits(erlmd_tokenizer:consume(T), []);
parse_reference(T) ->
    %% Named entity reference
    parse_entity_name(T, []).

%% Implementation details...
```

#### 6.3 `erlmd_construct_code_text.erl`

```erlang
%% Match `code` or ``code``
start(T = #tokenizer{bytes = <<$`, Rest/binary>>}) ->
    count_backticks(erlmd_tokenizer:consume(T), 1);
start(T) ->
    {nok, T}.

count_backticks(T = #tokenizer{bytes = <<$`, Rest/binary>>}, N) ->
    count_backticks(erlmd_tokenizer:consume(T), N + 1);
count_backticks(T, N) ->
    T1 = erlmd_tokenizer:enter(T, code_text),
    find_closing(T1, N).

find_closing(T, OpenCount) ->
    %% Find matching backticks
    %% ...
```

**Tasks**:

- [ ] Implement character escape
- [ ] Implement character references (numeric + named)
- [ ] Implement inline code
- [ ] Implement hard break escape
- [ ] Create HTML entity lookup table
- [ ] Update text dispatcher
- [ ] Add CommonMark tests

**Test Success Criteria**:

- Can parse "hello\*world" → "hello*world"
- Can parse "&amp;" → "&"
- Can parse "`code`" → Code("code")
- 30+ more CommonMark tests pass

**AI Prompt Count**: 4-5 prompts

- "Implement character escapes"
- "Implement character references with entity table"
- "Implement inline code"
- "Implement hard breaks"

---

### Phase 7: Complex Inline (2 weeks)

**Goal**: Implement emphasis, strong, links, and images.

**Challenge**: These require delimiter matching and resolution.

**Deliverables**:

1. Emphasis/strong (*, **, _, __)
2. Links ([text](url))
3. Images (![alt](url))
4. Resolution logic for matching delimiters

**Key Algorithm**: Attention (emphasis) uses a two-pass approach:

1. First pass: Mark potential delimiters
2. Second pass (resolution): Match opening/closing delimiters

**Modules to Create**:

#### 7.1 `erlmd_construct_attention.erl`

```erlang
%% Mark * or _ as potential emphasis delimiter
start(T = #tokenizer{bytes = <<Char, Rest/binary>>})
    when Char =:= $*; Char =:= $_ ->
    count_delimiters(T, Char, 0);
start(T) ->
    {nok, T}.

count_delimiters(T = #tokenizer{bytes = <<Char, Rest/binary>>}, Char, N) ->
    count_delimiters(erlmd_tokenizer:consume(T), Char, N + 1);
count_delimiters(T, Char, N) ->
    %% Create attention marker
    T1 = erlmd_tokenizer:enter(T, attention),
    %% Store delimiter info for resolution
    {ok, erlmd_tokenizer:exit(T1)}.
```

#### 7.2 `erlmd_construct_label_start_link.erl`

```erlang
start(T = #tokenizer{bytes = <<$[, Rest/binary>>}) ->
    T1 = erlmd_tokenizer:enter(T, label_start),
    T2 = erlmd_tokenizer:consume(T1),
    parse_label_text(T2);
start(T) ->
    {nok, T}.

parse_label_text(T) ->
    %% Parse until ] found
    %% Then check for (url) or [ref]
    %% ...
```

#### 7.3 `erlmd_construct_label_end.erl`

Match closing ] and destination/reference.

#### 7.4 `erlmd_resolve.erl` - Resolution Logic

```erlang
-spec resolve(Events :: [event()]) -> [event()].
resolve(Events) ->
    resolve_attention(Events).

resolve_attention(Events) ->
    %% Algorithm:
    %% 1. Find all attention markers
    %% 2. Match opening/closing pairs (inside-out)
    %% 3. Convert to Emphasis/Strong events
    %% Reference: CommonMark spec section 6.2
    %% ...
```

**Tasks**:

- [ ] Implement attention construct (delimiter marking)
- [ ] Implement link label parsing
- [ ] Implement label_end with destination
- [ ] Implement image construct (similar to link)
- [ ] Implement attention resolver
- [ ] Implement link resolver
- [ ] Add extensive tests for edge cases
- [ ] Document resolution algorithm

**Test Success Criteria**:

- Can parse "*emphasis*" → Emphasis("emphasis")
- Can parse "**strong**" → Strong("strong")
- Can parse "[link](url)" → Link("link", "url")
- Can parse "![alt](url)" → Image("alt", "url")
- Can handle nested emphasis
- 50+ more CommonMark tests pass

**AI Prompt Count**: 5-6 prompts

- "Implement attention construct"
- "Implement link label parsing"
- "Implement link destination and reference"
- "Implement image construct"
- "Implement attention resolution algorithm"
- "Implement link resolution"

---

### Phase 8: Complex Block (2.5 weeks)

**Goal**: Implement block quotes and lists (most complex constructs).

**Deliverables**:

1. Block quotes (>)
2. Unordered lists (*, -, +)
3. Ordered lists (1.)
4. Nested list support
5. Loose/tight list handling

**Modules to Create**:

#### 8.1 `erlmd_construct_block_quote.erl`

```erlang
start(T = #tokenizer{bytes = <<$>, Rest/binary>>}) ->
    T1 = erlmd_tokenizer:enter(T, block_quote),
    T2 = erlmd_tokenizer:consume(T1),
    %% Optional space after >
    T3 = case erlmd_tokenizer:current(T2) of
        $ -> erlmd_tokenizer:consume(T2);
        _ -> T2
    end,
    parse_block_content(T3);
start(T) ->
    {nok, T}.

parse_block_content(T) ->
    %% Parse flow content inside block quote
    case erlmd_state:call(flow, T) of
        {ok, T1} ->
            case erlmd_tokenizer:current(T1) of
                eof ->
                    {ok, erlmd_tokenizer:exit(T1)};
                $\n ->
                    %% Check for continuation
                    check_continuation(erlmd_tokenizer:consume(T1));
                _ ->
                    parse_block_content(T1)
            end;
        {nok, T1} ->
            {ok, erlmd_tokenizer:exit(T1)}
    end.

check_continuation(T = #tokenizer{bytes = <<$>, _/binary>>}) ->
    parse_block_content(T);
check_continuation(T) ->
    {ok, erlmd_tokenizer:exit(T)}.
```

#### 8.2 `erlmd_construct_list_item.erl`

```erlang
start(T) ->
    case detect_marker(T) of
        {bullet, Char, T1} ->
            parse_list_item(T1, bullet, Char);
        {ordered, Number, T1} ->
            parse_list_item(T1, ordered, Number);
        nok ->
            {nok, T}
    end.

detect_marker(T = #tokenizer{bytes = <<Char, $ , Rest/binary>>})
    when Char =:= $*; Char =:= $-; Char =:= $+ ->
    {bullet, Char, erlmd_tokenizer:consume(erlmd_tokenizer:consume(T))};
detect_marker(T = #tokenizer{bytes = Bytes}) ->
    case parse_ordered_marker(Bytes) of
        {ok, Number, T1} ->
            {ordered, Number, T1};
        nok ->
            nok
    end.

parse_list_item(T, Type, Marker) ->
    T1 = erlmd_tokenizer:enter(T, list_item),
    %% Parse list item content (flow)
    parse_item_content(T1, Type, Marker).
```

**List Complexity**:

- Indentation tracking
- Lazy continuation
- Loose vs tight lists
- Nested lists
- Mixed bullet/ordered

**Tasks**:

- [ ] Implement block quote construct
- [ ] Implement list_item construct
- [ ] Implement list marker detection
- [ ] Implement list continuation logic
- [ ] Implement loose/tight list detection
- [ ] Implement nested list support
- [ ] Add comprehensive list tests
- [ ] Document list parsing algorithm

**Test Success Criteria**:

- Can parse "> quote" → BlockQuote(Paragraph("quote"))
- Can parse "* item" → List(Item("item"))
- Can parse "1. item" → OrderedList(Item("item"))
- Can handle nested lists
- Can handle loose/tight lists
- 100+ more CommonMark tests pass

**AI Prompt Count**: 5-6 prompts

- "Implement block quote construct"
- "Implement list item marker detection"
- "Implement list item content parsing"
- "Implement list continuation and nesting"
- "Implement loose/tight list logic"

---

### Phase 9: Processing Pipeline (2 weeks)

**Goal**: Implement subtokenization and resolution.

**Deliverables**:

1. Subtokenization (nested content)
2. All resolvers
3. Main parser orchestration

**Modules to Create**:

#### 9.1 `erlmd_subtokenize.erl`

```erlang
-spec subtokenize(Events :: [event()], Options :: map()) -> [event()].
subtokenize(Events, Options) ->
    process_events(Events, [], Options).

process_events([], Acc, _Options) ->
    lists:reverse(Acc);
process_events([Event | Rest], Acc, Options) ->
    case Event of
        #event{content = Content, link = Link} when Content =/= undefined ->
            %% This event contains nested content to parse
            %% Extract the slice, re-tokenize it
            NestedEvents = tokenize_slice(Link, Content, Options),
            process_events(Rest, [NestedEvents | Acc], Options);
        _ ->
            process_events(Rest, [Event | Acc], Options)
    end.
```

#### 9.2 `erlmd_resolve.erl` (expanded)

```erlang
-spec resolve(Events :: [event()]) -> [event()].
resolve(Events) ->
    Events1 = resolve_attention(Events),
    Events2 = resolve_label(Events1),
    Events3 = resolve_data(Events2),
    Events3.

%% Individual resolvers for different constructs
```

#### 9.3 `erlmd_parser.erl` - Main Orchestration

```erlang
-spec parse(Binary :: binary(), Options :: map()) -> {ok, [event()]} | {error, term()}.
parse(Binary, Options) ->
    %% 1. Initial tokenization
    Tokenizer = erlmd_tokenizer:new(Binary, Options),
    case erlmd_state:call(document, Tokenizer) of
        {ok, T1} ->
            Events = erlmd_tokenizer:events(T1),
            %% 2. Subtokenization
            Events2 = erlmd_subtokenize:subtokenize(Events, Options),
            %% 3. Resolution
            Events3 = erlmd_resolve:resolve(Events2),
            {ok, Events3};
        {error, Reason, _T} ->
            {error, Reason}
    end.
```

**Tasks**:

- [ ] Implement subtokenize for content types
- [ ] Implement all resolvers (attention, label, data)
- [ ] Implement main parser orchestration
- [ ] Add integration tests for full pipeline
- [ ] Document parsing phases
- [ ] Test edge cases with complex nesting

**Test Success Criteria**:

- Can parse documents with nested inline/block
- All resolvers work correctly
- Parser orchestration is complete
- 200+ CommonMark tests pass

**AI Prompt Count**: 4-5 prompts

- "Implement subtokenization"
- "Implement attention resolver"
- "Implement label/link resolver"
- "Implement main parser orchestration"

---

### Phase 10: Output Generation (1.5 weeks)

**Goal**: Compile events to HTML and AST.

**Deliverables**:

1. HTML compiler
2. AST (mdast) compiler
3. Public API

**Modules to Create**:

#### 10.1 `erlmd_to_html.erl`

```erlang
-spec compile(Events :: [event()], Options :: map()) -> binary().
compile(Events, Options) ->
    iolist_to_binary(compile_events(Events, [], Options)).

compile_events([], Acc, _Options) ->
    lists:reverse(Acc);
compile_events([Event | Rest], Acc, Options) ->
    HTML = event_to_html(Event, Options),
    compile_events(Rest, [HTML | Acc], Options).

event_to_html(#event{kind = enter, name = paragraph}, _Opts) ->
    <<"<p>">>;
event_to_html(#event{kind = exit, name = paragraph}, _Opts) ->
    <<"</p>">>;
event_to_html(#event{kind = enter, name = heading_atx, ...}, _Opts) ->
    <<"<h", Level, ">">>;
%% ... etc for all event types
```

**HTML Features**:

- Entity encoding
- URI sanitization
- XSS prevention
- Tag filtering (GFM)

#### 10.2 `erlmd_to_mdast.erl`

```erlang
-spec compile(Events :: [event()], Options :: map()) -> map().
compile(Events, Options) ->
    build_tree(Events, [], Options).

%% Convert flat event stream to nested tree
build_tree([], [Root], _Options) ->
    Root;
build_tree([#event{kind = enter, name = Name} | Rest], Stack, Options) ->
    Node = #{type => Name, children => []},
    build_tree(Rest, [Node | Stack], Options);
build_tree([#event{kind = exit} | Rest], [Child, Parent | Stack], Options) ->
    Parent1 = add_child(Parent, Child),
    build_tree(Rest, [Parent1 | Stack], Options).
```

#### 10.3 `erlmd.erl` - Public API

```erlang
-module(erlmd).
-export([to_html/1, to_html/2, to_mdast/1, to_mdast/2]).

-spec to_html(binary()) -> binary().
to_html(Markdown) ->
    to_html(Markdown, #{}).

-spec to_html(binary(), map()) -> binary().
to_html(Markdown, Options) ->
    case erlmd_parser:parse(Markdown, Options) of
        {ok, Events} ->
            erlmd_to_html:compile(Events, Options);
        {error, Reason} ->
            error(Reason)
    end.

-spec to_mdast(binary()) -> map().
to_mdast(Markdown) ->
    to_mdast(Markdown, #{}).

-spec to_mdast(binary(), map()) -> map().
to_mdast(Markdown, Options) ->
    case erlmd_parser:parse(Markdown, Options) of
        {ok, Events} ->
            erlmd_to_mdast:compile(Events, Options);
        {error, Reason} ->
            error(Reason)
    end.
```

**Tasks**:

- [ ] Implement HTML compiler
- [ ] Implement entity encoding
- [ ] Implement URI sanitization
- [ ] Implement AST compiler
- [ ] Implement public API
- [ ] Add HTML output tests
- [ ] Add AST output tests
- [ ] Document API usage

**Test Success Criteria**:

- Can generate correct HTML for all constructs
- HTML is properly escaped and sanitized
- AST matches mdast specification
- Public API works as documented
- 300+ CommonMark tests pass (full spec!)

**AI Prompt Count**: 3-4 prompts

- "Implement HTML compiler with entity encoding"
- "Implement URI sanitization"
- "Implement AST compiler"
- "Implement public API"

---

### Phase 11: GFM Extensions (2 weeks)

**Goal**: Add GitHub Flavored Markdown extensions.

**Deliverables**:

1. Tables
2. Strikethrough (~~text~~)
3. Autolink literals
4. Task lists (- [ ] item)
5. Footnotes

**Modules to Create**:

#### 11.1 `erlmd_construct_gfm_table.erl`

Complex: requires multiple passes for alignment, cell parsing.

#### 11.2 `erlmd_construct_gfm_strikethrough.erl`

Similar to emphasis, but with ~~ delimiter.

#### 11.3 `erlmd_construct_gfm_autolink_literal.erl`

Detect URLs and emails without angle brackets.

#### 11.4 `erlmd_construct_gfm_task_list_item_check.erl`

Detect [ ] and [x] at start of list items.

#### 11.5 `erlmd_construct_gfm_footnote_definition.erl` and `erlmd_construct_gfm_label_start_footnote.erl`

Footnote references and definitions.

**Tasks**:

- [ ] Implement table parsing (header, delimiter, rows)
- [ ] Implement strikethrough
- [ ] Implement autolink literals
- [ ] Implement task list items
- [ ] Implement footnote definitions
- [ ] Implement footnote references
- [ ] Add GFM option flag
- [ ] Add GFM test suite
- [ ] Document GFM extensions

**Test Success Criteria**:

- Can parse GFM tables correctly
- Can parse ~~strikethrough~~
- Can detect URLs automatically
- Can parse task lists
- Can parse footnotes
- GFM test suite passes

**AI Prompt Count**: 6-8 prompts

- "Implement GFM table parsing"
- "Implement GFM strikethrough"
- "Implement GFM autolink literals"
- "Implement GFM task lists"
- "Implement GFM footnote definitions"
- "Implement GFM footnote references"

---

### Phase 12: Polish & Optimization (2 weeks)

**Goal**: Production readiness.

**Deliverables**:

1. Performance optimization
2. Complete documentation
3. Test coverage > 95%
4. Benchmarks
5. Hex.pm release

**Tasks**:

- [ ] Profile parser with fprof/eflame
- [ ] Optimize hot paths
- [ ] Add property-based tests (PropEr)
- [ ] Complete EDoc documentation
- [ ] Write user guide
- [ ] Create examples
- [ ] Run full CommonMark test suite
- [ ] Run GFM test suite
- [ ] Set up CI/CD
- [ ] Publish to Hex.pm
- [ ] Write blog post/announcement

**Test Success Criteria**:

- 100% CommonMark spec compliance (649 tests)
- 100% GFM spec compliance
- Performance within 2-3x of Rust markdown-rs
- >95% code coverage
- All documentation complete

**AI Prompt Count**: 3-4 prompts

- "Profile and optimize parser performance"
- "Add property-based tests"
- "Complete documentation and examples"
- "Prepare Hex.pm release"

---

## AI Prompt Templates

### Template Structure

Each AI prompt should follow this structure:

```markdown
# Task: [Specific Goal]

## Context
- Phase: [Phase Number and Name]
- Module: [Module Name]
- Dependencies: [List of required modules]

## Reference
- Rust Implementation: [markdown-rs file path]
- Erlang Patterns: [Section from patterns doc]
- CommonMark Spec: [Section reference]

## Requirements
[Specific requirements for this module]

## Implementation Hints
[Key patterns to use]

## Test Requirements
[Expected test cases]

## Acceptance Criteria
- [ ] [Criterion 1]
- [ ] [Criterion 2]
```

### Example Prompts

#### Prompt Example 1: Phase 2 - Tokenizer

```markdown
# Task: Implement erlmd_tokenizer State Machine Driver

## Context
- Phase: 2 (Tokenizer Framework)
- Module: erlmd_tokenizer.erl
- Dependencies: erlmd_types.hrl, erlmd_util_char.erl

## Reference
- Rust Implementation: markdown-rs/src/tokenizer.rs
- Erlang Patterns: Section "State Machine Implementation Without gen_statem"
- Architecture Doc: Section 2 (Core Data Structures)

## Requirements

Implement the tokenizer record and core state machine operations:

1. **Tokenizer Record**: Track input, position, events, stack
2. **Position Tracking**: Line, column, offset (handle \n, \r\n, \r)
3. **Event Generation**: enter/exit pairs
4. **State Transitions**: consume, move_to operations
5. **Backtracking**: attempt mechanism (save/restore state)

## Implementation Hints

### Use Match Context Optimization
```erlang
%% GOOD: Preserve match context
consume(#tokenizer{bytes = <<_, Rest/binary>>} = T) ->
    T#tokenizer{bytes = Rest, ...}.

%% BAD: Creates sub-binary
consume(T) ->
    case T#tokenizer.bytes of
        <<_, Rest/binary>> -> ...
    end.
```

### Track Position Correctly

```erlang
%% Handle all line endings
update_position(T = #tokenizer{bytes = <<$\n, _/binary>>}) ->
    T#tokenizer{line = T#tokenizer.line + 1, column = 1};
update_position(T = #tokenizer{bytes = <<$\r, $\n, _/binary>>}) ->
    %% Windows CRLF - count as one line
    T#tokenizer{line = T#tokenizer.line + 1, column = 1};
update_position(T) ->
    T#tokenizer{column = T#tokenizer.column + 1}.
```

### Implement Attempt for Backtracking

```erlang
attempt(T, StateName, OnFailure) ->
    %% Save current state
    Checkpoint = save_state(T),
    case erlmd_state:call(StateName, T) of
        {ok, T1} ->
            {ok, T1};
        {nok, _} ->
            %% Restore state, then call failure state
            T2 = restore_state(Checkpoint),
            erlmd_state:call(OnFailure, T2)
    end.
```

## Test Requirements

```erlang
%% Test 1: Basic consumption
Input = <<"Hello">>,
T0 = erlmd_tokenizer:new(Input, #{}),
T1 = erlmd_tokenizer:consume(T0),
?assertEqual($H, erlmd_tokenizer:previous(T1)),
?assertEqual($e, erlmd_tokenizer:current(T1)).

%% Test 2: Position tracking
Input = <<"Line1\nLine2">>,
T0 = erlmd_tokenizer:new(Input, #{}),
T1 = consume_until_newline(T0),
?assertEqual(1, T1#tokenizer.line),
T2 = erlmd_tokenizer:consume(T1),  % Consume \n
?assertEqual(2, T2#tokenizer.line),
?assertEqual(1, T2#tokenizer.column).

%% Test 3: Event generation
T0 = erlmd_tokenizer:new(<<"test">>, #{}),
T1 = erlmd_tokenizer:enter(T0, paragraph),
T2 = erlmd_tokenizer:exit(T1),
Events = erlmd_tokenizer:events(T2),
?assertMatch([
    #event{kind = enter, name = paragraph},
    #event{kind = exit, name = paragraph}
], Events).

%% Test 4: Attempt/backtracking
T0 = erlmd_tokenizer:new(<<"test">>, #{}),
{Result, T1} = erlmd_tokenizer:attempt(T0, non_existent_state, data),
?assertEqual(nok, Result),
%% T1 should be back at start position
?assertEqual(0, T1#tokenizer.index).
```

## Acceptance Criteria

- [ ] Tokenizer record defined with all fields
- [ ] new/2 creates tokenizer from binary
- [ ] current/1 returns current byte or eof
- [ ] consume/1 advances position correctly
- [ ] Line/column tracked correctly for all line endings
- [ ] enter/exit generate event pairs
- [ ] Event stack maintains proper nesting
- [ ] attempt saves and restores state
- [ ] All unit tests pass
- [ ] No compiler warnings (bin_opt_info)
- [ ] Match context preserved in hot paths

## Additional Notes

Reference the tokenizer.rs implementation for edge cases, but remember:

- Rust uses Option<u8>, Erlang uses binary patterns
- Rust has Vec<Event>, Erlang uses lists (prepend, reverse at end)
- Attempt in Rust uses closures, in Erlang use save/restore records

```

#### Prompt Example 2: Phase 5 - Paragraph

```markdown
# Task: Implement Paragraph Construct

## Context
- Phase: 5 (Basic Block Constructs)
- Module: erlmd_construct_paragraph.erl
- Dependencies: erlmd_tokenizer, erlmd_state, erlmd_construct_text

## Reference
- Rust Implementation: markdown-rs/src/construct/paragraph.rs
- Erlang Patterns: Section "Binary Pattern Matching"
- CommonMark Spec: Section 4.8 (Paragraphs)

## Requirements

A paragraph is the default block-level construct. It:
1. Contains text content (inline constructs)
2. Ends at blank line or EOF
3. Can be interrupted by other block constructs
4. Has the LOWEST priority (tried last)

## Implementation

```erlang
-module(erlmd_construct_paragraph).
-export([start/1]).

-include("erlmd_types.hrl").

start(T) ->
    T1 = erlmd_tokenizer:enter(T, paragraph),
    parse_content(T1).

parse_content(T) ->
    %% Delegate to text content
    case erlmd_state:call(text, T) of
        {ok, T1} ->
            %% Check for continuation
            case erlmd_tokenizer:current(T1) of
                eof ->
                    {ok, erlmd_tokenizer:exit(T1)};
                Byte when Byte =:= $\n; Byte =:= $\r ->
                    %% Check if next line is blank
                    T2 = erlmd_tokenizer:consume(T1),
                    check_continuation(T2);
                _ ->
                    %% Continue parsing
                    parse_content(T1)
            end;
        {nok, T1} ->
            %% No text content found - shouldn't happen in paragraph
            {nok, erlmd_tokenizer:exit(T1)}
    end.

check_continuation(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            {ok, erlmd_tokenizer:exit(T)};
        Byte when Byte =:= $\n; Byte =:= $\r ->
            %% Blank line - end paragraph
            {ok, erlmd_tokenizer:exit(T)};
        _ ->
            %% More content - continue
            parse_content(T)
    end.
```

## Test Requirements

```erlang
%% Test 1: Simple paragraph
test_simple_paragraph() ->
    Events = test_parse("Hello world"),
    ?assertMatch([
        #event{kind = enter, name = document},
        #event{kind = enter, name = paragraph},
        #event{kind = enter, name = data, ...},
        #event{kind = exit, name = data},
        #event{kind = exit, name = paragraph},
        #event{kind = exit, name = document}
    ], Events).

%% Test 2: Multiple paragraphs
test_multiple_paragraphs() ->
    Events = test_parse("Para 1\n\nPara 2"),
    %% Should have 2 paragraph enter/exit pairs
    Paragraphs = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(4, length(Paragraphs)).  % 2 enter + 2 exit

%% Test 3: Paragraph with inline code
test_paragraph_with_inline() ->
    Events = test_parse("Hello `code` world"),
    %% Should contain code_text events inside paragraph
    ?assert(has_event(Events, code_text)).

%% CommonMark Examples
test_commonmark_example_189() ->
    Input = "aaa\n\nbbb",
    Expected = "<p>aaa</p>\n<p>bbb</p>\n",
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

## Acceptance Criteria

- [ ] Parses simple paragraphs
- [ ] Ends at blank lines
- [ ] Ends at EOF
- [ ] Contains text content
- [ ] Multiple paragraphs separated by blank lines
- [ ] CommonMark examples 189-196 pass
- [ ] No infinite loops on edge cases
- [ ] Proper event nesting (paragraph wraps text)

```

---

## Success Criteria & Testing Strategy

### Overall Success Metrics

**MVP Success (Phases 1-6)**:
- ✅ 200+ CommonMark spec tests pass
- ✅ Can parse basic documents correctly
- ✅ HTML output is correct
- ✅ No crashes on valid input

**Full Success (Phases 1-10)**:
- ✅ 649/649 CommonMark spec tests pass (100%)
- ✅ Correct HTML and AST output
- ✅ Performance: 100+ MB/s throughput
- ✅ Memory: O(n) space complexity

**Production Ready (Phases 1-12)**:
- ✅ GFM extensions working
- ✅ >95% test coverage
- ✅ Complete documentation
- ✅ Published to Hex.pm
- ✅ Performance within 2-3x of Rust

### Testing Strategy

#### 1. Unit Tests (Per Module)

Each module should have comprehensive unit tests:

```erlang
-module(erlmd_construct_heading_atx_test).
-include_lib("eunit/include/eunit.hrl").

simple_heading_test() ->
    Events = parse("# Hello"),
    ?assertMatch([
        #event{kind = enter, name = heading_atx},
        #event{kind = enter, name = heading_atx_text},
        #event{kind = enter, name = data},
        #event{kind = exit, name = data},
        #event{kind = exit, name = heading_atx_text},
        #event{kind = exit, name = heading_atx}
    ], Events).

heading_levels_test() ->
    lists:foreach(fun(Level) ->
        Input = lists:duplicate(Level, $#) ++ " Title",
        Events = parse(Input),
        %% Verify level is correct
        ?assertEqual(Level, extract_heading_level(Events))
    end, lists:seq(1, 6)).
```

#### 2. CommonMark Spec Tests

Download and integrate the official test suite:

```erlang
-module(erlmd_commonmark_test).
-include_lib("eunit/include/eunit.hrl").

%% Test runner
commonmark_spec_test_() ->
    {ok, Tests} = load_spec_tests("test/commonmark-spec.json"),
    [make_test(T) || T <- Tests].

make_test(#{
    <<"example">> := N,
    <<"markdown">> := MD,
    <<"html">> := Expected
}) ->
    Title = io_lib:format("Example ~p", [N]),
    {Title, fun() ->
        Actual = erlmd:to_html(MD),
        ?assertEqual(Expected, Actual)
    end}.

load_spec_tests(Path) ->
    {ok, Binary} = file:read_file(Path),
    {ok, Tests} = json:decode(Binary),
    {ok, Tests}.
```

**Test Progression**:

- Phase 1-3: ~10 tests
- Phase 4-5: ~50 tests
- Phase 6: ~100 tests
- Phase 7: ~200 tests
- Phase 8: ~400 tests
- Phase 9-10: ~649 tests (full spec!)

#### 3. Integration Tests

Test complete parsing pipeline:

```erlang
-module(erlmd_integration_test).
-include_lib("eunit/include/eunit.hrl").

complex_document_test() ->
    Input = "
# Title

This is a paragraph with **bold** and *italic*.

> A block quote with a [link](http://example.com).

- List item 1
- List item 2
  - Nested item

```erlang
code_block()
```

",
    HTML = erlmd:to_html(Input),
    %% Verify structure
    ?assert(string:find(HTML, "<h1>") =/= nomatch),
    ?assert(string:find(HTML, "<strong>") =/= nomatch),
    ?assert(string:find(HTML, "<blockquote>") =/= nomatch),
    ?assert(string:find(HTML, "<ul>") =/= nomatch),
    ?assert(string:find(HTML, "<code>") =/= nomatch).

```

#### 4. Property-Based Tests (Phase 12)

Use PropEr for generative testing:

```erlang
-module(erlmd_proper_test).
-include_lib("proper/include/proper.hrl").

prop_roundtrip_preserves_structure() ->
    ?FORALL(MD, markdown_generator(),
        begin
            AST = erlmd:to_mdast(MD),
            HTML = erlmd:to_html(MD),
            %% Properties:
            %% 1. HTML is valid
            is_valid_html(HTML) andalso
            %% 2. AST is well-formed
            is_valid_ast(AST)
        end).

markdown_generator() ->
    %% Generate random but valid markdown
    oneof([
        simple_paragraph(),
        heading(),
        list(),
        blockquote()
    ]).
```

#### 5. Performance Tests

Benchmark throughput:

```erlang
-module(erlmd_bench).

benchmark() ->
    Sizes = [1000, 10000, 100000, 1000000],  % bytes
    lists:foreach(fun(Size) ->
        Input = generate_markdown(Size),
        {Time, _Result} = timer:tc(fun() ->
            erlmd:to_html(Input)
        end),
        Throughput = Size / (Time / 1000000),  % bytes/sec
        io:format("~p bytes: ~.2f MB/s~n", [Size, Throughput / 1024 / 1024])
    end, Sizes).

generate_markdown(Size) ->
    %% Generate Size bytes of realistic markdown
    ...
```

**Target**: 100-150 MB/s on commodity hardware.

#### 6. Memory Tests

Check for leaks and excessive allocation:

```erlang
-module(erlmd_memory_test).

memory_usage_test() ->
    Input = binary:copy(<<"# Hello\n\nParagraph\n\n">>, 10000),
    InitialMem = erlang:memory(total),

    %% Parse many times
    lists:foreach(fun(_) ->
        _Result = erlmd:to_html(Input)
    end, lists:seq(1, 1000)),

    %% Force GC
    erlang:garbage_collect(),

    FinalMem = erlang:memory(total),
    Growth = FinalMem - InitialMem,

    %% Should not grow significantly
    ?assert(Growth < 1024 * 1024).  % Less than 1MB growth
```

### Continuous Integration

Set up GitHub Actions:

```yaml
# .github/workflows/test.yml
name: Test

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: ['26', '27']
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: '3.22'
      - run: rebar3 compile
      - run: rebar3 eunit
      - run: rebar3 ct
      - run: rebar3 dialyzer
```

---

## Risk Management

### Technical Risks

#### Risk 1: Performance Not Meeting Target

**Likelihood**: Medium
**Impact**: Medium
**Mitigation**:

- Profile early and often (Phase 5+)
- Use `fprof`, `eflame` to identify bottlenecks
- Optimize hot paths (character classification, binary matching)
- Consider NIFs for critical sections if needed (Phase 12)

#### Risk 2: Resolver Algorithm Complexity

**Likelihood**: High
**Impact**: High
**Mitigation**:

- Study markdown-rs resolver implementation carefully
- Implement attention resolver first (most complex)
- Write extensive tests for edge cases
- Reference CommonMark spec algorithms
- Budget extra time for Phase 7

#### Risk 3: List Parsing Complexity

**Likelihood**: High
**Impact**: High
**Mitigation**:

- Break into sub-tasks (marker, content, continuation, nesting)
- Implement ordered/unordered separately
- Test incrementally
- Reference CommonMark spec section 5.2 carefully
- Budget extra time for Phase 8

#### Risk 4: GFM Table Parsing

**Likelihood**: Medium
**Impact**: Low
**Mitigation**:

- Tables are complex but well-specified
- Can be implemented last (Phase 11)
- Optional feature (can skip for MVP)
- Study existing implementations

### Schedule Risks

#### Risk 5: Underestimating Phase Duration

**Likelihood**: High
**Impact**: Medium
**Mitigation**:

- Build buffer into estimates (20% contingency)
- Track actual vs estimated time
- Adjust future estimates based on actuals
- Can skip GFM (Phase 11) if needed

#### Risk 6: Scope Creep

**Likelihood**: Medium
**Impact**: Medium
**Mitigation**:

- Strict phase boundaries
- MVP-first approach (Phases 1-10)
- GFM is separate (Phase 11)
- No MDX, math, etc. in initial release
- Document future enhancements separately

### Quality Risks

#### Risk 7: Test Coverage Gaps

**Likelihood**: Medium
**Impact**: High
**Mitigation**:

- Integrate CommonMark tests from day 1
- Track pass rate per phase
- Aim for 100% spec compliance
- Add property-based tests (Phase 12)

#### Risk 8: Edge Case Bugs

**Likelihood**: High
**Impact**: Medium
**Mitigation**:

- Study CommonMark spec carefully
- Reference markdown-rs for tricky cases
- Test with real-world documents
- Fuzz testing in Phase 12

---

## Project Tracking

### Progress Dashboard

Track progress with this template:

```markdown
# erlmd Progress Dashboard

**Last Updated**: [Date]
**Current Phase**: [Phase Number]
**Overall Progress**: [X]% complete

## Phase Status

| Phase | Status | Tests Passing | Notes |
|-------|--------|---------------|-------|
| 0 | ✅ Complete | 0/0 | Setup done |
| 1 | 🟡 In Progress | 5/10 | Character utils done |
| 2 | ⚪ Not Started | 0/20 | - |
| ... | | | |

## Current Week Goals

- [ ] Goal 1
- [ ] Goal 2
- [ ] Goal 3

## Blockers

- None

## Metrics

- **CommonMark Tests**: 150/649 (23%)
- **Code Coverage**: 78%
- **Performance**: 85 MB/s
- **LOC**: 2,500

## Next Steps

1. Complete tokenizer attempt mechanism
2. Add more character classification tests
3. Begin simple constructs (blank_line)
```

### Weekly Review Template

```markdown
# Week [N] Review - [Date Range]

## Completed This Week

- ✅ Task 1
- ✅ Task 2

## Tests Added

- X new unit tests
- Y CommonMark tests now passing

## Challenges Encountered

- Challenge 1: [Description]
  - Resolution: [How solved]

## Next Week Plan

- [ ] Task 1
- [ ] Task 2

## Estimate Accuracy

- Estimated: X hours
- Actual: Y hours
- Variance: Z%
```

---

## Conclusion

This implementation plan provides:

1. ✅ **Clear phases** with 1-2 week durations
2. ✅ **Specific deliverables** for each phase
3. ✅ **Testable outcomes** to validate progress
4. ✅ **AI prompt guidance** for implementation
5. ✅ **Risk mitigation** strategies
6. ✅ **Success criteria** at multiple milestones

### Key Success Factors

1. **Follow the phases strictly** - Don't skip ahead
2. **Test continuously** - Integrate CommonMark tests early
3. **Reference markdown-rs** - It's battle-tested
4. **Use Erlang patterns** - Binary matching, tail recursion
5. **Profile early** - Don't wait until Phase 12 to optimize
6. **Document thoroughly** - Future maintainers will thank you

### Estimated Timeline

- **Aggressive**: 16 weeks (full-time)
- **Realistic**: 20 weeks (80% time)
- **Conservative**: 24 weeks (with buffer)

**Total Estimated Effort**: 400-600 hours

### Next Steps

1. Review this plan
2. Set up project (Phase 0)
3. Begin Phase 1 (Foundation Types)
4. Start tracking progress
5. Create first AI prompt for core types

Good luck! This is a substantial but achievable project. The architecture is sound, the reference implementation is solid, and Erlang is well-suited to this problem domain. 🚀
