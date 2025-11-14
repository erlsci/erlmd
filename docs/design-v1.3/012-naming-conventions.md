# Erlmd Naming Conventions

**Project**: erlmd (Erlang Markdown Parser)
**Document**: Naming Conventions and Standards
**Date**: November 14, 2025
**Version**: 1.1
**Purpose**: Establish consistent naming patterns for all future development

---

## Table of Contents

1. [Overview](#overview)
2. [Module Naming](#module-naming)
3. [Function Naming](#function-naming)
4. [Record and Type Naming](#record-and-type-naming)
5. [File Organization](#file-organization)
6. [Quick Reference](#quick-reference)
7. [Examples](#examples)
8. [Rationale](#rationale)
9. [Common Confusions](#common-confusions)
10. [Complete Module List](#complete-module-list)

---

## Overview

### Guiding Principles

The erlmd project uses **abbreviated naming conventions** to achieve:

1. **Brevity**: Shorter names reduce cognitive load
2. **Consistency**: Predictable patterns aid navigation
3. **Clarity**: Abbreviations are standard and unambiguous
4. **Searchability**: Unique prefixes enable quick file finding

### Core Abbreviations

| Full Term | Abbreviation | Usage |
|-----------|--------------|-------|
| construct | `cnstr` | Markdown parsing constructs |
| partial | `prtl` | Partial/incomplete constructs |
| private/internal | `pvt` | Private implementation modules |
| utility | `util` | Helper/utility functions |
| character | `char` | Character operations |
| reference | `ref` | References (character entities) |
| identifier | `id` | Identifiers |
| constant | `consts` | Constants header file |
| types | `types` | Types header file |
| continuation | `cont` | Continuation (in long names only) |

---

## Module Naming

### 1. Construct Modules

**Pattern**: `erlmd_cnstr_<construct_name>`

Construct modules implement markdown parsing constructs as defined in the CommonMark spec and extensions.

#### Regular Constructs - Block Level

```erlang
% Core block constructs
erlmd_cnstr_paragraph          % Paragraphs
erlmd_cnstr_heading_atx        % ATX headings (# syntax)
erlmd_cnstr_heading_setext     % Setext headings (underline syntax)
erlmd_cnstr_thematic_break     % Horizontal rules (---, ***, ___)
erlmd_cnstr_code_fenced        % Fenced code blocks (```)
erlmd_cnstr_code_indented      % Indented code blocks
erlmd_cnstr_html_flow          % Block-level HTML
erlmd_cnstr_block_quote        % Block quotes (>)
erlmd_cnstr_list_item          % List items
erlmd_cnstr_blank_line         % Blank lines (NOT partial)
erlmd_cnstr_definition         % Link reference definitions
```

#### Regular Constructs - Inline Level

```erlang
% Inline constructs
erlmd_cnstr_attention          % Emphasis (*) and strong (**)
erlmd_cnstr_autolink           % Automatic links <http://...>
erlmd_cnstr_character_escape   % Backslash escapes (\*)
erlmd_cnstr_character_reference % HTML entities (&amp;)
erlmd_cnstr_code_text          % Inline code (`code`)
erlmd_cnstr_hard_break_escape  % Hard line breaks (backslash + newline)
erlmd_cnstr_html_text          % Inline HTML tags
erlmd_cnstr_label_end          % Link/image endings (])
erlmd_cnstr_label_start_link   % Link starts ([)
erlmd_cnstr_label_start_image  % Image starts (![)
```

#### Partial Constructs

**Pattern**: `erlmd_cnstr_prtl_<name>`

Partial constructs are reusable parsing components that don't correspond to complete markdown elements. They are building blocks used by other constructs.

```erlang
% Data and whitespace
erlmd_cnstr_prtl_data          % Plain text data
erlmd_cnstr_prtl_space_or_tab  % Space or tab characters
erlmd_cnstr_prtl_whitespace    % Whitespace handling

% Link/image components
erlmd_cnstr_prtl_label         % Link label parsing
erlmd_cnstr_prtl_title         % Link/image title parsing
erlmd_cnstr_prtl_destination   % Link destination parsing

% Other partial constructs
erlmd_cnstr_prtl_line_ending   % Line ending normalization
erlmd_cnstr_prtl_non_lazy_cont % Non-lazy continuation lines
```

**Note**: `blank_line` is a regular construct, NOT a partial construct. It appears in markdown-rs as `construct/blank_line.rs`, not `construct/partial_blank_line.rs`.

#### Content Type Dispatchers

**Pattern**: `erlmd_cnstr_<content_type>`

Content type dispatchers orchestrate which constructs are valid at each parsing level. These ARE constructs (they implement the construct pattern), but serve a special dispatching role.

```erlang
erlmd_cnstr_document       % Top-level document content
erlmd_cnstr_flow           % Block-level content
erlmd_cnstr_text           % Inline text content
erlmd_cnstr_string         % String content (limited inline)
```

**Why `erlmd_cnstr_*` prefix?**: These modules implement the construct pattern from markdown-rs (they live in the `construct/` directory in the Rust implementation) and follow the same state machine architecture as other constructs.

---

### 2. Core Framework Modules

**Pattern**: `erlmd_<component>`

Core modules that provide fundamental parsing infrastructure:

```erlang
erlmd                      % Main public API
erlmd_tokenizer           % State machine driver
erlmd_state               % State dispatcher
erlmd_event               % Event types and utilities
erlmd_html                % HTML output generator
erlmd_ast                 % AST output generator
erlmd_subtokenize         % Nested content handler
erlmd_resolve             % Post-processing resolver
erlmd_parser              % Parse orchestration (if separate from API)
```

**Important**: Output modules are `erlmd_html` and `erlmd_ast`, NOT `erlmd_to_html` or `erlmd_to_mdast`. The shorter names follow the core framework pattern and are more concise.

---

### 3. Utility Modules

**Pattern**: `erlmd_util_<utility_name>`

Utility modules provide helper functions and common operations:

```erlang
erlmd_util_char              % Character classification
erlmd_util_char_ref          % Character reference decoding
erlmd_util_const             % Constants
erlmd_util_position          % Position tracking
erlmd_util_normalize_id      % Identifier normalization
erlmd_util_sanitize_uri      % URI sanitization
erlmd_util_encode            % HTML/URL encoding
```

**Note**: `erlmd_util_const` is the correct name (not `constant`). While we generally prefer full words, `const` is a universal programming term and saves characters.

---

### 4. Private/Internal Modules

**Pattern**: `erlmd_pvt_<component>`

Private modules contain internal implementation details not part of the public API:

```erlang
erlmd_pvt_tokenizer         % Tokenizer internals
erlmd_pvt_parser            % Parser internals
erlmd_pvt_event_helpers     % Event manipulation helpers
```

**Important**:
- ❌ Never use `*_internal` suffix
- ✅ Always use `erlmd_pvt_*` prefix
- These modules are for internal use only and may change between versions

---

### 5. Extension Modules

**Pattern**: `erlmd_ext_<extension_name>`

Extension modules implement markdown extensions (GFM, MDX, etc.):

```erlang
% GitHub Flavored Markdown
erlmd_ext_gfm_table                  % Tables
erlmd_ext_gfm_task_list              % Task list items [x]
erlmd_ext_gfm_strikethrough          % Strikethrough ~~text~~
erlmd_ext_gfm_autolink_literal       % Automatic URL detection
erlmd_ext_gfm_footnote_definition    % Footnote definitions
erlmd_ext_gfm_label_start_footnote   % Footnote references

% MDX (future)
erlmd_ext_mdx_jsx                    % JSX syntax
erlmd_ext_mdx_expression             % Expression syntax

% Other extensions (future)
erlmd_ext_frontmatter                % YAML/TOML frontmatter
erlmd_ext_math                       % Math blocks
```

---

## Function Naming

### 1. Construct Entry Points

All construct modules should export consistent entry point functions:

```erlang
% Primary entry point
start/1              % Start parsing this construct

% State functions (named states)
inside/1            % Inside the construct
after/1             % After a sub-construct
before/1            % Before a sub-construct
at_break/1          % At a break point
continuation/1      % Continuation line

% Post-processing
resolve/1           % Resolve/merge events
```

### 2. Tokenizer Operations

Tokenizer functions use imperative verbs:

```erlang
consume/1           % Consume current byte
enter/2             % Enter a construct (emit enter event)
exit/1              % Exit a construct (emit exit event)
attempt/3           % Try parsing with backtracking
current/1           % Get current byte
peek/2              % Look ahead N bytes
```

### 3. Utility Functions

Utility functions use descriptive names:

```erlang
% Character classification (is_*)
is_whitespace/1
is_alpha/1
is_digit/1
is_hex_digit/1

% Character operations
to_lower/1
to_upper/1
normalize/1

% Position operations
advance/2
retreat/2
offset_to_position/2
```

### 4. Boolean Functions

Boolean functions should start with `is_`, `has_`, or `can_`:

```erlang
is_whitespace/1
has_markers/1
can_interrupt/2
is_blank_line/1
```

---

## Record and Type Naming

### 1. Record Names

Records use lowercase snake_case and are typically defined in header files:

```erlang
% Core records (erlmd_types.hrl)
#event{}            % Parser event
#point{}            % Position in input
#link{}             % Link/image metadata
#tokenizer{}        % Tokenizer state
#token{}            % Token data

% Extension records
#gfm_table{}
#mdx_jsx{}
```

### 2. Type Names

Types mirror record names but can include common Erlang conventions:

```erlang
-type event() :: #event{}.
-type point() :: #point{}.
-type tokenizer() :: #tokenizer{}.
-type state_result() :: ok | nok | {next, atom()} | {retry, atom()}.
-type event_kind() :: enter | exit.
-type content_type() :: flow | document | text | string.
```

### 3. Event Names (Atoms)

Event names use lowercase snake_case and match construct names (without prefixes):

```erlang
% Block events
paragraph
heading_atx
heading_setext
thematic_break
block_quote
list_item

% Inline events
emphasis
strong
code_text
link
image

% Partial events
data
line_ending
space_or_tab
```

---

## File Organization

### Directory Structure

```
erlmd/
├── src/
│   ├── erlmd.erl                    % Main API
│   ├── erlmd_*.erl                  % Core modules
│   ├── erlmd_cnstr_*.erl            % Construct modules
│   ├── erlmd_cnstr_prtl_*.erl       % Partial constructs
│   ├── erlmd_util_*.erl             % Utilities
│   ├── erlmd_pvt_*.erl              % Private modules
│   └── erlmd_ext_*.erl              % Extensions
│   ├── types.hrl                    % Type definitions
│   ├── consts.hrl                   % Constants
│   └── macros.hrl                   % Macros
└── test/
    ├── erlmd_*_test.erl             % Unit tests (match src/)
    ├── erlmd_integration_test.erl   % Integration tests
    └── test_helper.erl              % Test utilities
```

### File Naming Rules

1. **Source files**: Match module name exactly
   - Module: `erlmd_cnstr_paragraph`
   - File: `src/erlmd_cnstr_paragraph.erl`

2. **Test files**: Append `_test` to module name
   - Module: `erlmd_cnstr_prtl_data_test`
   - File: `test/erlmd_cnstr_prtl_data_test.erl`

3. **Header files**: Use `.hrl` extension and put header files used by the library in `./src` (`./include` is best used for header files that are meant to be used by other libraries)
   - `types.hrl`
   - `consts.hrl`

---

## Quick Reference

### Module Prefix Guide

| Prefix | Type | Example |
|--------|------|---------|
| `erlmd_cnstr_` | Regular construct | `erlmd_cnstr_paragraph` |
| `erlmd_cnstr_prtl_` | Partial construct | `erlmd_cnstr_prtl_data` |
| `erlmd_util_` | Utility | `erlmd_util_char` |
| `erlmd_pvt_` | Private/internal | `erlmd_pvt_tokenizer` |
| `erlmd_ext_` | Extension | `erlmd_ext_gfm_table` |
| `erlmd_` | Core framework | `erlmd_tokenizer` |

### Common Function Names

| Function | Purpose |
|----------|---------|
| `start/1` | Construct entry point |
| `inside/1` | Inside construct state |
| `after/1` | After sub-construct state |
| `resolve/1` | Post-processing |
| `consume/1` | Consume byte |
| `enter/2` | Enter construct |
| `exit/1` | Exit construct |

---

## Examples

### Example 1: Block Construct

**Rust reference**: `markdown-rs/src/construct/paragraph.rs`

**Erlang module**: `erlmd_cnstr_paragraph.erl`

```erlang
-module(erlmd_cnstr_paragraph).

-export([start/1, inside/1, continuation/1]).
-export([resolve/1]).

-include("erlmd_types.hrl").

%% Entry point
start(T) -> ...

%% State functions
inside(T) -> ...
continuation(T) -> ...

%% Post-processing
resolve(T) -> ...
```

### Example 2: Partial Construct

**Rust reference**: `markdown-rs/src/construct/partial_space_or_tab.rs`

**Erlang module**: `erlmd_cnstr_prtl_space_or_tab.erl`

```erlang
-module(erlmd_cnstr_prtl_space_or_tab).

-export([space_or_tab/1, space_or_tab_min_max/3]).
-export([start/1, inside/1, after_space_or_tab/1]).

-include("erlmd_types.hrl").

%% Public API
space_or_tab(T) -> ...
space_or_tab_min_max(T, Min, Max) -> ...

%% State functions
start(T) -> ...
inside(T) -> ...
after_space_or_tab(T) -> ...
```

### Example 3: Utility Module

**Erlang module**: `erlmd_util_char.erl`

```erlang
-module(erlmd_util_char).

-export([
    is_whitespace/1,
    is_alpha/1,
    is_digit/1,
    is_hex_digit/1,
    is_ascii_control/1,
    is_ascii_punctuation/1
]).

is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace(_) -> false.

% ... more functions
```

### Example 4: Output Module

**Erlang module**: `erlmd_html.erl`

```erlang
-module(erlmd_html).

-export([compile/1, compile/2]).

-include("types.hrl").

%% Convert event stream to HTML
compile(Events) ->
    compile(Events, #{}).

compile(Events, Options) ->
    % Process events and generate HTML
    process_events(Events, Options, []).

process_events([], _Options, Acc) ->
    lists:reverse(Acc);
process_events([Event | Rest], Options, Acc) ->
    % ... convert events to HTML strings
    process_events(Rest, Options, [html_for_event(Event) | Acc]).
```

---

## Rationale

### Why Abbreviated Names?

1. **Cognitive Load**:
   - ❌ `erlmd_construct_partial_space_or_tab` (43 characters)
   - ✅ `erlmd_cnstr_prtl_space_or_tab` (32 characters)
   - 25% reduction while maintaining clarity

2. **Screen Real Estate**:
   - Shorter names fit better in terminals, editors, and diffs
   - Reduces horizontal scrolling

3. **Typing Efficiency**:
   - Less typing for frequent operations
   - Autocomplete works better with shorter prefixes

4. **Consistency with Erlang/OTP**:
   - Erlang conventions favor short names (e.g., `gen_server`, `proc_lib`)
   - Our abbreviations align with this philosophy

### Why These Specific Abbreviations?

- **`cnstr`**: Unambiguous abbreviation for "construct"
- **`prtl`**: Common abbreviation for "partial" in CS literature
- **`pvt`**: Clear abbreviation for "private" (avoids `priv` which is a directory name)
- **`util`**: Standard abbreviation in programming
- **`char`**: Universal abbreviation in programming
- **`const`**: Universal programming term (shorter than `constant`)
- **`cont`**: Standard abbreviation for continuation (used only in very long names)

### Why Consistency Matters?

1. **Predictability**: Developers can guess module names without looking them up
2. **Navigation**: File completion works better with consistent prefixes
3. **Maintenance**: Refactoring is easier with predictable patterns
4. **Documentation**: Easier to generate and maintain docs

---

## Common Confusions

### 1. Output Module Names

**❓ Question**: Should I use `erlmd_to_html` or `erlmd_html`?

**✅ Answer**: Use `erlmd_html` and `erlmd_ast`

These are core framework modules, not utilities or constructs, so they follow the pattern `erlmd_<component>`.

---

### 2. Blank Line: Partial or Regular?

**❓ Question**: Is `blank_line` a partial construct?

**✅ Answer**: NO. `blank_line` is a **regular construct**.

```erlang
% Correct
erlmd_cnstr_blank_line        % Regular construct

% Wrong - does not exist
erlmd_cnstr_prtl_blank_line   % This does not exist!
```

In markdown-rs, it lives at `construct/blank_line.rs`, not `construct/partial_blank_line.rs`.

---

### 3. HTML Construct Names

**❓ Question**: Should I use `html_block` or `html_flow`?

**✅ Answer**: Use `html_flow` and `html_text` to match content type terminology.

```erlang
% Correct - matches content types (flow, text, string, document)
erlmd_cnstr_html_flow         % Block-level HTML
erlmd_cnstr_html_text         % Inline HTML

% Wrong - inconsistent with content type naming
erlmd_cnstr_html_block        % Don't use this
```

---

### 4. Constants Module

**❓ Question**: Should it be `const` or `constant`?

**✅ Answer**: Use `erlmd_util_const`

While we generally prefer full words, `const` is universally recognized in programming and is the standard abbreviation.

---

### 5. Content Dispatchers: Are They Constructs?

**❓ Question**: Why do `document`, `flow`, `text`, and `string` have the `erlmd_cnstr_` prefix?

**✅ Answer**: They ARE constructs—they implement the construct pattern.

In markdown-rs, these live in the `construct/` directory:
- `construct/document.rs`
- `construct/flow.rs`
- `construct/text.rs`
- `construct/string.rs`

They follow the same state machine architecture as other constructs but serve a special dispatching role.

---

### 6. When to Abbreviate `continuation`?

**❓ Question**: Should I abbreviate `continuation` to `cont`?

**✅ Answer**: Yes!

```erlang
% Use full word in most cases
erlmd_cnstr_list_item_continuation    % OK - 34 characters

% Abbreviate only when name would be too long
erlmd_cnstr_prtl_non_lazy_cont        % Good - 34 characters
% instead of
erlmd_cnstr_prtl_non_lazy_continuation % Would be 42 characters
```

---

## Anti-Patterns to Avoid

### ❌ DON'T: Use Old Naming Patterns

```erlang
% Bad - old convention (from before v1.1)
erlmd_construct_paragraph
erlmd_construct_partial_data
erlmd_partial_data
erlmd_to_html
erlmd_to_mdast

% Good - current convention
erlmd_cnstr_paragraph
erlmd_cnstr_prtl_data
erlmd_html
erlmd_ast
```

### ❌ DON'T: Use `_internal` Suffix

```erlang
% Bad - doesn't follow our convention
tokenizer_internal
parser_internal

% Good - use pvt prefix
erlmd_pvt_tokenizer
erlmd_pvt_parser
```

### ❌ DON'T: Over-Abbreviate

```erlang
% Bad - too cryptic
erlmd_c_p_d      % What does this mean?
erlmd_u_c        % Unclear

% Good - clear abbreviations
erlmd_cnstr_prtl_data
erlmd_util_char
```

### ❌ DON'T: Mix Conventions

```erlang
% Bad - inconsistent
erlmd_construct_paragraph   % Old convention
erlmd_cnstr_heading        % New convention
erlmd_partial_data         % No prefix

% Good - consistent
erlmd_cnstr_paragraph
erlmd_cnstr_heading
erlmd_cnstr_prtl_data
```

### ✅ DO: Use Standard Conventions

```erlang
% Good - follows our conventions
erlmd_cnstr_prtl_data
erlmd_cnstr_paragraph
erlmd_util_char
erlmd_pvt_tokenizer
erlmd_html
erlmd_ast
```

---

## Complete Module List

### Core Framework (8 modules)

```erlang
erlmd                      % Main public API
erlmd_parser               % Parse orchestration
erlmd_tokenizer            % State machine driver
erlmd_state                % State dispatcher
erlmd_event                % Event types and utilities
erlmd_html                 % HTML output generator
erlmd_ast                  % AST output generator
erlmd_subtokenize          % Nested content handler
erlmd_resolve              % Post-processing resolver
```

### Content Dispatchers (4 modules)

```erlang
erlmd_cnstr_document       % Top-level document content
erlmd_cnstr_flow           % Block-level content
erlmd_cnstr_text           % Inline text content
erlmd_cnstr_string         % String content (limited inline)
```

### Block Constructs (11 modules)

```erlang
erlmd_cnstr_paragraph
erlmd_cnstr_heading_atx
erlmd_cnstr_heading_setext
erlmd_cnstr_thematic_break
erlmd_cnstr_code_fenced
erlmd_cnstr_code_indented
erlmd_cnstr_html_flow
erlmd_cnstr_block_quote
erlmd_cnstr_list_item
erlmd_cnstr_blank_line
erlmd_cnstr_definition
```

### Inline Constructs (10 modules)

```erlang
erlmd_cnstr_attention
erlmd_cnstr_autolink
erlmd_cnstr_character_escape
erlmd_cnstr_character_reference
erlmd_cnstr_code_text
erlmd_cnstr_hard_break_escape
erlmd_cnstr_html_text
erlmd_cnstr_label_end
erlmd_cnstr_label_start_link
erlmd_cnstr_label_start_image
```

### Partial Constructs (8 modules)

```erlang
erlmd_cnstr_prtl_data
erlmd_cnstr_prtl_space_or_tab
erlmd_cnstr_prtl_whitespace
erlmd_cnstr_prtl_label
erlmd_cnstr_prtl_title
erlmd_cnstr_prtl_destination
erlmd_cnstr_prtl_line_ending
erlmd_cnstr_prtl_non_lazy_cont
```

### Utility Modules (7 modules)

```erlang
erlmd_util_char
erlmd_util_char_ref
erlmd_util_const
erlmd_util_normalize_id
erlmd_util_sanitize_uri
erlmd_util_encode
erlmd_util_position
```

### GFM Extensions (6 modules)

```erlang
erlmd_ext_gfm_table
erlmd_ext_gfm_strikethrough
erlmd_ext_gfm_autolink_literal
erlmd_ext_gfm_task_list
erlmd_ext_gfm_footnote_definition
erlmd_ext_gfm_label_start_footnote
```

### Private Modules (as needed)

```erlang
erlmd_pvt_tokenizer
erlmd_pvt_parser
erlmd_pvt_event_helpers
% ... other internal modules as needed
```

**Total Core Modules**: 54 modules (before GFM extensions)

---

## Migration Guide

### Migrating from Old Naming (Pre-v1.1)

If you encounter old naming conventions in existing code or documentation:

1. **Identify**: Use grep to find old patterns
   ```bash
   grep -r "erlmd_construct_" .
   grep -r "erlmd_to_html\|erlmd_to_mdast" .
   grep -r "_internal" .
   ```

2. **Map**: Use this conversion table

   | Old Name | New Name |
   |----------|----------|
   | `erlmd_construct_*` | `erlmd_cnstr_*` |
   | `erlmd_construct_partial_*` | `erlmd_cnstr_prtl_*` |
   | `erlmd_to_html` | `erlmd_html` |
   | `erlmd_to_mdast` | `erlmd_ast` |
   | `erlmd_util_character_reference` | `erlmd_util_char_ref` |
   | `erlmd_util_normalize_identifier` | `erlmd_util_normalize_id` |
   | `erlmd_cnstr_html_block` | `erlmd_cnstr_html_flow` |
   | `*_internal` | `erlmd_pvt_*` |

3. **Rename**: Use git mv to preserve history
   ```bash
   git mv src/old_name.erl src/new_name.erl
   ```

4. **Update**: Fix module directives and references

5. **Test**: Ensure all tests pass

6. **Document**: Update any local documentation

---

## Future Considerations

### When Adding New Module Types

If a new module category is needed:

1. Choose a clear, short abbreviation (3-5 characters)
2. Ensure it doesn't conflict with existing prefixes
3. Document it in this guide
4. Apply consistently across all new modules

### Potential Future Categories

- **Validators**: `erlmd_val_*` (for input validation)
- **Transformers**: `erlmd_xfrm_*` (for AST transformations)
- **Analyzers**: `erlmd_anlz_*` (for static analysis)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-11-14 | Initial version - established naming conventions |
| 1.1 | 2025-11-14 | Clarifications: output modules, html_flow vs html_block, blank_line classification, const vs constant, continuation abbreviation, added Common Confusions section, added Complete Module List |

---

## References

- [Erlang Naming Conventions](https://www.erlang.org/doc/reference_manual/modules.html)
- [CommonMark Specification](https://spec.commonmark.org/)
- [markdown-rs Source Code](https://github.com/wooorm/markdown-rs)
- erlmd Design Documents (002, 007, implementation-plan)

---

*Document created: November 14, 2025*
*Document updated: November 14, 2025 (v1.1)*
*Purpose: Establish consistent naming for all erlmd development*
*Status: Active standard*
