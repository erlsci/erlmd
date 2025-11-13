# erlmd AI Prompt Quick Reference

**Purpose**: Templates for generating focused AI prompts for each implementation phase

---

## How to Use This Guide

For each phase/task, use this format:

1. **Copy the relevant template below**
2. **Fill in specific details** from the main implementation plan
3. **Add context** from the Rust API docs (006-erlmd-library-rewrite.md)
4. **Reference patterns** from the Erlang patterns doc (002-rewrite-research...)
5. **Submit to Claude** in a focused conversation

---

## Standard Prompt Template

```markdown
# Task: [Specific Module/Feature]

## Context
- **Phase**: [Phase Number and Name]
- **Module**: `[module_name.erl]`
- **Dependencies**: [List required modules]
- **Priority**: [High/Medium/Low]

## Reference Materials
- **Rust Implementation**: `markdown-rs/src/[relevant_file].rs`
- **Erlang Patterns**: Section "[Pattern Name]" in implementation patterns doc
- **CommonMark Spec**: Section [X.Y] - [Topic]
- **Architecture Doc**: Section [N] in 006-erlmd-library-rewrite.md

## Requirements

[3-5 bullet points of what this module must do]

## Implementation Details

### Record/Type Definitions
```erlang
[Relevant record definitions]
```

### Key Functions

```erlang
[Function signatures with -spec types]
```

### Algorithm Notes

[Any specific algorithms or patterns to use]

## Code Examples

### Pattern to Follow

```erlang
[Good example showing optimal pattern]
```

### Pattern to Avoid

```erlang
[Anti-pattern to avoid]
```

## Test Requirements

```erlang
[3-5 specific test cases]
```

## Acceptance Criteria

- [ ] [Criterion 1]
- [ ] [Criterion 2]
- [ ] [Criterion 3]
- [ ] All tests pass
- [ ] No compiler warnings
- [ ] Match context preserved (use bin_opt_info)

## Additional Notes

[Any edge cases, gotchas, or special considerations]

```

---

## Phase-Specific Templates

### Phase 0: Project Setup

```markdown
# Task: Initialize erlmd Project Structure

## Context
- **Phase**: 0 (Project Setup)
- **Duration**: 2-3 days
- **Goal**: Working rebar3 project with test infrastructure

## Tasks

1. Create new rebar3 library:
   ```bash
   rebar3 new lib erlmd
   ```

2. Configure rebar.config:

   ```erlang
   {erl_opts, [
       debug_info,
       bin_opt_info,
       warnings_as_errors,
       {i, "include"}
   ]}.

   {minimum_otp_vsn, "26"}.

   {deps, []}.

   {plugins, [rebar3_proper]}.

   {profiles, [
       {test, [
           {deps, [proper]}
       ]}
   ]}.
   ```

3. Create directory structure:

   ```
   erlmd/
   ├── src/
   │   ├── erlmd.erl
   │   ├── erlmd.app.src
   │   ├── erlmd_cnstr_*    % construct modules
   │   └── erlmd_util_*     % utility modules
   ├── include/
   │   └── types.hrl
   ├── test/
   │   ├── commonmark_spec/
   │   └── unit/
   └── rebar.config
   ```

4. Create initial header file (include/types.hrl):

   ```erlang
   -ifndef(ERLMD_TYPES_HRL).
   -define(ERLMD_TYPES_HRL, true).

   %% Placeholder - will be filled in Phase 1

   -endif.
   ```

5. Create stub public API (src/erlmd.erl):

   ```erlang
   -module(erlmd).
   -export([to_html/1, to_html/2]).

   to_html(Markdown) ->
       to_html(Markdown, #{}).

   to_html(_Markdown, _Options) ->
       error(not_implemented).
   ```

6. Download CommonMark test suite:
   - URL: <https://spec.commonmark.org/current/spec.json>
   - Save to: test/commonmark_spec/spec.json

7. Create test runner stub:

   ```erlang
   -module(erlmd_commonmark_test).
   -include_lib("eunit/include/eunit.hrl").

   load_tests_test() ->
       %% Just verify we can load the spec file
       Path = "test/commonmark_spec/spec.json",
       {ok, _Binary} = file:read_file(Path).
   ```

## Acceptance Criteria

- [ ] `rebar3 compile` succeeds
- [ ] `rebar3 eunit` runs (0 tests pass is OK)
- [ ] Directory structure created
- [ ] CommonMark spec downloaded
- [ ] Git repository initialized
- [ ] README.md exists with project description

## Deliverables

- Working rebar3 project
- Test infrastructure in place
- Ready for Phase 1 implementation

```

### Phase 1: Core Types

```markdown
# Task: Implement Core Record Types and Character Utilities

## Context
- **Phase**: 1 (Foundation Types)
- **Modules**:
  - `include/types.hrl`
  - `src/erlmd_util_char.erl`
  - `src/erlmd_util_const.erl`

## Reference
- **Architecture Doc**: Section 2 (Core Data Structures)
- **Rust Code**: `markdown-rs/src/event.rs`, `src/util/char.rs`
- **Erlang Patterns**: Section "Binary Pattern Matching"

## Part 1: Core Records (erlmd_types.hrl)

Define these records:

```erlang
-record(event, {
    kind :: enter | exit,
    name :: atom(),  % paragraph, heading_atx, emphasis, etc.
    point :: point(),
    link = undefined :: link() | undefined,
    content = undefined :: content_type() | undefined
}).

-record(point, {
    line = 1 :: pos_integer(),       % 1-indexed
    column = 1 :: pos_integer(),     % 1-indexed
    offset = 0 :: non_neg_integer()  % 0-indexed byte offset
}).

-record(position, {
    start :: point(),
    end :: point()
}).

-record(link, {
    previous :: non_neg_integer(),
    next = undefined :: non_neg_integer() | undefined,
    content :: content_type()
}).

-type content_type() :: string | text | flow | document.

-record(message, {
    reason :: atom() | binary(),
    position :: position() | undefined,
    source :: binary() | undefined
}).

-type state_result() :: ok | nok | {next, atom()} | {retry, atom()} | {error, term()}.
```

## Part 2: Character Classification (erlmd_util_char.erl)

Implement these functions:

```erlang
-module(erlmd_util_char).
-export([
    is_whitespace/1,
    is_ascii_alpha/1,
    is_ascii_alphanumeric/1,
    is_ascii_digit/1,
    is_ascii_punctuation/1,
    is_ascii_control/1,
    is_unicode_whitespace/1,
    is_unicode_punctuation/1
]).

%% ASCII checks (fast path - most common)
-spec is_whitespace(byte()) -> boolean().
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace($\v) -> true;
is_whitespace($\f) -> true;
is_whitespace(_) -> false.

-spec is_ascii_alpha(byte()) -> boolean().
is_ascii_alpha(C) when C >= $a, C =< $z -> true;
is_ascii_alpha(C) when C >= $A, C =< $Z -> true;
is_ascii_alpha(_) -> false.

-spec is_ascii_digit(byte()) -> boolean().
is_ascii_digit(C) when C >= $0, C =< $9 -> true;
is_ascii_digit(_) -> false.

-spec is_ascii_alphanumeric(byte()) -> boolean().
is_ascii_alphanumeric(C) ->
    is_ascii_alpha(C) orelse is_ascii_digit(C).

-spec is_ascii_punctuation(byte()) -> boolean().
is_ascii_punctuation(C) ->
    (C >= $! andalso C =< $/) orelse
    (C >= $: andalso C =< $@) orelse
    (C >= $[ andalso C =< $`) orelse
    (C >= ${ andalso C =< $~).

-spec is_ascii_control(byte()) -> boolean().
is_ascii_control(C) when C >= 0, C =< 31 -> true;
is_ascii_control(127) -> true;
is_ascii_control(_) -> false.

%% Unicode checks (slower - for extended characters)
-spec is_unicode_whitespace(binary()) -> boolean().
is_unicode_whitespace(<<C/utf8>>) ->
    %% Unicode whitespace categories: Zs, Zl, Zp
    %% For now, delegate to ASCII for MVP
    is_whitespace(C).

-spec is_unicode_punctuation(binary()) -> boolean().
is_unicode_punctuation(<<C/utf8>>) ->
    %% Unicode punctuation categories: Pc, Pd, Pe, Pf, Pi, Po, Ps
    %% For now, delegate to ASCII for MVP
    is_ascii_punctuation(C).
```

## Part 3: Constants (erlmd_util_const.erl)

```erlang
-module(erlmd_util_constant).
-export([
    tab_size/0,
    code_indent_size/0,
    list_item_prefix_size_max/0,
    heading_atx_opening_fence_size_max/0
]).

-define(TAB_SIZE, 4).
-define(CODE_INDENT_SIZE, 4).
-define(LIST_ITEM_PREFIX_SIZE_MAX, 4).
-define(HEADING_ATX_OPENING_FENCE_SIZE_MAX, 6).

tab_size() -> ?TAB_SIZE.
code_indent_size() -> ?CODE_INDENT_SIZE.
list_item_prefix_size_max() -> ?LIST_ITEM_PREFIX_SIZE_MAX.
heading_atx_opening_fence_size_max() -> ?HEADING_ATX_OPENING_FENCE_SIZE_MAX.
```

## Test Requirements

```erlang
-module(erlmd_util_char_test).
-include_lib("eunit/include/eunit.hrl").

whitespace_test() ->
    ?assert(erlmd_util_char:is_whitespace($\s)),
    ?assert(erlmd_util_char:is_whitespace($\t)),
    ?assert(erlmd_util_char:is_whitespace($\n)),
    ?assert(erlmd_util_char:is_whitespace($\r)),
    ?assertNot(erlmd_util_char:is_whitespace($a)),
    ?assertNot(erlmd_util_char:is_whitespace($1)).

ascii_alpha_test() ->
    ?assert(erlmd_util_char:is_ascii_alpha($a)),
    ?assert(erlmd_util_char:is_ascii_alpha($Z)),
    ?assertNot(erlmd_util_char:is_ascii_alpha($1)),
    ?assertNot(erlmd_util_char:is_ascii_alpha($!)).

ascii_digit_test() ->
    ?assert(erlmd_util_char:is_ascii_digit($0)),
    ?assert(erlmd_util_char:is_ascii_digit($9)),
    ?assertNot(erlmd_util_char:is_ascii_digit($a)).

punctuation_test() ->
    ?assert(erlmd_util_char:is_ascii_punctuation($!)),
    ?assert(erlmd_util_char:is_ascii_punctuation($*)),
    ?assert(erlmd_util_char:is_ascii_punctuation($[)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($a)).
```

## Acceptance Criteria

- [ ] All records defined with proper types
- [ ] Character classification functions work
- [ ] ASCII fast-path optimized
- [ ] Unicode stubs in place (can be TODO for now)
- [ ] Constants defined
- [ ] All unit tests pass
- [ ] No dialyzer warnings

```

### Phase 2: Tokenizer

```markdown
# Task: Implement State Machine Tokenizer

## Context
- **Phase**: 2 (Tokenizer Framework)
- **Module**: `src/erlmd_tokenizer.erl`
- **Dependencies**: erlmd_types.hrl, erlmd_util_char

## Reference
- **Rust Code**: `markdown-rs/src/tokenizer.rs`
- **Architecture Doc**: Section 2 (Tokenizer)
- **Erlang Patterns**: Section "State Machine Implementation Without gen_statem"

## Implementation

[See detailed tokenizer implementation in main plan...]

## Key Performance Patterns

### Match Context Preservation
Always pattern match in function head:

```erlang
%% GOOD - preserves match context
consume(#tokenizer{bytes = <<_, Rest/binary>>} = T) ->
    update_position(T#tokenizer{bytes = Rest}).

%% BAD - creates sub-binary
consume(T) ->
    case T#tokenizer.bytes of
        <<_, Rest/binary>> -> ...
    end.
```

### Tail Recursion

All parsing functions must be tail-recursive:

```erlang
%% GOOD - tail recursive
parse_chars(<<C, Rest/binary>>, Acc) ->
    parse_chars(Rest, [C | Acc]);
parse_chars(<<>>, Acc) ->
    lists:reverse(Acc).

%% BAD - not tail recursive
parse_chars(<<C, Rest/binary>>) ->
    [C | parse_chars(Rest)];
parse_chars(<<>>) ->
    [].
```

## Test Requirements

[See detailed tests in main plan...]

## Acceptance Criteria

- [ ] Tokenizer record complete
- [ ] Basic operations (new, current, consume)
- [ ] Position tracking (line, column, offset)
- [ ] Event generation (enter, exit)
- [ ] Attempt mechanism (backtracking)
- [ ] All unit tests pass
- [ ] No binary optimization warnings

```

### Phase 3-11: Construct Implementation

```markdown
# Task: Implement [Construct Name] Construct

## Context
- **Phase**: [N]
- **Module**: `src/construct/erlmd_construct_[name].erl`
- **Dependencies**: erlmd_tokenizer, erlmd_state
- **Priority**: [Priority based on content type hierarchy]

## Reference
- **Rust Code**: `markdown-rs/src/construct/[name].rs`
- **CommonMark Spec**: Section [X.Y]
- **Architecture Doc**: Section 5 (Construct System)

## CommonMark Specification

[Quote relevant parts of spec]

## Implementation Requirements

1. [Requirement 1]
2. [Requirement 2]
3. [Requirement 3]

## Function Signatures

```erlang
-module(erlmd_cnstr_[name]).
-export([start/1]).

-include_lib("erlmd/include/types.hrl").

-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    %% Implementation...
```

## Algorithm

[Step-by-step description]

## Edge Cases

- Edge case 1: [Description]
- Edge case 2: [Description]

## Test Requirements

```erlang
%% Test 1: Basic case
test_basic() ->
    Events = parse("[construct input]"),
    ?assertMatch([expected events], Events).

%% Test 2: Edge case
test_edge_case() ->
    ...

%% CommonMark Examples
test_commonmark_example_N() ->
    Input = "...",
    Expected = "...",
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

## Acceptance Criteria

- [ ] Basic functionality works
- [ ] Edge cases handled
- [ ] CommonMark examples X-Y pass
- [ ] No infinite loops
- [ ] Proper event nesting

```

---

## Prompt Generation Workflow

### Step 1: Choose Phase/Task
Look at main implementation plan, identify current task.

### Step 2: Gather Context
- Open 006-erlmd-library-rewrite.md → find relevant section
- Open 002-rewrite-research... → find relevant pattern
- Open CommonMark spec → find relevant section
- Look at markdown-rs Rust code for reference

### Step 3: Fill Template
Use appropriate template above, fill in specifics.

### Step 4: Add Examples
Include code examples showing:
- What TO do (good pattern)
- What NOT to do (anti-pattern)
- Test cases

### Step 5: Define Acceptance
Be specific about what "done" means.

---

## Example: Full Prompt for Paragraph Construct

```markdown
# Task: Implement Paragraph Construct

## Context
- **Phase**: 5 (Basic Block Constructs)
- **Module**: `src/construct/erlmd_construct_paragraph.erl`
- **Dependencies**: erlmd_tokenizer, erlmd_state, erlmd_construct_text
- **Priority**: HIGH (fundamental construct)

## Reference
- **Rust Code**: `markdown-rs/src/construct/paragraph.rs`
- **CommonMark Spec**: Section 4.8 (Paragraphs)
- **Architecture Doc**: Section 5.1 (Construct Pattern)

## CommonMark Specification

> A sequence of non-blank lines that cannot be interpreted as other kinds of blocks forms a paragraph. The contents of the paragraph are the result of parsing the paragraph's raw content as inlines. The paragraph's raw content is formed by concatenating the lines and removing initial and final whitespace.

Key properties:
- Default block-level construct (lowest priority)
- Contains inline content (text)
- Ends at: blank line, EOF, or interrupting construct
- Content parsed as "text" content type

## Implementation Requirements

1. **Entry**: Must be called by flow dispatcher
2. **Content Parsing**: Delegate to text content dispatcher
3. **Termination**: Detect blank lines and EOF
4. **Interruption**: Can be interrupted by other block constructs (not implemented yet, but structure should allow)

## Implementation

```erlang
-module(erlmd_construct_paragraph).
-export([start/1]).

-include("erlmd_types.hrl").

-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
start(T) ->
    T1 = erlmd_tokenizer:enter(T, paragraph),
    parse_content(T1).

parse_content(T) ->
    %% Try to parse text content
    case erlmd_state:call(text, T) of
        {ok, T1} ->
            %% Check what's next
            case erlmd_tokenizer:current(T1) of
                eof ->
                    %% End of input - close paragraph
                    {ok, erlmd_tokenizer:exit(T1)};
                C when C =:= $\n; C =:= $\r ->
                    %% Newline - consume and check for blank line
                    T2 = erlmd_tokenizer:consume(T1),
                    check_continuation(T2);
                _ ->
                    %% More content - continue parsing
                    parse_content(T1)
            end;
        {nok, T1} ->
            %% No text content parsed - shouldn't happen in paragraph
            %% but handle gracefully
            {nok, erlmd_tokenizer:exit(T1)}
    end.

check_continuation(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            %% End of input
            {ok, erlmd_tokenizer:exit(T)};
        C when C =:= $\n; C =:= $\r ->
            %% Another newline = blank line - end paragraph
            {ok, erlmd_tokenizer:exit(T)};
        _ ->
            %% More content on next line - continue paragraph
            parse_content(T)
    end.
```

## Pattern Notes

### Why This Pattern Works

1. **Tail Recursion**: Both `parse_content` and `check_continuation` are tail-recursive
2. **Match Context**: We only use `current()` and `consume()`, never pattern match on full binary inside function
3. **Event Nesting**: Enter at start, exit at end - maintains proper nesting
4. **Delegation**: Delegates inline parsing to text content dispatcher

### Common Pitfalls to Avoid

```erlang
%% DON'T DO THIS - not tail recursive
parse_content(T) ->
    case erlmd_state:call(text, T) of
        {ok, T1} ->
            T2 = parse_content(T1),  % NOT TAIL CALL!
            erlmd_tokenizer:exit(T2)
    end.

%% DON'T DO THIS - creates sub-binary
parse_content(T) ->
    case T#tokenizer.bytes of
        <<$\n, Rest/binary>> -> ...  % Creates sub-binary!
    end.
```

## Test Requirements

```erlang
-module(erlmd_construct_paragraph_test).
-include_lib("eunit/include/eunit.hrl").

%% Test 1: Simple paragraph
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

%% Test 2: Multiple paragraphs
multiple_paragraphs_test() ->
    Input = <<"Para 1\n\nPara 2">>,
    Events = test_helpers:parse(Input),
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(4, length(ParagraphEvents)),  % 2 enter + 2 exit
    ?assertEqual(2, length([E || E <- ParagraphEvents, E#event.kind =:= enter])).

%% Test 3: Paragraph ending at EOF
paragraph_eof_test() ->
    Input = <<"Single line">>,
    Events = test_helpers:parse(Input),
    ?assert(lists:any(fun(E) -> E#event.name =:= paragraph end, Events)).

%% CommonMark Example 189
commonmark_189_test() ->
    Input = <<"aaa\n\nbbb">>,
    Expected = <<"<p>aaa</p>\n<p>bbb</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 190
commonmark_190_test() ->
    Input = <<"aaa\nbbb\n\nccc\nddd">>,
    Expected = <<"<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 191
commonmark_191_test() ->
    Input = <<"aaa\n\n\nbbb">>,
    Expected = <<"<p>aaa</p>\n<p>bbb</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

## Integration with Flow Dispatcher

The flow dispatcher should call paragraph LAST (lowest priority):

```erlang
%% In erlmd_construct_flow.erl
-define(FLOW_CONSTRUCTS, [
    blank_line,
    thematic_break,
    % ... other constructs
    paragraph  % MUST BE LAST
]).
```

## Acceptance Criteria

- [ ] Can parse simple paragraphs
- [ ] Ends at blank lines
- [ ] Ends at EOF
- [ ] Multiple paragraphs separated by blank lines work
- [ ] CommonMark examples 189-196 pass
- [ ] No infinite loops on any input
- [ ] Proper event nesting (enter/exit paired)
- [ ] No binary optimization warnings
- [ ] All unit tests pass
- [ ] Integrates with flow dispatcher

## Next Steps After This Module

Once paragraph works:

1. Test with flow dispatcher
2. Verify HTML output
3. Move to next construct (heading_atx)

```

---

## Quick Tips

### Debugging

```erlang
%% Add debug logging
io:format("~s:~p Current: ~p~n", [?MODULE, ?LINE, erlmd_tokenizer:current(T)]),

%% Inspect events
Events = erlmd_tokenizer:events(T),
io:format("Events: ~p~n", [Events]),

%% Check position
#tokenizer{line = L, column = C, offset = O} = T,
io:format("Position: ~p:~p (offset ~p)~n", [L, C, O]).
```

### Common Issues

1. **Infinite Loop**: Usually missing EOF check
2. **Wrong Events**: Check enter/exit pairing
3. **Position Off**: Check line ending handling (\n vs \r\n)
4. **Sub-binary Warning**: Pattern match in function head, not in body
5. **Stack Overflow**: Check tail recursion

### Testing Strategy

1. **Start Small**: Single construct, simple input
2. **Add Edge Cases**: Empty input, EOF, line endings
3. **CommonMark Tests**: Run official tests
4. **Integration**: Test with full pipeline

---

## Summary

This quick reference provides:

- ✅ Standard prompt template
- ✅ Phase-specific examples
- ✅ Complete working example (paragraph)
- ✅ Common patterns and anti-patterns
- ✅ Testing strategies

Use these templates to generate focused, effective prompts for Claude!
