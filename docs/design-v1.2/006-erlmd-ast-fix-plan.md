# erlmd AST Refactoring: Complete Implementation Plan

## Executive Summary

**Goal**: Achieve 100% test compatibility (296 passing tests) by faithfully replicating the original `erlmd.erl` block parsing behavior in the new AST-based architecture.

**Current Status**: ~256 tests passing, ~40 failing (as of last check)

**Root Cause**: The original `p1/4` function in `erlmd.erl` uses complex lookahead and token consumption logic BEFORE generating HTML. Our AST builder tries to create nodes immediately, missing critical preprocessing/merging steps.

**Strategy**: Add consumption clauses to `parse_blocks/3` that replicate the original's greedy merging behavior, then fix inline parsing issues.

---

## Project Context

### Architecture Overview

**Original Implementation** (`erlmd.erl`):
```
Lexer → Make Lines → Type Lines → Parse (p1/4) → HTML String
                                    ↑
                                    Complex lookahead and merging
```

**New AST Implementation**:
```
Lexer → Make Lines → Type Lines → Parse Blocks (parse_blocks/3) → AST → HTML Renderer
                                    ↑
                                    Need to add merging logic HERE
```

### Key Files

- `src/erlmd.erl` - Original implementation (reference), now includes `conv_ast/1` and `conv_original/1`
- `src/erlmd_ast.erl` - AST builder (needs fixes)
- `src/erlmd_html.erl` - HTML renderer (mostly correct)
- `include/types.hrl` - AST type definitions
- `test/erlmd_SUITE.erl` - Test suite (296 tests)

### Critical Insight

The original `p1/4` function processes typed lines with patterns like:

```erlang
%% Lists swallow normal lines
p1([{{ul, P1}, S1}, {{normal, P2}, S2} | T], R, I, Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
```

This means: "When you see a list followed by a normal line, DON'T create two nodes - merge them into one list node and try again."

Our AST builder currently creates nodes immediately without this merging, causing mismatches.

---

## Implementation Phases

### Phase 1: List Consumption Logic ⏸ NOT STARTED
**Priority**: CRITICAL  
**Estimated Time**: 2-3 hours  
**Test Impact**: Fixes ~11-14 tests immediately

#### Background

The original implementation has lists "swallow" following normal and codeblock lines by merging them into the list item content. This happens BEFORE the list is converted to HTML.

#### Original Behavior (from erlmd.erl lines 196-210)

```erlang
%% Unordered lists swallow normal and codeblock lines
p1([{{ul, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ul, P1}, S1}, {{codeblock, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);

%% Ordered lists swallow normal and codeblock lines
p1([{{ol, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ol, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ol, P1}, S1}, {{codeblock, P2}, S2} | T], R, I , Acc) ->
    p1([{{ol, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
```

#### Required Changes

**Location**: `src/erlmd_ast.erl`, function `parse_blocks/3`

**Action**: Add these clauses BEFORE the general `ul`/`ol` parsing clauses (around line 194):

```erlang
%% List consumption - MUST come before general list parsing
%% Unordered lists swallow normal lines
parse_blocks([{{ul, P1}, S1}, {normal, P2} | T], Refs, Acc) ->
    % Merge the normal line into the list item
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ul, Merged}, S1} | T], Refs, Acc);

%% Unordered lists swallow codeblock lines
parse_blocks([{{ul, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ul, Merged}, S1 ++ S2} | T], Refs, Acc);

%% Ordered lists swallow normal lines
parse_blocks([{{ol, P1}, S1}, {normal, P2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ol, Merged}, S1} | T], Refs, Acc);

%% Ordered lists swallow codeblock lines
parse_blocks([{{ol, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ol, Merged}, S1 ++ S2} | T], Refs, Acc);
```

**Note**: These clauses ALREADY EXIST in the current code (lines 200-217), so this phase may already be complete. Need to verify why tests are still failing.

#### Verification Steps

1. Run test suite: `rebar3 ct`
2. Check if list-related test failures decrease
3. Look for patterns like:
   - `ordered_list_*` tests
   - `unordered_list_*` tests
   - Tests with list items containing multiple paragraphs

#### Success Criteria

- At least 10 additional tests passing
- No new test failures introduced
- List items with continuation lines render correctly

---

### Phase 2: Codeblock Consumption Logic ⏸ NOT STARTED
**Priority**: CRITICAL  
**Estimated Time**: 1-2 hours  
**Test Impact**: Fixes ~3-5 tests

#### Background

Codeblocks consume following codeblocks, including empty lines between them. This creates multi-line code blocks from separate typed lines.

#### Original Behavior (from erlmd.erl lines 211-218)

```erlang
%% Codeblocks consume following codeblocks
p1([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], R, I, Acc) ->
    p1([{{codeblock, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);

%% Codeblocks also grab empty lines then more codeblocks
p1([{{codeblock, P1}, S1} | T1], R, I, Acc) ->
    case grab_empties(T1) of
        {[{{codeblock, P2}, S2} | T2], E} ->
            p1([{{codeblock, merge(P1, pad(I), E ++ P2)}, S1 ++ E ++ S2} | T2],
               R, I, Acc);
        {Rest, _} ->
            p1(Rest, R, I, ["<pre><code>" ++ htmlencode(make_plain_str(snip(P1)))
                            ++ "\n</code></pre>\n\n" | Acc])
    end;
```

#### Required Changes

**Location**: `src/erlmd_ast.erl`, function `parse_blocks/3`

**Current State**: Lines 219-228 already have codeblock consumption logic:

```erlang
%% Code blocks
parse_blocks([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{{codeblock, Merged}, S1 ++ S2} | T], Refs, Acc);

parse_blocks([{{codeblock, P1}, S1} | T1], Refs, Acc) ->
    case grab_empties(T1) of
        {[{{codeblock, P2}, S2} | T2], E} ->
            Merged = lists:flatten([P1, E | P2]),
            parse_blocks([{{codeblock, Merged}, S1 ++ E ++ S2} | T2], Refs, Acc);
        {Rest, _} ->
            Content = make_plain_str(snip(P1)),
            CodeBlock = #code_block{content = Content, language = undefined},
            parse_blocks(Rest, Refs, [CodeBlock | Acc])
    end;
```

**Issue**: The merging logic might not perfectly match the original. The original uses `merge/3` with padding, ours uses `lists:flatten`.

**Action Required**:
1. Check if `merge/3` is implemented (it should be `merge_tokens/2`)
2. Verify padding behavior matches original
3. May need to update merging to use proper padding

#### Verification Steps

1. Find tests related to code blocks: `grep -r "codeblock" test/`
2. Run those specific tests
3. Compare output with original implementation

#### Success Criteria

- Multi-line code blocks render correctly
- Empty lines within code blocks are preserved
- Code blocks separated by blank lines merge appropriately

---

### Phase 3: Hard Line Break Detection ⏸ NOT STARTED
**Priority**: CRITICAL  
**Estimated Time**: 2-3 hours  
**Test Impact**: Fixes ~3 tests

#### Background

Hard line breaks occur when:
- Two spaces before a newline: `"text  \n"`
- Tab before a newline: `"text\t\n"`

These should render as `<br />` tags in HTML, not soft line breaks.

#### Current Implementation

We have `mark_hard_breaks/1` in `erlmd_ast.erl` (lines 56-73) that attempts to detect hard breaks at the token level:

```erlang
mark_hard_breaks([{{ws, comp}, _}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
mark_hard_breaks([{{ws, sp}, "  "}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
mark_hard_breaks([{{ws, tab}, _}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
```

#### Problem

Hard breaks need to be detected BOTH at:
1. **Token level** (for block-level content like paragraphs) - DONE
2. **String level** (for inline parsing within text) - MISSING

When we collect text tokens and convert to strings in `parse_text_formatting`, we lose the hard break markers.

#### Original Behavior (from erlmd.erl)

The original does this in `make_br/1` (lines 263-268):

```erlang
make_br1([{{lf, _}, _}, {{ws, comp}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1([{{lf, _}, _}, {{ws, tab}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1(List) -> 
    reverse(List).
```

This happens during MERGING in `merge/3`, which operates on tokens before string conversion.

#### Required Changes

**Option A: Fix at Token Level** (Recommended)

Update `mark_hard_breaks/1` to handle all whitespace patterns:

```erlang
%% Two spaces before LF (check string content)
mark_hard_breaks([{{ws, sp}, WS}, {{lf, _}, LF} | T], Acc) ->
    case ends_with_two_spaces(WS) of
        true -> mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
        false -> mark_hard_breaks(T, [{{lf, soft}, LF}, {{ws, sp}, WS} | Acc])
    end;

%% Helper to check if whitespace ends with two spaces
ends_with_two_spaces(WS) when length(WS) >= 2 ->
    case lists:reverse(WS) of
        [?SPACE, ?SPACE | _] -> true;
        _ -> false
    end;
ends_with_two_spaces(_) -> false.
```

**Option B: Fix During Merge**

Implement in `merge_with_br/2` and ensure it's called appropriately:

```erlang
merge_with_br(P1, P2) ->
    % Check if P1 ends with two spaces + lf pattern
    NewP1 = case lists:reverse(P1) of
        [{{lf, _}, _}, {{ws, sp}, "  "} | Rest] ->
            lists:reverse([{tags, " <br />\n"} | Rest]);
        [{{lf, _}, _}, {{ws, tab}, _} | Rest] ->
            lists:reverse([{tags, " <br />\n"} | Rest]);
        _ ->
            P1
    end,
    lists:flatten([NewP1, {tags, "<br /> "} | P2]).
```

#### Verification Steps

1. Create test with hard break: `"line1  \nline2"`
2. Check AST contains `#line_break{type = hard}`
3. Check HTML output contains `<br />`
4. Run: `rebar3 ct --suite test/erlmd_SUITE --case hard_line_breaks`

#### Success Criteria

- Hard breaks render as `<br />` not soft line breaks
- Two spaces + newline detected correctly
- Tab + newline detected correctly
- No false positives (single space + newline remains soft)

---

### Phase 4: List Tight/Loose Detection ⏸ NOT STARTED
**Priority**: HIGH  
**Estimated Time**: 2-3 hours  
**Test Impact**: Fixes ~4 tests

#### Background

Lists can be "tight" or "loose":
- **Tight**: No blank lines between items → items NOT wrapped in `<p>` tags
- **Loose**: Blank lines between items → items wrapped in `<p>` tags

Once a list becomes loose, ALL items are wrapped in `<p>` tags.

#### Original Logic (from erlmd.erl lines 223-230)

```erlang
parse_list(Type, [{{Type, P}, _} | T], R, I, A, Wrap) ->
    {Rest, NewP, NewWrap} = grab(T, R, [], Wrap),
    Li = case NewWrap of
             false -> % tight - strip <p> tags
                      Ret = parse([{normal, P}], R),
                      Ret2 = string:left(Ret, length(Ret) - 4),
                      Ret3 = string:right(Ret2, length(Ret2) - 3),
                      Ret3 ++ "\n" ++ NewP ++ pad(I);
             true  -> % loose - keep <p> tags
                      string:strip(parse([{normal, P}], R), right, ?LF)
                          ++ NewP ++ pad(I)
         end,
    NewWrap2 = case T of
                   []         -> false;
                   [H2 | _T2] -> case H2 of
                                     {linefeed, _} -> true;
                                     _             -> false
                                 end
               end,
    parse_list(Type, Rest, R, I, [pad(I) ++ "<li>" ++ ... ++ "</li>\n" | A], NewWrap2);
```

**Key insight**: The decision about wrapping happens by looking at the NEXT item (checking if current item is followed by linefeed/blank).

#### Current Implementation Issues

In `erlmd_ast.erl`, `collect_list_items/5` (lines 271-294):

```erlang
collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    {Rest, ItemContent, NewTight} = grab_list_item(T, Refs, [], Tight),
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs),
    Item = #list_item{content = ItemBlocks},
    
    NextTight = case T of
        [] -> NewTight;
        [H2 | _T2] ->
            case H2 of
                {linefeed, _} -> false;  % loose list
                _ -> NewTight
            end
    end,
    
    collect_list_items(Type, Rest, Refs, [Item | Acc], NextTight);
```

**Problem**: This logic checks for linefeed but may not propagate loose mode correctly to ALL items.

#### Required Changes

**Update `collect_list_items/5`**:

```erlang
collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    % Check if THIS item is followed by blank/linefeed
    NextWrap = case T of
        [] -> false;
        [{linefeed, _} | _] -> true;
        [{blank, _} | _] -> true;
        _ -> false
    end,
    
    {Rest, ItemContent, _} = grab_list_item(T, Refs, [], Tight),
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs, Tight),
    Item = #list_item{content = ItemBlocks},
    
    % Once we're loose, stay loose
    NewTight = Tight andalso (not NextWrap),
    
    collect_list_items(Type, Rest, Refs, [Item | Acc], NewTight);
```

**Update `parse_list_item_content/3` to take Tight parameter**:

```erlang
parse_list_item_content(P, AdditionalContent, Refs, Tight) ->
    Content = build_inline(P, Refs),
    Para = #paragraph{content = Content},
    
    % Store tight/loose info somehow - or handle in renderer
    case AdditionalContent of
        [] -> [Para];
        _ -> [Para | AdditionalContent]
    end.
```

**Update renderer** (`erlmd_html.erl`) to respect `tight` flag properly.

#### Verification Steps

1. Test tight list (no blank lines between items)
2. Test loose list (blank lines between items)
3. Test mixed (starts tight, becomes loose)
4. Check HTML output matches original

#### Success Criteria

- Tight lists have no `<p>` tags around items
- Loose lists have `<p>` tags around items
- Mixed lists are fully loose
- All list tests pass

---

### Phase 5: Blockquote Merging ⏸ NOT STARTED
**Priority**: MEDIUM  
**Estimated Time**: 1-2 hours  
**Test Impact**: Fixes ~3 tests

#### Background

Blockquotes can consume:
1. Other blockquotes (with `<br />` separator)
2. Normal lines

#### Original Behavior (from erlmd.erl lines 178-183)

```erlang
%% Blockquotes swallow each other - replace first > with space
p1([{blockquote, P1}, {blockquote, [_ | P2]} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1++[{tags, "<br />"}], [], P2)} | T], R, I, Acc);

%% Blockquotes swallow normal lines
p1([{blockquote, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1, [], P2)} | T], R, I, Acc);
```

#### Current Implementation

Lines 145-156 in `erlmd_ast.erl`:

```erlang
parse_blocks([{blockquote, P1}, {blockquote, [_ | P2]} | T], Refs, Acc) ->
    P2Trimmed = case P2 of
        [{{ws, _}, _} | Rest] -> Rest;
        _ -> P2
    end,
    Merged = merge_with_br(P1, P2Trimmed),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);

parse_blocks([{blockquote, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);
```

**Issue**: May need to verify that whitespace trimming matches original exactly.

#### Required Changes

Verify implementation matches original behavior:

1. Check that `merge_with_br/2` produces correct output
2. Ensure whitespace after `>` is stripped correctly
3. Test nested blockquotes

#### Verification Steps

1. Test blockquote followed by blockquote
2. Test blockquote followed by normal
3. Check for `<br />` between merged blockquotes

#### Success Criteria

- Consecutive blockquotes merge with `<br />`
- Normal lines are absorbed into blockquotes
- Whitespace handling matches original

---

### Phase 6: Tab Expansion ⏸ NOT STARTED
**Priority**: LOW  
**Estimated Time**: 30 minutes  
**Test Impact**: Fixes 1 test

#### Background

Tabs should expand to 4 spaces in rendered output.

#### Current Issue

Tabs inside `{string, "text\tmore"}` tokens aren't being expanded.

#### Required Changes

**Option A**: Expand during text collection in `collect_text_tokens/0`:

```erlang
collect_text_tokens([{string, S} | T]) ->
    {Rest, More} = collect_text_tokens(T),
    % Expand tabs in strings
    Expanded = expand_tabs_in_string(S),
    {Rest, Expanded ++ More};

expand_tabs_in_string(S) ->
    lists:flatten([case C of ?TAB -> "    "; _ -> C end || C <- S]).
```

**Option B**: Expand in `parse_emphasis_and_code/2` (already done at line 444):

```erlang
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

#### Verification

Check that tabs in text content become 4 spaces in HTML output.

---

### Phase 7: h2_or_hr Handling ⏸ NOT STARTED
**Priority**: LOW  
**Estimated Time**: 30 minutes  
**Test Impact**: Fixes 1-2 tests

#### Background

The `h2_or_hr` type is ambiguous - it becomes:
- `<h2>` if preceded by a normal line (setext h2)
- `<hr />` otherwise

#### Original Behavior (from erlmd.erl lines 226-228)

```erlang
%% h2_or_hr is greedy for normal lines
p1([{h2_or_hr, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{normal, flatten([P1 | P2])} | T], R, I, Acc);

%% Standalone h2_or_hr becomes hr
p1([{h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I, ["<hr />" | Acc]);
```

#### Current Implementation

Lines 233-237 in `erlmd_ast.erl`:

```erlang
parse_blocks([{h2_or_hr, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{normal, Merged} | T], Refs, Acc);

parse_blocks([{h2_or_hr, _} | T], Refs, Acc) ->
    parse_blocks(T, Refs, [#horizontal_rule{} | Acc]);
```

**Status**: Looks correct, but may need verification.

---

### Phase 8: Tag Handling ⏸ NOT STARTED
**Priority**: LOW  
**Estimated Time**: 1 hour  
**Test Impact**: Fixes 1-2 tests

#### Background

Single HTML tags (not block tags) have special handling in the original.

#### Original Behavior (from erlmd.erl lines 122-131)

```erlang
p1([{tag, Tag} | T], R, I, Acc) ->
    case T of
        []                -> p1([], R, I,
                                ["</p>", make_tag_str(Tag, R), "<p>" | Acc]);
        [{blank, _} | T2] -> p1(T2, R, I,
                                [make_tag_str(Tag, R) | Acc]);
        _Other            -> p1(T, R, I,
                                [pad(I) ++ make_tag_str(Tag, R) | Acc])
    end;
```

#### Current Implementation

Lines 84-92 in `erlmd_ast.erl`:

```erlang
parse_blocks([{tag, Tag} | T], Refs, Acc) ->
    TagStr = make_tag_str(Tag),
    Content = [#html_inline{content = TagStr}],
    Para = #paragraph{content = Content},
    parse_blocks(T, Refs, [Para | Acc]);
```

**Issue**: We wrap in paragraph, original has more nuanced behavior.

#### Required Changes

May need to match original's context-sensitive behavior, but this is low priority as it affects edge cases.

---

## Testing Strategy

### Full Test Run

```bash
rebar3 ct
```

### Specific Test Categories

```bash
# List tests
rebar3 ct --suite test/erlmd_SUITE --case ordered_list_*
rebar3 ct --suite test/erlmd_SUITE --case unordered_list_*

# Blockquote tests
rebar3 ct --suite test/erlmd_SUITE --case blockquote_*

# Code block tests
rebar3 ct --suite test/erlmd_SUITE --case codeblock_*

# Hard break tests
rebar3 ct --suite test/erlmd_SUITE --case hard_line_*
```

### Comparing Output

To compare AST vs Original:

```erlang
% In Erlang shell
{ok, Content} = file:read_file("test/data/test.md").
Original = erlmd:conv_original(binary_to_list(Content)).
AST = erlmd:conv_ast(binary_to_list(Content)).
Original =:= AST.  % Should be true
```

---

## Success Metrics

### Phase Completion

- [ ] Phase 1: List consumption (11-14 tests)
- [ ] Phase 2: Codeblock consumption (3-5 tests)
- [ ] Phase 3: Hard line breaks (3 tests)
- [ ] Phase 4: List tight/loose (4 tests)
- [ ] Phase 5: Blockquote merging (3 tests)
- [ ] Phase 6: Tab expansion (1 test)
- [ ] Phase 7: h2_or_hr (1-2 tests)
- [ ] Phase 8: Tag handling (1-2 tests)

### Final Target

- **296/296 tests passing** (100%)
- **No regressions** in previously passing tests
- **Clean test output** with no warnings

---

## Development Workflow

### Starting a Phase

1. Read phase description thoroughly
2. Review original code referenced
3. Check current implementation
4. Identify exact changes needed
5. Make changes incrementally
6. Test after each change

### Claude Code Instructions Template

For each phase, use this format:

```markdown
## Task: [Phase Name]

**Files to modify**:
- src/erlmd_ast.erl

**Changes required**:
1. [Specific change with line numbers]
2. [Specific change with line numbers]

**Reference implementation**:
[Show original erlmd.erl code]

**Success criteria**:
- [Specific test or behavior]
- [Specific output format]

**Testing**:
```bash
rebar3 ct --suite test/erlmd_SUITE
```

Check that [X] tests now pass.
```

---

## Notes and Observations

### Why Tests Are Failing

The core issue is **order of operations**:

1. **Original**: Lex → Type → **Merge/Consume** → Generate HTML
2. **AST version**: Lex → Type → **Create Nodes** → Render HTML

We're creating nodes too early, before the merging logic runs.

### Key Functions to Understand

From `erlmd.erl`:
- `p1/4` - Main parser with lookahead
- `merge/3` - Token merging with padding
- `grab_empties/1` - Collect blank/linefeed lines
- `parse_list/6` - List parsing with tight/loose detection
- `make_br/1` - Hard line break detection

### Helper Functions Available

In `erlmd_ast.erl`:
- `merge_tokens/2` - Simple token merging
- `merge_with_br/2` - Merge with `<br />` separator
- `grab_empties/1` - Same as original
- `make_plain_str/1` - Convert tokens to plain string

---

## Future Refactoring (After Tests Pass)

Once we achieve 100% test compatibility:

### Module Organization

Split `erlmd_ast.erl` into logical modules:

```
erlmd_lexer.erl      - Tokenization
erlmd_lines.erl      - Line processing and typing
erlmd_parser.erl     - Block parsing (parse_blocks/3)
erlmd_inline.erl     - Inline parsing
erlmd_ast.erl        - Public API and coordination
erlmd_html.erl       - HTML rendering (already separate)
```

### API Cleanup

Create clean public API in `erlmd.erl`:

```erlang
-export([
    conv/1,          % Main conversion (uses AST by default)
    conv_utf8/1,     % UTF-8 conversion
    conv_file/2,     % File conversion
    conv_ast/1,      % Explicit AST path
    conv_original/1  % Original implementation (for comparison)
]).
```

### Documentation

- Add comprehensive EDoc comments
- Create user guide
- Document AST structure
- Add examples of AST manipulation

---

## Contact Points for Help

- **Original implementation**: `src/erlmd.erl` - especially `p1/4` function
- **Test suite**: `test/erlmd_SUITE.erl` - all test cases
- **This document**: Complete context for any phase
- **Roadmap document**: `005-complete-compatibility-roadmap.md` - detailed analysis

---

## Revision History

- **2025-01-XX**: Initial creation
- Plan created before starting Phase 1 implementation
- All phases marked as NOT STARTED
- Ready for systematic implementation
