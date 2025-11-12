# Phase 3: Hard Line Break Detection - Implementation Instructions for Claude Code

## Overview

**Objective**: Fix hard line break detection in `erlmd_ast.erl` to properly convert two spaces + newline or tab + newline into `<br />` tags in HTML output.

**Estimated Time**: 2-3 hours  
**Expected Impact**: Fixes 3 failing tests  
**Priority**: CRITICAL  
**Prerequisites**: Phases 1 and 2 should be complete

---

## Context

Hard line breaks in markdown are created by ending a line with:
1. **Two spaces + newline**: `"text  \n"` → `text <br />`
2. **Tab + newline**: `"text\t\n"` → `text <br />`

These are DIFFERENT from soft line breaks (regular newlines), which are just whitespace.

### Example

**Input**:
```markdown
Line one  
Line two
```

**Expected output**:
```html
<p>Line one <br />
Line two</p>
```

### The Challenge

Hard line breaks need to be detected at TWO levels:

1. **Token level** (when merging blocks) - PARTIALLY DONE
2. **String level** (when parsing inline content) - MISSING

The issue: When we convert tokens to strings for inline parsing, we lose the hard break markers.

---

## Current State Analysis

### What's Already Implemented

**File**: `src/erlmd_ast.erl`  
**Function**: `mark_hard_breaks/1`  
**Lines**: 56-73

```erlang
mark_hard_breaks(Tokens) ->
    mark_hard_breaks(Tokens, []).

mark_hard_breaks([], Acc) ->
    lists:reverse(Acc);
%% Two spaces before LF - mark as hard break (check for comp type)
mark_hard_breaks([{{ws, comp}, _}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
%% Two spaces before LF - mark as hard break (check for sp type)
mark_hard_breaks([{{ws, sp}, "  "}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
%% Tab before LF - mark as hard break
mark_hard_breaks([{{ws, tab}, _}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
%% Keep other tokens as-is
mark_hard_breaks([H | T], Acc) ->
    mark_hard_breaks(T, [H | Acc]).
```

This function is called in `build_inline/2` (line 48):
```erlang
build_inline(Tokens, Refs) ->
    ProcessedTokens = mark_hard_breaks(Tokens),
    parse_inline_elements(ProcessedTokens, Refs, []).
```

And hard breaks are handled in `parse_inline_elements/3` (line 320):
```erlang
%% Hard line breaks (marked by mark_hard_breaks/1)
parse_inline_elements([{{hard_break, true}, _} | T], Refs, Acc) ->
    LB = #line_break{type = hard},
    parse_inline_elements(T, Refs, [LB | Acc]);
```

### The Problem

**The token-level detection works for some cases**, but fails when:

1. Tokens are collected and converted to strings in `collect_text_tokens/0`
2. The resulting string is passed to `parse_emphasis_and_code/2`
3. At this point, we've lost the hard break markers

**Example flow**:
```
Tokens: [{{ws, sp}, "  "}, {{lf, lf}, "\n"}, {string, "more"}]
        ↓ mark_hard_breaks
Tokens: [{{hard_break, true}, " <br />\n"}, {string, "more"}]
        ↓ parse_inline_elements
        ↓ collect_text_tokens (converts to string)
String: " <br />\nmore"
        ↓ parse_emphasis_and_code
        ↓ Hard break info is LOST - it's now just a string
```

---

## Task 1: Understand Original Behavior

**Action**: Study how the original implementation handles hard breaks.

**Reference**: `src/erlmd.erl` lines 263-268

```erlang
merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    flatten([NewP1, {string, Pad} | P2]).

make_br(List) -> make_br1(reverse(List)).

make_br1([{{lf, _}, _}, {{ws, comp}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1([{{lf, _}, _}, {{ws, tab}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1(List) -> 
    reverse(List).
```

**Key insight**: The original converts the pattern IN THE TOKEN STREAM by:
1. Looking at the end of the token list (reversed)
2. Finding `lf + ws` pattern
3. Replacing with `{tags, " <br />\n"}` token

This happens DURING MERGING, before strings are created.

When the tokens are eventually converted to strings, the `{tags, ...}` token is preserved and rendered as raw HTML.

---

## Task 2: Run Hard Break Tests

**Action**: Identify which hard break tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct --suite test/erlmd_SUITE | grep -i "break\|hard"
```

**Also search for line break tests**:
```bash
grep -rn "break\|hard.*line" test/erlmd_SUITE.erl
```

**Capture**:
1. How many hard break tests exist?
2. Which are failing?
3. What's the expected vs actual output?

---

## Task 3: Create Minimal Test Cases

**Action**: Create test files to isolate hard break scenarios.

### Test Case A: Basic Hard Break (Two Spaces)

**File**: `test_hard_break_spaces.md`
```markdown
Line one  
Line two
```

**Expected output**:
```html
<p>Line one <br />
Line two</p>
```

### Test Case B: Hard Break (Tab)

**File**: `test_hard_break_tab.md`
```markdown
Line one	
Line two
```
(Note: That's a literal tab character before the newline)

**Expected output**:
```html
<p>Line one <br />
Line two</p>
```

### Test Case C: Multiple Hard Breaks

**File**: `test_hard_break_multiple.md`
```markdown
Line one  
Line two  
Line three
```

**Expected output**:
```html
<p>Line one <br />
Line two <br />
Line three</p>
```

### Test Case D: Hard Break in Emphasis

**File**: `test_hard_break_emphasis.md`
```markdown
*Emphasized  
text*
```

**Expected output**:
```html
<p><em>Emphasized <br />
text</em></p>
```

### Test Case E: Not a Hard Break (Single Space)

**File**: `test_soft_break.md`
```markdown
Line one 
Line two
```

**Expected output** (soft break = space):
```html
<p>Line one
Line two</p>
```

### Test Case F: Hard Break in Code Block (Should NOT Convert)

**File**: `test_hard_break_code.md`
```markdown
    code line  
    more code
```

**Expected output** (preserve literal spaces):
```html
<pre><code>code line  
more code
</code></pre>
```

---

## Task 4: Test Original vs AST

**Action**: Run each test case through both implementations.

```erlang
test_hard_break(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    Original = erlmd:conv_original(MD),
    AST = erlmd:conv_ast(MD),
    
    io:format("~n=== Testing ~s ===~n", [File]),
    io:format("Original:~n~p~n", [Original]),
    io:format("AST:~n~p~n", [AST]),
    
    case Original =:= AST of
        true -> io:format("✓ MATCH~n");
        false -> 
            io:format("✗ MISMATCH~n"),
            % Show hex dump to see whitespace
            io:format("Original bytes: ~w~n", [Original]),
            io:format("AST bytes: ~w~n", [AST])
    end.

% Test all files
test_hard_break("test_hard_break_spaces.md").
test_hard_break("test_hard_break_tab.md").
test_hard_break("test_hard_break_multiple.md").
test_hard_break("test_hard_break_emphasis.md").
test_hard_break("test_soft_break.md").
test_hard_break("test_hard_break_code.md").
```

**Capture**: Which test cases fail and how?

---

## Task 5: Trace Token Flow

**Action**: Add debug output to see how tokens flow through the system.

**In `erlmd_ast.erl`, add debug to `build_inline/2`**:

```erlang
build_inline(Tokens, Refs) ->
    io:format("~nDEBUG build_inline - Input tokens:~n~p~n", [Tokens]),
    ProcessedTokens = mark_hard_breaks(Tokens),
    io:format("DEBUG build_inline - After mark_hard_breaks:~n~p~n", [ProcessedTokens]),
    Result = parse_inline_elements(ProcessedTokens, Refs, []),
    io:format("DEBUG build_inline - Result:~n~p~n", [Result]),
    Result.
```

**Test with a simple case**:
```erlang
{ok, C} = file:read_file("test_hard_break_spaces.md").
erlmd:conv_ast(binary_to_list(C)).
```

**Observe**:
1. What do the input tokens look like?
2. What do they look like after `mark_hard_breaks`?
3. What's the final result?

---

## Task 6: Check Where Markers Get Lost

**Action**: Trace where `{{hard_break, true}, ...}` tokens disappear.

**Add debug to `collect_text_tokens/0`** (around line 372):

```erlang
collect_text_tokens([]) ->
    {[], ""};
collect_text_tokens([{string, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
% Stop at hard breaks
collect_text_tokens([{{hard_break, _}, _} | _] = List) ->
    io:format("DEBUG: Stopping at hard break: ~p~n", [List]),
    {List, ""};
% ... rest of function
```

**Also add debug to `parse_inline_elements/3`** (around line 320):

```erlang
parse_inline_elements([{{hard_break, true}, _} | T], Refs, Acc) ->
    io:format("DEBUG: Found hard break marker~n"),
    LB = #line_break{type = hard},
    parse_inline_elements(T, Refs, [LB | Acc]);
```

**Run test again and observe**:
- Do we see "Found hard break marker"?
- Or does it get converted to a string first?

---

## Task 7: Analyze the Problem

Based on debug output, you should see one of these patterns:

### Pattern A: Hard break never marked

**Symptom**: After `mark_hard_breaks`, no `{{hard_break, true}, ...}` tokens exist.

**Cause**: The pattern matching in `mark_hard_breaks` doesn't catch the actual token pattern.

**Solution**: Fix the pattern matching in `mark_hard_breaks/1`.

### Pattern B: Hard break marked but lost

**Symptom**: After `mark_hard_breaks`, we see `{{hard_break, true}, ...}`, but later it disappears.

**Cause**: `collect_text_tokens` or another function converts it to a string.

**Solution**: Ensure hard break tokens are NOT collected as text.

### Pattern C: Hard break in wrong place

**Symptom**: Hard break is created but not at the right position.

**Cause**: Token ordering or collection logic.

**Solution**: Review token collection and ordering.

---

## Task 8: Fix mark_hard_breaks/1

**Action**: Ensure all hard break patterns are caught.

**Current patterns** (lines 61-69):
```erlang
mark_hard_breaks([{{ws, comp}, _}, {{lf, _}, _LF} | T], Acc) ->
mark_hard_breaks([{{ws, sp}, "  "}, {{lf, _}, _LF} | T], Acc) ->
mark_hard_breaks([{{ws, tab}, _}, {{lf, _}, _LF} | T], Acc) ->
```

**Issue 1**: The `{{ws, comp}, _}` pattern might not check if it actually has 2+ spaces.

**Check**: What is `{ws, comp}`? Looking at the lexer, this is composite whitespace (multiple spaces or tabs merged). But how many spaces does it have?

**Potential fix**:
```erlang
%% Composite whitespace before LF - check if it's 2+ spaces
mark_hard_breaks([{{ws, comp}, WS}, {{lf, _}, LF} | T], Acc) ->
    case ends_with_two_spaces(WS) of
        true -> mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
        false -> mark_hard_breaks(T, [{{lf, soft}, LF}, {{ws, comp}, WS} | Acc])
    end;

%% Two explicit spaces before LF
mark_hard_breaks([{{ws, sp}, "  "}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);

%% Tab before LF
mark_hard_breaks([{{ws, tab}, _}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);

%% Keep other tokens as-is
mark_hard_breaks([H | T], Acc) ->
    mark_hard_breaks(T, [H | Acc]).

%% Helper to check if whitespace ends with two spaces
ends_with_two_spaces(WS) when is_list(WS), length(WS) >= 2 ->
    case lists:reverse(WS) of
        [?SPACE, ?SPACE | _] -> true;
        _ -> false
    end;
ends_with_two_spaces(_) -> false.
```

**Issue 2**: The pattern might need to handle `{ws, sp}` with varying lengths.

**Check**: Can `{{ws, sp}, " "}` (single space) match? If so, we need to verify length.

**Better pattern**:
```erlang
%% Single space type - must be exactly "  " (two spaces)
mark_hard_breaks([{{ws, sp}, "  "}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);

%% Tab before LF
mark_hard_breaks([{{ws, tab}, _}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);

%% Composite whitespace - check if ends with two spaces
mark_hard_breaks([{{ws, comp}, WS}, {{lf, _}, _LF} | T], Acc) ->
    case ends_with_two_spaces(WS) of
        true -> mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
        false -> mark_hard_breaks(T, [{{lf, soft}, _LF}, {{ws, comp}, WS} | Acc])
    end;
```

---

## Task 9: Fix collect_text_tokens/0

**Action**: Ensure hard break tokens stop collection.

**Current code** (around line 387):
```erlang
% Stop at hard breaks
collect_text_tokens([{{hard_break, _}, _} | _] = List) ->
    {List, ""};
```

This looks correct! Hard breaks should stop text collection.

**But verify**: Add debug output to confirm this clause is matched:
```erlang
collect_text_tokens([{{hard_break, _}, _} | _] = List) ->
    io:format("DEBUG collect_text_tokens: Stopping at hard break~n"),
    {List, ""};
```

**If this clause is never hit**, the problem is that hard breaks aren't in the token stream at this point.

---

## Task 10: Alternative Approach - String-Level Detection

**If token-level detection is too complex**, we can detect hard breaks when parsing strings.

**Location**: `src/erlmd_ast.erl`, function `parse_emphasis_and_code/2` (around line 422)

**Add BEFORE any other line break handlers**:

```erlang
parse_emphasis_and_code([], Acc) ->
    lists:reverse(merge_adjacent_text(Acc));

%% CRITICAL: These hard break clauses must come BEFORE soft line break handlers

%% Hard line breaks - TWO spaces before newline (CRLF)
parse_emphasis_and_code([?SPACE, ?SPACE, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Hard line breaks - TWO spaces before newline (LF)
parse_emphasis_and_code([?SPACE, ?SPACE, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Tab before newline is also hard break (CRLF)
parse_emphasis_and_code([?TAB, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Tab before newline is also hard break (LF)
parse_emphasis_and_code([?TAB, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Regular soft line breaks (these come AFTER hard break handlers)
parse_emphasis_and_code([?CR, ?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

%% ... rest of function
```

**CRITICAL**: Order matters! Hard break clauses must come BEFORE soft break clauses, or Erlang will match the more general pattern first.

**Check current code**: Look at lines 451-461. The soft breaks are currently there. We need to add hard breaks BEFORE them.

---

## Task 11: Verify Tab Handling

**Action**: Ensure tabs are NOT expanded before hard break detection.

**In `collect_text_tokens/0`**, we should NOT expand tabs:
```erlang
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
```

This looks good - we're just collecting the raw string content.

**But check**: Does the lexer expand tabs? Look at `erlmd.erl` lex function.

From `erlmd.erl` line 1016:
```erlang
l1([?TAB | T], A1, A2)     -> l1(T, [], [{{ws, tab}, "\t"}, l2(A1) | A2]);
```

Good! The lexer preserves `"\t"` as the actual tab character.

**However**, in `parse_emphasis_and_code/2` (line 444), we have:
```erlang
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

**Problem**: This expands tabs to spaces BEFORE we can detect hard breaks!

**Fix**: Move this clause AFTER hard break detection:

```erlang
%% Hard breaks with tab (MUST come before tab expansion)
parse_emphasis_and_code([?TAB, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?TAB, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Tab expansion (comes AFTER hard break detection)
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

---

## Task 12: Check Code Block Handling

**Action**: Ensure hard breaks in code blocks are NOT converted.

Code blocks should preserve literal whitespace. The HTML encoding happens in the renderer, not during parsing.

**In `parse_blocks/3`** (around line 228):
```erlang
{Rest, _} ->
    Content = make_plain_str(snip(P1)),
    CodeBlock = #code_block{content = Content, language = undefined},
    parse_blocks(Rest, Refs, [CodeBlock | Acc])
```

This uses `make_plain_str/1` which should preserve whitespace.

**Check**: Does `make_plain_str/1` preserve the pattern `"  \n"`?

Test it:
```erlang
Tokens = [{{ws, sp}, "  "}, {{lf, lf}, "\n"}, {string, "more"}].
Result = erlmd_ast:make_plain_str(Tokens).
% Should be: "  \nmore"
```

If this works, code blocks should be fine.

**But wait**: Code blocks go through `build_inline/2`? No, they don't - code block content is processed as plain text, not inline elements.

So hard breaks in code blocks should already work correctly.

---

## Task 13: Apply the Fix

**Action**: Implement string-level hard break detection.

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_emphasis_and_code/2`  
**Location**: Around line 422

**Find the current line break handlers** (around lines 451-461):
```erlang
%% Line breaks (hard breaks already handled at token level)
parse_emphasis_and_code([?CR, ?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?CR | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);
```

**Replace with** (ADD hard break detection BEFORE soft breaks):

```erlang
%% ============================================================================
%% Line Breaks - CRITICAL: Hard breaks MUST come before soft breaks
%% ============================================================================

%% Hard line breaks - TWO spaces before CRLF
parse_emphasis_and_code([?SPACE, ?SPACE, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Hard line breaks - TWO spaces before LF
parse_emphasis_and_code([?SPACE, ?SPACE, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Hard line breaks - TAB before CRLF
parse_emphasis_and_code([?TAB, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Hard line breaks - TAB before LF
parse_emphasis_and_code([?TAB, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Soft line breaks - regular newlines (AFTER hard break detection)
parse_emphasis_and_code([?CR, ?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?CR | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);
```

**Also move tab expansion clause** (from line 444):

Find:
```erlang
%% Tab - expand to 4 spaces (hard breaks with tab already handled above)
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

**Move it AFTER the hard break with tab handlers**, so the order is:
1. Hard break with tab (tab + LF) ← detect this first
2. Tab expansion (bare tab) ← only expand if not followed by LF

The fix above already handles this correctly because we match `[?TAB, ?LF | T]` before the bare `[?TAB | T]` clause.

**BUT**: Make sure the bare tab clause comes AFTER the hard break clauses:

```erlang
%% Hard line breaks - TAB before LF (these come FIRST)
parse_emphasis_and_code([?TAB, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?TAB, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% ... soft line breaks ...

%% Tab expansion - only for bare tabs (comes AFTER hard break detection)
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

---

## Task 14: Test the Fix

**Action**: Run all hard break test cases.

```erlang
test_hard_break("test_hard_break_spaces.md").
test_hard_break("test_hard_break_tab.md").
test_hard_break("test_hard_break_multiple.md").
test_hard_break("test_hard_break_emphasis.md").
test_hard_break("test_soft_break.md").
test_hard_break("test_hard_break_code.md").
```

**Expected**: All should match between Original and AST.

**Run full test suite**:
```bash
rebar3 ct
```

**Check**:
1. How many tests pass now?
2. Did hard break tests improve?
3. Any regressions?

---

## Task 15: Document Findings

**Action**: Create a summary report.

```markdown
## Phase 3 Results

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Changes made**:
1. Added hard break detection in `parse_emphasis_and_code/2` BEFORE soft breaks
2. Moved tab expansion AFTER hard break detection
3. Added clauses for: two spaces + LF, two spaces + CRLF, tab + LF, tab + CRLF

**Root cause**:
The token-level `mark_hard_breaks/1` was partially working, but hard breaks weren't being detected reliably when tokens were converted to strings for inline parsing. String-level detection in `parse_emphasis_and_code/2` catches all patterns.

**Why the fix works**:
By detecting the literal pattern `[?SPACE, ?SPACE, ?LF | T]` in the string, we catch hard breaks at the point where inline content is being parsed, before emphasis/strong/code processing.

**Test results**:
- ✓/✗ Basic hard break (two spaces)
- ✓/✗ Hard break (tab)
- ✓/✗ Multiple hard breaks
- ✓/✗ Hard break in emphasis
- ✓/✗ Soft break (no conversion)
- ✓/✗ Code block (no conversion)

**Remaining issues**:
[List any hard break tests still failing]

**Next steps**:
- [If all pass]: Move to Phase 4 (List Tight/Loose Detection)
- [If some fail]: Investigate [specific issue]
```

---

## Success Criteria

Phase 3 is complete when:

- ✅ Two spaces + newline creates hard break
- ✅ Tab + newline creates hard break
- ✅ Single space + newline does NOT create hard break
- ✅ Hard breaks work inside emphasis/strong
- ✅ Hard breaks in code blocks are NOT converted
- ✅ All hard break tests pass
- ✅ No regressions in other tests
- ✅ At least 3 additional tests passing overall

---

## Troubleshooting Guide

### Issue: Hard breaks not detected at all

**Check**: Are the hard break clauses in `parse_emphasis_and_code/2` BEFORE soft break clauses?

**Debug**: Add output:
```erlang
parse_emphasis_and_code([?SPACE, ?SPACE, ?LF | T], Acc) ->
    io:format("DEBUG: Hard break detected!~n"),
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);
```

### Issue: All line breaks become hard breaks

**Symptom**: Regular newlines are converted to `<br />`.

**Cause**: Clause ordering is wrong - soft breaks are matching before hard breaks are checked.

**Solution**: Move hard break clauses BEFORE soft break clauses.

### Issue: Tab hard breaks don't work

**Check**: Is tab expansion happening BEFORE hard break detection?

**Debug**: Print the character codes:
```erlang
String = "text\t\n",
io:format("Chars: ~w~n", [String]).
% Should see: [116,101,120,116,9,10] (9 is tab, 10 is LF)
```

If you see spaces (32) instead of tab (9), expansion happened too early.

### Issue: Hard breaks in emphasis don't work

**Symptom**: `*text  \nmore*` doesn't have hard break inside emphasis.

**Cause**: Emphasis parsing might be consuming the text before hard break detection.

**Check**: The flow should be:
1. Find emphasis delimiters
2. Extract content between delimiters
3. Parse that content (which includes hard break detection)

The current implementation calls `collect_until/2` which returns a string, then wraps it in `#text{}`. We need to recursively parse the content.

**But wait**: Looking at lines 360-364:
```erlang
parse_emphasis_and_code([$* | T], Acc) ->
    case collect_until(T, [$*]) of
        {Rest, Content} ->
            Em = #emphasis{content = [#text{content = Content}], delimiter = $*},
```

This puts raw `Content` in a `#text{}` node. It doesn't parse it for hard breaks!

**Potential fix** (complex - may be Phase 4 issue):
```erlang
parse_emphasis_and_code([$* | T], Acc) ->
    case collect_until(T, [$*]) of
        {Rest, Content} ->
            % Recursively parse content for hard breaks, code, etc.
            ParsedContent = parse_emphasis_and_code(Content, []),
            Em = #emphasis{content = ParsedContent, delimiter = $*},
```

**But**: This might break other things. Test carefully.

---

## Reference Code

### Original make_br/1 (erlmd.erl lines 265-268)

```erlang
make_br(List) -> make_br1(reverse(List)).

make_br1([{{lf, _}, _}, {{ws, comp}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1([{{lf, _}, _}, {{ws, tab}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1(List) -> 
    reverse(List).
```

This operates on TOKENS, replacing the `lf + ws` pattern with a `{tags, " <br />\n"}` token.

### Original merge/3 (erlmd.erl line 263)

```erlang
merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    flatten([NewP1, {string, Pad} | P2]).
```

Hard breaks are detected DURING merging, at the token level, BEFORE strings are created.

---

## Key Insights

1. **Two levels of detection needed**: Token level (during merging) AND string level (during inline parsing)
2. **Order matters**: Hard break clauses must come BEFORE soft break and tab expansion clauses
3. **Clause specificity**: More specific patterns (like `[?TAB, ?LF | T]`) must come before less specific patterns (like `[?TAB | T]`)
4. **Code blocks are special**: They don't go through inline parsing, so hard breaks are naturally preserved as literal text

---

## Alternative Approaches

If the string-level approach doesn't work, we could:

### Approach A: Keep hard break tokens through parsing

Instead of converting to strings, keep tokens and only convert at the very end. This is a bigger refactor.

### Approach B: Pre-process strings to insert markers

Before parsing inline content, replace `"  \n"` with a special marker like `"§HARDBREAK§"`, then replace back in the renderer.

```erlang
preprocess_hard_breaks(String) ->
    String1 = re:replace(String, "  \n", "§HB§", [global, {return, list}]),
    re:replace(String1, "\t\n", "§HB§", [global, {return, list}]).
```

Then in `parse_emphasis_and_code`:
```erlang
parse_emphasis_and_code([$§, $H, $B, $§ | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);
```

**But**: Using special characters is fragile. The pattern-matching approach is cleaner.

---

## Deliverables

After completing Phase 3, provide:

1. **Test results**: Before and after numbers, specific hard break tests
2. **Code changes**: Exact diffs showing the fix
3. **Test file results**: Output from all 6 test files
4. **Analysis**: Why string-level detection was necessary
5. **Edge case results**: Which edge cases pass/fail
6. **Debug output**: Any interesting observations from tracing

This information will guide Phase 4 (List Tight/Loose Detection).

---

## Notes

**Complexity**: This phase is tricky because hard breaks need detection at multiple levels. The string-level approach is the most reliable.

**Performance**: The pattern matching on strings is efficient - Erlang optimizes these patterns well.

**Relationship to Phase 1**: If Phase 1 fixed `make_br/1`, it handles hard breaks during token merging. Phase 3 handles them during inline parsing.

**Relationship to Phase 2**: Code blocks don't go through inline parsing, so Phase 3 shouldn't affect them.
