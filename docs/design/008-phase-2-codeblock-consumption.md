# Phase 2: Codeblock Consumption Logic - Implementation Instructions for Claude Code

## Overview

**Objective**: Verify and fix codeblock consumption logic in `erlmd_ast.erl` to match the original `erlmd.erl` behavior where codeblocks merge with following codeblocks, including those separated by empty lines.

**Estimated Time**: 1-2 hours  
**Expected Impact**: Fixes 3-5 failing tests  
**Priority**: CRITICAL  
**Prerequisites**: Phase 1 (List Consumption) should be complete

---

## Context

Code blocks in markdown are created by indenting lines with 4 spaces or 1 tab. The original parser has special logic to merge consecutive code blocks, even when separated by blank lines.

### Example

**Input**:
```markdown
    code line 1
    code line 2

    code line 3
```

**Expected behavior**: These should merge into a SINGLE code block with the blank line preserved as content:
```html
<pre><code>code line 1
code line 2

code line 3
</code></pre>
```

**Current behavior** (if broken): Might create separate code blocks or drop the blank line.

### Why This Matters

The original `p1/4` function has two consumption patterns:
1. **Adjacent codeblocks** merge immediately
2. **Codeblocks separated by empties** grab the empties first, then merge

This preprocessing happens BEFORE HTML generation, ensuring multi-line code blocks are preserved correctly.

---

## Current State Analysis

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_blocks/3`  
**Lines**: 219-228 (approximately)

The code ALREADY HAS codeblock consumption clauses:

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

**Problem**: These clauses exist but may not perfectly match the original's merging behavior.

---

## Task 1: Understand Original Behavior

**Action**: Study the original codeblock consumption logic.

**Reference**: `src/erlmd.erl` lines 211-218

```erlang
%% codeblock consumes any following empty lines and other codeblocks
p1([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], R, I, Acc) ->
    p1([{{codeblock, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);

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

**Key observations**:

1. **Adjacent codeblocks**: Uses `merge(P1, pad(I), P2)`
2. **Separated codeblocks**: Uses `merge(P1, pad(I), E ++ P2)` where `E` is the empty lines
3. **Final output**: Uses `htmlencode(make_plain_str(snip(P1)))`
4. **Padding**: Uses `pad(I)` based on indent level

**Our implementation**:

1. **Adjacent**: Uses `lists:flatten([P1 | P2])` (no merge call, no padding)
2. **Separated**: Uses `lists:flatten([P1, E | P2])` (no merge call, no padding)
3. **Final output**: Uses `make_plain_str(snip(P1))` (correct)
4. **Padding**: No padding applied

**Hypothesis**: The lack of proper `merge/3` call and padding might cause issues.

---

## Task 2: Run Codeblock Tests

**Action**: Identify which codeblock tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct --suite test/erlmd_SUITE --case codeblock_*
```

**Also search for code-related tests**:
```bash
grep -r "code" test/erlmd_SUITE.erl | grep "test_"
```

**Capture**:
1. How many codeblock tests exist?
2. How many are failing?
3. What are the test names?
4. What are the actual vs expected outputs?

**Save output** to a file for analysis:
```bash
rebar3 ct --suite test/erlmd_SUITE --case codeblock_* > codeblock_test_results.txt 2>&1
```

---

## Task 3: Create Minimal Test Cases

**Action**: Create test files to isolate different codeblock scenarios.

### Test Case A: Adjacent Codeblocks

**File**: `test_code_adjacent.md`
```markdown
    line 1
    line 2
```

**Expected output**:
```html
<pre><code>line 1
line 2
</code></pre>
```

### Test Case B: Separated Codeblocks

**File**: `test_code_separated.md`
```markdown
    line 1

    line 2
```

**Expected output** (blank line should be preserved):
```html
<pre><code>line 1

line 2
</code></pre>
```

### Test Case C: Multiple Blanks

**File**: `test_code_multiple_blanks.md`
```markdown
    line 1


    line 2
```

**Expected output** (both blank lines preserved):
```html
<pre><code>line 1


line 2
</code></pre>
```

### Test Case D: Codeblock followed by normal

**File**: `test_code_then_normal.md`
```markdown
    code line

Normal paragraph.
```

**Expected output** (two separate blocks):
```html
<pre><code>code line
</code></pre>

<p>Normal paragraph.</p>
```

---

## Task 4: Test Original vs AST

**Action**: Run each test case through both implementations.

```erlang
test_file(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    Original = erlmd:conv_original(MD),
    AST = erlmd:conv_ast(MD),
    
    io:format("~n=== Testing ~s ===~n", [File]),
    io:format("Original:~n~s~n", [Original]),
    io:format("AST:~n~s~n", [AST]),
    
    case Original =:= AST of
        true -> io:format("✓ MATCH~n");
        false -> 
            io:format("✗ MISMATCH~n"),
            io:format("Original length: ~p~n", [length(Original)]),
            io:format("AST length: ~p~n", [length(AST)]),
            % Find first difference
            Zipped = lists:zip(Original, AST),
            case lists:dropwhile(fun({A, B}) -> A =:= B end, Zipped) of
                [{OC, AC} | _] ->
                    io:format("First diff at char ~p vs ~p~n", [OC, AC]);
                [] ->
                    io:format("Lengths differ but common prefix matches~n")
            end
    end.

% Test all files
test_file("test_code_adjacent.md").
test_file("test_code_separated.md").
test_file("test_code_multiple_blanks.md").
test_file("test_code_then_normal.md").
```

**Capture**: Which test cases fail and how?

---

## Task 5: Analyze grab_empties/1

**Action**: Verify that `grab_empties/1` is working correctly.

**Location**: `src/erlmd_ast.erl`, around line 560

**Current implementation**:
```erlang
grab_empties(List) -> grab_empties1(List, []).

grab_empties1([{linefeed, _} | T], E) ->
    grab_empties1(T, [{{lf,lf}, "\n"} | E]);
grab_empties1([{blank, _} | T], E) ->
    grab_empties1(T, [{{lf,lf}, "\n"} | E]);
grab_empties1(List, E) ->
    {List, E}.
```

**Original implementation** (from `erlmd.erl` line 256):
```erlang
grab_empties(List) -> grab_empties1(List, []).

grab_empties1([{linefeed, _} | T], E) -> grab_empties1(T, [{{lf,lf}, "\n"} | E]);
grab_empties1([{blank, _} | T], E)    -> grab_empties1(T, [{{lf,lf}, "\n"} | E]);
grab_empties1(List, E)                -> {List, E}.
```

**Observation**: These are identical! ✓

**Test it**:
```erlang
% Create a list with empties
TestList = [{blank, []}, {linefeed, []}, {normal, []}].
{Rest, Empties} = erlmd_ast:grab_empties(TestList).
io:format("Rest: ~p~n", [Rest]).
io:format("Empties: ~p~n", [Empties]).
% Should return: Rest = [{normal, []}], Empties = [{{lf,lf}, "\n"}, {{lf,lf}, "\n"}]
```

**If this works correctly**, the issue is in how we use the empties, not in grabbing them.

---

## Task 6: Compare Merging Logic

**Action**: Compare how original vs current implementation merges codeblocks.

### Original Approach

**Adjacent codeblocks**:
```erlang
merge(P1, pad(I), P2)
% Which expands to:
flatten([make_br(P1), {string, pad(I)} | P2])
```

**With empties**:
```erlang
merge(P1, pad(I), E ++ P2)
% Which expands to:
flatten([make_br(P1), {string, pad(I)} | E ++ P2])
```

**Key**: The padding and `make_br` call on P1.

### Current Approach

**Adjacent codeblocks**:
```erlang
lists:flatten([P1 | P2])
```

**With empties**:
```erlang
lists:flatten([P1, E | P2])
```

**Key**: No padding, no `make_br` call.

### Analysis

**Question 1**: Do we need padding in codeblocks?
- In the original, `pad(I)` generates indentation: `"  "`, `"    "`, etc.
- For codeblocks at the root level, `I=0`, so `pad(0) = ""`
- For nested codeblocks (inside lists), `I>0`, so padding is added

**Question 2**: Do we need `make_br` for codeblocks?
- `make_br/1` converts trailing `ws+lf` to `<br />`
- In code blocks, we want to preserve literal whitespace
- So `make_br` might not be needed for codeblocks
- BUT the original uses it anyway

**Hypothesis**: The padding issue might only affect nested code blocks (code blocks inside list items). For root-level code blocks, padding is empty string, so it shouldn't matter.

---

## Task 7: Check make_plain_str/1

**Action**: Verify that `make_plain_str/1` correctly converts tokens to plain text for code blocks.

**Location**: `src/erlmd_ast.erl`, around line 575

**Current implementation**:
```erlang
make_plain_str(List) ->
    make_plain_str(List, []).

make_plain_str([], Acc) ->
    lists:flatten(lists:reverse(Acc));
make_plain_str([{{ws, none}, none} | T], Acc) ->
    make_plain_str(T, [" " | Acc]);
make_plain_str([{_, Str} | T], Acc) when is_list(Str) ->
    make_plain_str(T, [Str | Acc]);
make_plain_str([{_, _} | T], Acc) ->
    make_plain_str(T, Acc).
```

**Original implementation** (from `erlmd.erl` line 853):
```erlang
make_plain_str(List) -> m_plain(List, []).

m_plain([], Acc)                           -> flatten(reverse(Acc));
m_plain([{{ws, none}, none} | T], Acc)     -> m_plain(T, [" " | Acc]);
m_plain([{_, Str} | T], Acc)               -> m_plain(T, [Str | Acc]).
```

**Difference spotted**:
- Original: Always includes `Str` (2nd arg of tuple)
- Current: Only includes if `is_list(Str)`, otherwise skips

**This could be a problem!** If tokens have non-list content (atoms, integers), we skip them.

**Potential fix**:
```erlang
make_plain_str([{_, Str} | T], Acc) when is_list(Str) ->
    make_plain_str(T, [Str | Acc]);
make_plain_str([{_, Str} | T], Acc) when is_atom(Str) ->
    % Skip atoms (like 'none')
    make_plain_str(T, Acc);
make_plain_str([{_, Str} | T], Acc) ->
    % For other types, convert to string
    make_plain_str(T, [io_lib:format("~p", [Str]) | Acc]).
```

Actually, looking at the original, it just does `[Str | Acc]` unconditionally. Let's check if this causes issues.

**Test it**:
```erlang
% Create token list with different types
Tokens = [
    {{ws, sp}, "  "},
    {string, "hello"},
    {{lf, lf}, "\n"},
    {string, "world"}
].
Result = erlmd_ast:make_plain_str(Tokens).
io:format("Result: ~p~n", [Result]).
% Should be: "  hello\nworld"
```

---

## Task 8: Fix Merging to Match Original

**Action**: Update codeblock merging to use proper merge logic.

### Option A: Simple Fix (No Padding)

If tests show that padding isn't the issue:

```erlang
%% Code blocks - adjacent
parse_blocks([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    % Use merge_tokens which applies make_br
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{codeblock, Merged}, S1 ++ S2} | T], Refs, Acc);

%% Code blocks - with empties
parse_blocks([{{codeblock, P1}, S1} | T1], Refs, Acc) ->
    case grab_empties(T1) of
        {[{{codeblock, P2}, S2} | T2], E} ->
            % Merge P1 with E and P2
            Merged = merge_tokens(P1, E ++ P2),
            parse_blocks([{{codeblock, Merged}, S1 ++ E ++ S2} | T2], Refs, Acc);
        {Rest, _} ->
            Content = make_plain_str(snip(P1)),
            CodeBlock = #code_block{content = Content, language = undefined},
            parse_blocks(Rest, Refs, [CodeBlock | Acc])
    end;
```

**Changes**:
1. Use `merge_tokens(P1, P2)` instead of `lists:flatten([P1 | P2])`
2. Use `merge_tokens(P1, E ++ P2)` instead of `lists:flatten([P1, E | P2])`

This ensures `make_br/1` is called on P1, matching the original.

### Option B: Full Original Match (With Padding)

If padding is needed (for nested code blocks in lists):

```erlang
%% Would need to thread indent level through parse_blocks/3
%% This is a bigger change - let's try Option A first
```

---

## Task 9: Verify snip/1

**Action**: Check that `snip/1` correctly removes trailing linefeeds.

**Location**: `src/erlmd_ast.erl`, around line 553

**Current implementation**:
```erlang
snip(List) ->
    List2 = lists:reverse(List),
    case List2 of
        [{{lf, _}, _} | T] -> lists:reverse(T);
        _ -> List
    end.
```

**Original implementation** (from `erlmd.erl` line 848):
```erlang
snip(List) -> List2 = reverse(List),
              case List2 of
                  [{{lf, _}, _} | T] -> lists:reverse(T);
                  _                  -> List
              end.
```

**Observation**: These are identical! ✓

---

## Task 10: Apply Fixes

**Action**: Implement Option A (simple fix with merge_tokens).

**File**: `src/erlmd_ast.erl`  
**Location**: Lines 219-228 (approximately)

**Change from**:
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

**Change to**:
```erlang
%% Code blocks
parse_blocks([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{codeblock, Merged}, S1 ++ S2} | T], Refs, Acc);

parse_blocks([{{codeblock, P1}, S1} | T1], Refs, Acc) ->
    case grab_empties(T1) of
        {[{{codeblock, P2}, S2} | T2], E} ->
            Merged = merge_tokens(P1, E ++ P2),
            parse_blocks([{{codeblock, Merged}, S1 ++ E ++ S2} | T2], Refs, Acc);
        {Rest, _} ->
            Content = make_plain_str(snip(P1)),
            CodeBlock = #code_block{content = Content, language = undefined},
            parse_blocks(Rest, Refs, [CodeBlock | Acc])
    end;
```

**Rationale**: 
- `merge_tokens/2` calls `make_br/1` on the first token list
- This matches the original's behavior more closely
- Simple change, easy to test

---

## Task 11: Test the Fixes

**Action**: Run tests after applying fixes.

### Test Individual Cases

```erlang
% Test each case
test_file("test_code_adjacent.md").
test_file("test_code_separated.md").
test_file("test_code_multiple_blanks.md").
test_file("test_code_then_normal.md").
```

**Expected**: All should now match between Original and AST.

### Run Full Test Suite

```bash
rebar3 ct --suite test/erlmd_SUITE --case codeblock_*
```

**Expected**: Codeblock tests should pass (or most of them).

### Run Full Suite

```bash
rebar3 ct
```

**Check**:
1. How many tests pass now?
2. Did codeblock tests improve?
3. Any regressions?

---

## Task 12: Edge Cases

**Action**: Test edge cases that might still fail.

### Edge Case 1: Code block with hard breaks

```markdown
    line 1  
    line 2
```

The two spaces before newline should NOT create `<br />` in code blocks (they're literal).

**Expected**: The spaces should be preserved as-is in the code block.

### Edge Case 2: Empty code block

```markdown
    
```

Just whitespace - should this be a code block?

### Edge Case 3: Code block in list

```markdown
* List item

      code in list
```

This is a nested code block (inside a list item). The original uses padding here.

**If this fails**, we might need Option B (threading indent level).

---

## Task 13: Document Findings

**Action**: Create a summary report.

```markdown
## Phase 2 Results

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Changes made**:
1. Changed adjacent codeblock merging to use `merge_tokens(P1, P2)`
2. Changed separated codeblock merging to use `merge_tokens(P1, E ++ P2)`

**Root cause**:
The issue was [describe what was wrong].

**Why the fix works**:
[Explain why using merge_tokens fixes it]

**Remaining issues**:
[List any codeblock tests still failing]

**Edge cases tested**:
- ✓/✗ Adjacent codeblocks
- ✓/✗ Separated codeblocks (single blank)
- ✓/✗ Separated codeblocks (multiple blanks)
- ✓/✗ Code block followed by normal
- ✓/✗ Code block with hard breaks
- ✓/✗ Code block in list (nested)

**Next steps**:
- [If all pass]: Move to Phase 3 (Hard Line Breaks)
- [If nested code fails]: Implement padding support (Option B)
- [If other issues]: Investigate [specific problem]
```

---

## Success Criteria

Phase 2 is complete when:

- ✅ Adjacent code blocks merge correctly
- ✅ Code blocks separated by blank lines merge correctly
- ✅ Multiple blank lines between code blocks are preserved
- ✅ Code blocks followed by normal text don't merge
- ✅ All codeblock-related tests pass
- ✅ No regressions in other tests
- ✅ At least 3-5 additional tests passing overall

---

## Troubleshooting Guide

### Issue: Blank lines disappear between code blocks

**Cause**: Empties aren't being included in the merge.

**Check**: Verify that `E` (empties) is actually in the merged result:
```erlang
Merged = merge_tokens(P1, E ++ P2),
io:format("E is: ~p~n", [E]),
io:format("Merged is: ~p~n", [Merged]).
```

### Issue: Code blocks not merging at all

**Cause**: The consumption clauses might not be matching.

**Check**: Add debug output:
```erlang
parse_blocks([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    io:format("MERGING ADJACENT CODEBLOCKS~n"),
    ...
```

If you don't see this output, the pattern isn't matching. Check the typed lines.

### Issue: Wrong whitespace in code blocks

**Cause**: `make_plain_str/1` might be handling whitespace incorrectly.

**Check**: Test the token-to-string conversion:
```erlang
Tokens = [{{ws, sp}, "    "}, {string, "code"}],
Result = erlmd_ast:make_plain_str(Tokens).
% Should be: "    code"
```

### Issue: Nested code blocks in lists fail

**Symptom**: Root-level code blocks work, but code in list items fails.

**Cause**: Missing padding for nested code blocks.

**Solution**: Need to implement Option B (threading indent level through `parse_blocks/3`). This is a bigger change - document it and possibly defer to a later phase.

---

## Reference Code

### Original codeblock consumption (erlmd.erl lines 211-218)

```erlang
%% codeblock consumes any following empty lines
%% and other codeblocks
p1([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], R, I, Acc) ->
    p1([{{codeblock, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
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

### Original merge/3 (erlmd.erl line 263)

```erlang
merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    flatten([NewP1, {string, Pad} | P2]).
```

### HTML encoding

The original uses `htmlencode/1` on code block content:

```erlang
htmlencode(List) ->
    htmlencode(List, []).

htmlencode([], Acc) ->
    lists:flatten(lists:reverse(Acc));
htmlencode([$&   | Rest], Acc) -> htmlencode(Rest, ["&amp;" | Acc]);
htmlencode([$<   | Rest], Acc) -> htmlencode(Rest, ["&lt;" | Acc]);
htmlencode([$>   | Rest], Acc) -> htmlencode(Rest, ["&gt;" | Acc]);
htmlencode([160  | Rest], Acc) -> htmlencode(Rest, ["&nbsp;" | Acc]);
htmlencode([Else | Rest], Acc) -> htmlencode(Rest, [Else | Acc]).
```

Our AST version does this in the renderer (`erlmd_html.erl`), not during parsing. This should be fine.

---

## Key Insights

1. **Codeblocks are simpler than lists**: No tight/loose mode, no complex item parsing
2. **The main issue is merging**: Using proper merge logic (with `make_br`) vs simple flattening
3. **Empties are key**: Blank lines between code blocks must be preserved as content
4. **Padding might matter**: But probably only for nested code blocks in lists

---

## Deliverables

After completing Phase 2, provide:

1. **Test results**: Before and after numbers, specific codeblock tests
2. **Code changes**: Exact diffs of what changed
3. **Test file results**: Output from the 4 test files created
4. **Analysis**: Root cause and why the fix works
5. **Edge case results**: Which edge cases pass/fail
6. **Remaining issues**: Any codeblock tests still failing and hypothesis about why

This information will guide Phase 3 (Hard Line Breaks).

---

## Notes

**Relationship to Phase 1**: If Phase 1 fixed `merge_tokens/2` and `make_br/1`, Phase 2 should be straightforward - just using those functions correctly for codeblocks.

**Relationship to Phase 3**: Hard line breaks are about detecting `  \n` patterns. In code blocks, these should be literal (not converted to `<br />`), so Phase 3 might need special handling for code blocks.

**Simplicity**: Phase 2 should be simpler than Phase 1 because there's no tight/loose mode or item parsing - just merging token lists.
