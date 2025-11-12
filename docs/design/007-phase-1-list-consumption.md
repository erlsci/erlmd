# Phase 1: List Consumption Logic - Implementation Instructions for Claude Code

## Overview

**Objective**: Verify and fix list consumption logic in `erlmd_ast.erl` to match the original `erlmd.erl` behavior where lists "swallow" following normal and codeblock lines.

**Estimated Time**: 2-3 hours  
**Expected Impact**: Fixes 11-14 failing tests  
**Priority**: CRITICAL

---

## Context

The original markdown parser processes lists by merging following normal lines and codeblocks into the list item BEFORE creating any HTML. This is a preprocessing step that happens at the typed-line level.

When we see:
```markdown
* List item
  continuation line
```

The typed lines are:
```erlang
[{{ul, Tokens1}, OrigLine1}, {normal, Tokens2}, ...]
```

The original parser merges these into:
```erlang
[{{ul, MergedTokens}, OrigLine1 ++ OrigLine2}, ...]
```

THEN it processes the merged list item.

Our AST builder needs to do the same merging before creating list nodes.

---

## Current State Analysis

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_blocks/3`  
**Lines**: 200-217

The code ALREADY HAS consumption clauses:

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

**Problem**: These clauses exist but tests are still failing. We need to investigate WHY.

---

## Task 1: Run Tests and Identify Failures

**Action**: Run the test suite and identify which list-related tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct
```

**Expected Output**: Around 256 passing, 40 failing (approximate)

**Then** run list-specific tests:

```bash
rebar3 ct --suite test/erlmd_SUITE --case ordered_list_*
rebar3 ct --suite test/erlmd_SUITE --case unordered_list_*
```

**Capture**:
1. How many list tests fail?
2. What are the test names?
3. What are the actual vs expected outputs?

**Save this information** - we need it to understand what's wrong.

---

## Task 2: Compare Original vs AST Output

**Action**: For one failing list test, compare the outputs.

Create a test file `test_list.md`:
```markdown
* Item 1
  continuation
* Item 2
```

Then in Erlang shell:
```erlang
{ok, Content} = file:read_file("test_list.md").
MD = binary_to_list(Content).

%% Get original output
Original = erlmd:conv_original(MD).
io:format("ORIGINAL:~n~s~n~n", [Original]).

%% Get AST output
AST = erlmd:conv_ast(MD).
io:format("AST:~n~s~n~n", [AST]).

%% Compare
case Original =:= AST of
    true -> io:format("MATCH!~n");
    false -> io:format("MISMATCH!~n"),
             io:format("Diff length: ~p vs ~p~n", [length(Original), length(AST)])
end.
```

**Capture**:
1. What does Original produce?
2. What does AST produce?
3. Where do they differ?

---

## Task 3: Verify merge_tokens/2 Implementation

**Action**: Check that `merge_tokens/2` matches the original `merge/3` behavior.

**Location**: `src/erlmd_ast.erl`, around line 568

**Current implementation**:
```erlang
merge_tokens(P1, P2) ->
    lists:flatten([P1, {string, " "} | P2]).
```

**Original implementation** (from `erlmd.erl` line 263):
```erlang
merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    flatten([NewP1, {string, Pad} | P2]).
```

**Key differences**:
1. Original calls `make_br/1` on P1 first (converts trailing whitespace+LF to `<br />`)
2. Original takes a `Pad` parameter (spacing/indentation)
3. We always use `" "` as padding

**Investigation needed**:
- Does the lack of `make_br` call cause issues?
- Should we be using variable padding instead of `" "`?
- Do we need to match the original's padding behavior?

**Action**: Look at how the original `p1/4` calls `merge/3`:

```erlang
%% From erlmd.erl line 196
p1([{{ul, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
```

The `pad(I)` generates indentation based on nesting level `I`.

**Possible fix needed**:
```erlang
merge_tokens(P1, P2) ->
    NewP1 = make_br(P1),  % Add hard break detection
    lists:flatten([NewP1, {string, " "} | P2]).  % Or use proper padding?
```

---

## Task 4: Check make_br/1 Implementation

**Action**: Verify that `make_br/1` exists and works correctly.

**Location**: Should be in `erlmd_ast.erl` around line 570

**Original implementation** (from `erlmd.erl` line 265):
```erlang
make_br(List) -> make_br1(reverse(List)).

make_br1([{{lf, _}, _}, {{ws, comp}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1([{{lf, _}, _}, {{ws, tab}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1(List) -> 
    reverse(List).
```

**Current implementation** (from `erlmd_ast.erl` line 570):
```erlang
make_br(List) ->
    case lists:reverse(List) of
        [{{lf, _}, _}, {{ws, comp}, _} | T] ->
            lists:reverse([{tags, "<br /> "} | T]);
        [{{lf, _}, _}, {{ws, tab}, _} | T] ->
            lists:reverse([{tags, "<br /> "} | T]);
        _ ->
            List
    end.
```

**Differences spotted**:
1. Original: `" <br />\n"` (space before, newline after)
2. Current: `"<br /> "` (no space before, space after, no newline)

**This might be significant!** The whitespace and newline placement could affect parsing.

**Potential fix**:
```erlang
make_br(List) ->
    case lists:reverse(List) of
        [{{lf, _}, _}, {{ws, comp}, _} | T] ->
            lists:reverse([{tags, " <br />\n"} | T]);  % Match original exactly
        [{{lf, _}, _}, {{ws, tab}, _} | T] ->
            lists:reverse([{tags, " <br />\n"} | T]);  % Match original exactly
        _ ->
            List
    end.
```

---

## Task 5: Investigate Clause Ordering

**Action**: Verify that consumption clauses come BEFORE general list parsing clauses.

**In `erlmd_ast.erl`**:
- Consumption clauses should be at lines 200-217
- General list parsing should be at lines 219-225 (approx)

**Check order**:
```erlang
%% These MUST come first:
parse_blocks([{{ul, P1}, S1}, {normal, P2} | T], Refs, Acc) -> ...
parse_blocks([{{ul, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) -> ...
parse_blocks([{{ol, P1}, S1}, {normal, P2} | T], Refs, Acc) -> ...
parse_blocks([{{ol, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) -> ...

%% These come AFTER:
parse_blocks([{{ul, _P}, _} | _T] = List, Refs, Acc) -> ...
parse_blocks([{{ol, _P}, _} | _T] = List, Refs, Acc) -> ...
```

If the order is wrong, Erlang will match the general clause first and never try merging.

**Verify**: Use `grep -n "parse_blocks(\[\{\{ul" src/erlmd_ast.erl` to see all ul clauses.

---

## Task 6: Test with Specific Example

**Action**: Create a minimal test case to isolate the issue.

**Test file**: `test_list_continuation.md`
```markdown
* First item
  with continuation
* Second item
```

**Expected original behavior**:
```html
<ul>
<li>First item with continuation</li>
<li>Second item</li>
</ul>
```

**Run comparison**:
```erlang
{ok, C} = file:read_file("test_list_continuation.md").
Orig = erlmd:conv_original(binary_to_list(C)).
New = erlmd:conv_ast(binary_to_list(C)).
io:format("Original:~n~s~n~n", [Orig]).
io:format("AST:~n~s~n~n", [New]).
Orig =:= New.
```

**If they differ**, examine the AST:
```erlang
Lex = erlmd:lex(binary_to_list(C)).
Lines = erlmd:make_lines(Lex).
{TypedLines, Refs} = erlmd:type_lines(Lines).
io:format("Typed lines:~n~p~n", [TypedLines]).
```

This will show you the exact typed lines being passed to `parse_blocks/3`.

---

## Task 7: Apply Fixes

Based on findings from Tasks 1-6, apply necessary fixes:

### Fix 1: Update make_br/1 (if needed)

**File**: `src/erlmd_ast.erl`  
**Location**: Around line 570

**Change**:
```erlang
make_br(List) ->
    case lists:reverse(List) of
        [{{lf, _}, _}, {{ws, comp}, _} | T] ->
            lists:reverse([{tags, " <br />\n"} | T]);  % Match original
        [{{lf, _}, _}, {{ws, tab}, _} | T] ->
            lists:reverse([{tags, " <br />\n"} | T]);  % Match original
        _ ->
            List
    end.
```

### Fix 2: Update merge_tokens/2 (if needed)

**File**: `src/erlmd_ast.erl`  
**Location**: Around line 568

**Change**:
```erlang
merge_tokens(P1, P2) ->
    NewP1 = make_br(P1),  % Apply hard break detection first
    lists:flatten([NewP1, {string, " "} | P2]).
```

### Fix 3: Consider padding

If tests still fail, we might need proper padding. The original uses `pad(I)` which generates indentation based on nesting level.

**Current**: We use `" "` (single space)  
**Original**: Uses `pad(I)` which generates `"  "`, `"    "`, etc. based on nesting

**Potential fix** (more complex):
```erlang
%% Would need to thread indent level through parse_blocks
merge_tokens(P1, P2, Indent) ->
    NewP1 = make_br(P1),
    Padding = lists:duplicate(Indent * 2, $ ),  % 2 spaces per indent level
    lists:flatten([NewP1, {string, Padding} | P2]).
```

**But**: This would require changing the signature of `parse_blocks/3` to track indent level, which is a bigger change. Try simpler fixes first.

---

## Task 8: Run Tests Again

After applying fixes:

```bash
rebar3 ct
```

**Check**:
1. How many tests pass now?
2. Did list tests improve?
3. Did we introduce any regressions?

**Compare**:
- Before fixes: X passing, Y failing
- After fixes: A passing, B failing
- Net improvement: (A - X) tests fixed

**Target**: At least 10-15 more tests passing (out of ~40 failing)

---

## Task 9: Document Findings

**Create a summary**:

```markdown
## Phase 1 Results

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Changes made**:
1. [Describe fix 1]
2. [Describe fix 2]

**Remaining issues**:
1. [List any list tests still failing]
2. [Note any patterns in failures]

**Next steps**:
- [If all list tests pass]: Move to Phase 2 (Codeblock Consumption)
- [If some still fail]: Investigate [specific issue]
```

---

## Success Criteria

Phase 1 is complete when:

- ✅ All unordered list tests pass
- ✅ All ordered list tests pass  
- ✅ Lists with continuation lines render correctly
- ✅ Lists with code blocks render correctly
- ✅ No regressions in other tests
- ✅ At least 10 additional tests passing overall

---

## Troubleshooting Guide

### Issue: Tests still failing after fixes

**Check**:
1. Are consumption clauses in the right order?
2. Is `merge_tokens/2` being called with correct arguments?
3. Are the tokens being merged correctly?

**Debug approach**:
```erlang
%% Add debug output to parse_blocks/3
parse_blocks([{{ul, P1}, S1}, {normal, P2} | T], Refs, Acc) ->
    io:format("MERGING UL+NORMAL:~n  P1=~p~n  P2=~p~n", [P1, P2]),
    Merged = merge_tokens(P1, P2),
    io:format("  Merged=~p~n", [Merged]),
    parse_blocks([{{ul, Merged}, S1} | T], Refs, Acc);
```

### Issue: Wrong HTML output format

**Check**: Compare character-by-character with original:
```erlang
Original = "...",
AST = "...",
lists:zip(Original, AST).  % Shows first difference
```

### Issue: Padding seems wrong

The original uses context-aware padding based on nesting level. We might need to track this if simple space doesn't work.

---

## Reference Code

### Original merge/3 (from erlmd.erl)

```erlang
merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    flatten([NewP1, {string, Pad} | P2]).
```

### Original list consumption (from erlmd.erl lines 196-210)

```erlang
%% unordered lists swallow normal and codeblock lines
p1([{{ul, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ul, P1}, S1}, {{codeblock, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ul, _P}, _} | _T] = List, R, I, Acc) ->
    {Rest, NewAcc} = parse_list(ul, List, R, I, [], false),
    p1(Rest, R, I,  [pad(I) ++ "<ul>\n" ++ NewAcc
                           ++ pad(I) ++ "</ul>\n" | Acc]);

%% ordered lists swallow normal and codeblock lines
p1([{{ol, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ol, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ol, P1}, S1}, {{codeblock, P2}, S2} | T], R, I , Acc) ->
    p1([{{ol, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{ol, _P}, _} | _T] = List, R, I, Acc) ->
    {Rest, NewAcc} = parse_list(ol, List, R, I, [], false),
    p1(Rest, R, I,  [pad(I) ++ "<ol>\n" ++ NewAcc
                           ++ pad(I) ++ "</ol>\n" | Acc]);
```

### Original pad/1 (from erlmd.erl line 270)

```erlang
pad(N) -> pad1(N, []).

pad1(0, Acc)            -> Acc;
pad1(N, Acc) when N > 0 -> pad1(N - 1, ["  " | Acc]).
```

This generates 2 spaces per indent level.

---

## Final Notes

**Important**: The consumption logic is the foundation for correct list rendering. If this doesn't work, downstream issues (like tight/loose detection in Phase 4) won't work either.

**Be methodical**: Test after each small change. Don't change multiple things at once or you won't know what fixed (or broke) the tests.

**Document everything**: When you find the issue, document it clearly so we understand the root cause for future reference.

---

## Deliverables

After completing Phase 1, provide:

1. **Test results**: Before and after numbers
2. **Code changes**: Exact diffs of what was changed
3. **Analysis**: What was wrong and why the fix works
4. **Remaining issues**: Any list tests still failing and why

This information will guide Phase 2 and subsequent phases.
