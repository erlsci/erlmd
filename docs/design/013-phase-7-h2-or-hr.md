# Phase 7: h2_or_hr Handling - Implementation Instructions for Claude Code

## Overview

**Objective**: Verify and fix the handling of `h2_or_hr` typed lines, which can become either setext h2 headers (when preceded by text) or horizontal rules (when standalone).

**Estimated Time**: 30 minutes - 1 hour  
**Expected Impact**: Fixes 1-2 failing tests  
**Priority**: LOW  
**Prerequisites**: Phases 1-6 should be complete

---

## Context

The `h2_or_hr` line type represents a line of dashes (`---`), which is ambiguous in markdown:

### When It's a Setext H2 Header

**Input**:
```markdown
Header Text
---
```

**Expected output**:
```html
<h2>Header Text</h2>
```

### When It's a Horizontal Rule

**Input**:
```markdown
---
```

**Expected output**:
```html
<hr />
```

### Greedy Consumption

The `h2_or_hr` type is "greedy" - if followed by a normal line, it consumes that line and becomes a normal paragraph:

**Input**:
```markdown
---
Following text
```

**Expected output** (merged into paragraph):
```html
<p>--- Following text</p>
```

---

## Current State Analysis

### What's Already Implemented

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_blocks/3`  
**Lines**: 120-137 and 233-237

```erlang
%% Normal line followed by h2_or_hr (setext h2)
parse_blocks([{normal, P}, {h2_or_hr, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 2, content = Content, type = setext},
    parse_blocks(T, Refs, [Header | Acc]);

%% Blockquote followed by h2_or_hr
parse_blocks([{blockquote, P}, {h2_or_hr, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 2, content = Content, type = setext},
    parse_blocks(T, Refs, [Header | Acc]);

%% Codeblock followed by h2_or_hr
parse_blocks([{{codeblock, P}, _}, {h2_or_hr, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 2, content = Content, type = setext},
    parse_blocks(T, Refs, [Header | Acc]);

% ... later in the file ...

%% h2_or_hr is greedy for normal lines
parse_blocks([{h2_or_hr, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{normal, Merged} | T], Refs, Acc);

%% Standalone h2_or_hr becomes hr
parse_blocks([{h2_or_hr, _} | T], Refs, Acc) ->
    parse_blocks(T, Refs, [#horizontal_rule{} | Acc]);
```

### The Original Behavior

**Reference**: `src/erlmd.erl` lines 153-172 and 226-228

```erlang
%% setext h2 might be a look behind
p1([{normal, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    P2 = string:strip(make_str(snip(P), R), both, ?SPACE),
    p1(T, R, I, [pad(I) ++ "<h2>" ++ P2 ++ "</h2>\n\n" | Acc]);

%% blockquotes can become setext h2
p1([{blockquote, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ make_str(snip(P), R)
                        ++ "</h2>\n\n" | Acc]);
p1([{{codeblock, P}, _}, {h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ make_str(snip(P), R)
                        ++ "</h2>\n\n" | Acc]);

% ... later ...

%% h2_or_hr is greedy for normal lines
p1([{h2_or_hr, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{normal, flatten([P1 | P2])} | T], R, I, Acc);
%% the clause with a normal before an 'h2_or_hr' has already been
%% handled further up the tree, so this is a bona fide 'hr'...
p1([{h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  ["<hr />" | Acc]);
```

**Key observations**:

1. **Look-behind for h2**: Normal/blockquote/codeblock followed by `h2_or_hr` becomes h2 header
2. **Greedy consumption**: `h2_or_hr` followed by normal merges into normal
3. **Standalone becomes hr**: If neither look-behind nor look-ahead, it's a horizontal rule

**Our implementation matches this!** Let's verify it's working correctly.

---

## Task 1: Run h2_or_hr Tests

**Action**: Identify which h2_or_hr tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct --suite test/erlmd_SUITE | grep -i "h2\|hr\|header\|horizontal"
```

**Also search for setext tests**:
```bash
grep -rn "setext\|h2_or_hr\|---" test/erlmd_SUITE.erl | grep "test_"
```

**Capture**:
1. How many h2_or_hr related tests exist?
2. Which are failing?
3. What's the expected vs actual output?

---

## Task 2: Create Minimal Test Cases

**Action**: Create test files to isolate h2_or_hr scenarios.

### Test Case A: Setext H2 (Look-behind)

**File**: `test_h2_setext.md`
```markdown
Header Text
---
```

**Expected output**:
```html
<h2>Header Text</h2>
```

### Test Case B: Standalone HR

**File**: `test_hr_standalone.md`
```markdown
---
```

**Expected output**:
```html
<hr />
```

### Test Case C: Greedy Consumption (h2_or_hr + normal)

**File**: `test_h2_hr_greedy.md`
```markdown
---
Following text
```

**Expected output** (merged into paragraph):
```html
<p>--- Following text</p>
```

### Test Case D: Multiple Dashes

**File**: `test_hr_multiple.md`
```markdown
-----
```

**Expected output**:
```html
<hr />
```

### Test Case E: Setext H2 with Long Text

**File**: `test_h2_setext_long.md`
```markdown
This is a longer header that should become h2
---
```

**Expected output**:
```html
<h2>This is a longer header that should become h2</h2>
```

### Test Case F: Blockquote Followed by Dashes

**File**: `test_blockquote_h2.md`
```markdown
> Quote text
---
```

**Expected output** (blockquote becomes h2):
```html
<h2>Quote text</h2>
```

### Test Case G: HR Between Paragraphs

**File**: `test_hr_between_paras.md`
```markdown
First paragraph.

---

Second paragraph.
```

**Expected output**:
```html
<p>First paragraph.</p>

<hr />

<p>Second paragraph.</p>
```

---

## Task 3: Test Original vs AST

**Action**: Run each test case through both implementations.

```erlang
test_h2_hr(File) ->
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
            % Check what type of element was created
            case {string:str(Original, "<h2>"), string:str(AST, "<h2>")} of
                {N, 0} when N > 0 -> 
                    io:format("Original has <h2>, AST doesn't~n");
                {0, N} when N > 0 -> 
                    io:format("AST has <h2>, Original doesn't~n");
                _ -> ok
            end,
            case {string:str(Original, "<hr"), string:str(AST, "<hr")} of
                {N2, 0} when N2 > 0 -> 
                    io:format("Original has <hr>, AST doesn't~n");
                {0, N2} when N2 > 0 -> 
                    io:format("AST has <hr>, Original doesn't~n");
                _ -> ok
            end
    end.

% Test all files
test_h2_hr("test_h2_setext.md").
test_h2_hr("test_hr_standalone.md").
test_h2_hr("test_h2_hr_greedy.md").
test_h2_hr("test_hr_multiple.md").
test_h2_hr("test_h2_setext_long.md").
test_h2_hr("test_blockquote_h2.md").
test_h2_hr("test_hr_between_paras.md").
```

**Capture**: Which test cases fail and how?

---

## Task 4: Check Clause Ordering

**Action**: Verify that `h2_or_hr` clauses are in the correct order.

**Critical order**:
1. **Look-behind clauses** (normal/blockquote/codeblock + h2_or_hr) - FIRST
2. **Greedy consumption** (h2_or_hr + normal) - MIDDLE
3. **Standalone hr** (just h2_or_hr) - LAST

**Current order in code**:
- Lines 120-137: Look-behind clauses ✓
- Lines 233-237: Greedy and standalone clauses ✓

**This looks correct!** The order matches the original.

**Verify with grep**:
```bash
grep -n "h2_or_hr" src/erlmd_ast.erl
```

Should show:
```
120:parse_blocks([{normal, P}, {h2_or_hr, _} | T], Refs, Acc) ->
125:parse_blocks([{blockquote, P}, {h2_or_hr, _} | T], Refs, Acc) ->
130:parse_blocks([{{codeblock, P}, _}, {h2_or_hr, _} | T], Refs, Acc) ->
233:parse_blocks([{h2_or_hr, P1}, {normal, P2} | T], Refs, Acc) ->
237:parse_blocks([{h2_or_hr, _} | T], Refs, Acc) ->
```

If the standalone clause (line 237) comes BEFORE the greedy clause (line 233), that's a problem!

---

## Task 5: Trace h2_or_hr Processing

**Action**: Add debug output to see which clauses match.

```erlang
%% Add debug to each h2_or_hr clause

%% Normal + h2_or_hr (setext h2)
parse_blocks([{normal, P}, {h2_or_hr, _} | T], Refs, Acc) ->
    io:format("DEBUG: normal + h2_or_hr -> setext h2~n"),
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 2, content = Content, type = setext},
    parse_blocks(T, Refs, [Header | Acc]);

%% h2_or_hr + normal (greedy consumption)
parse_blocks([{h2_or_hr, P1}, {normal, P2} | T], Refs, Acc) ->
    io:format("DEBUG: h2_or_hr + normal -> merge to normal~n"),
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{normal, Merged} | T], Refs, Acc);

%% Standalone h2_or_hr (hr)
parse_blocks([{h2_or_hr, _} | T], Refs, Acc) ->
    io:format("DEBUG: standalone h2_or_hr -> hr~n"),
    parse_blocks(T, Refs, [#horizontal_rule{} | Acc]);
```

**Test each scenario**:
```erlang
{ok, C} = file:read_file("test_h2_setext.md").
erlmd:conv_ast(binary_to_list(C)).
% Should see: "DEBUG: normal + h2_or_hr -> setext h2"

{ok, C2} = file:read_file("test_hr_standalone.md").
erlmd:conv_ast(binary_to_list(C2)).
% Should see: "DEBUG: standalone h2_or_hr -> hr"

{ok, C3} = file:read_file("test_h2_hr_greedy.md").
erlmd:conv_ast(binary_to_list(C3)).
% Should see: "DEBUG: h2_or_hr + normal -> merge to normal"
```

---

## Task 6: Check for Potential Issues

Based on the code review and debug output, identify any issues:

### Issue A: Clause Order Wrong

**Symptom**: Greedy consumption doesn't work (h2_or_hr becomes hr instead of merging).

**Cause**: Standalone clause comes before greedy clause.

**Fix**: Move standalone clause to the very end, after greedy.

### Issue B: Look-behind Not Working

**Symptom**: Normal + h2_or_hr becomes hr instead of h2.

**Cause**: Look-behind clause might not be matching, or is after standalone clause.

**Fix**: Ensure look-behind clauses are first.

### Issue C: Whitespace Handling

**Symptom**: h2 headers have extra spaces.

**Cause**: Not stripping spaces from header text.

**Check**: The original does:
```erlang
P2 = string:strip(make_str(snip(P), R), both, ?SPACE),
```

We do:
```erlang
Content = build_inline(snip(P), Refs),
```

The renderer should strip spaces. Check `erlmd_html.erl` line 16:
```erlang
render_block(#header{level = Level, content = Content}) ->
    Tag = "h" ++ integer_to_list(Level),
    InlineHTML = lists:map(fun render_inline/1, Content),
    ContentStr = string:strip(lists:flatten(InlineHTML), right),
    "<" ++ Tag ++ ">" ++ ContentStr ++ "</" ++ Tag ++ ">\n\n";
```

We only strip RIGHT, not both sides. The original strips both.

**Potential fix**:
```erlang
ContentStr = string:strip(string:strip(lists:flatten(InlineHTML), left), right),
```

But actually, this should be fine. The paragraph renderer already does both:
```erlang
Stripped = string:strip(string:strip(ContentStr, left, $ ), right, $ ),
```

And headers should match. Let me check if there's an issue...

Actually, the original only strips for the NORMAL + h2_or_hr case:
```erlang
p1([{normal, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    P2 = string:strip(make_str(snip(P), R), both, ?SPACE),
```

For blockquote and codeblock cases, it doesn't strip:
```erlang
p1([{blockquote, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ make_str(snip(P), R) ++ "</h2>\n\n" | Acc]);
```

So we should only strip for normal + h2_or_hr. But we strip all headers in the renderer, which is probably fine.

---

## Task 7: Verify Current Implementation

**Action**: Check if the current implementation actually works correctly.

Looking at the code again (lines 120-137 and 233-237), the implementation looks correct:

1. ✅ Look-behind clauses are first
2. ✅ Greedy clause comes before standalone
3. ✅ Standalone is last

**This should work!** If tests are failing, it's likely:
- A different issue (wrong HTML output format)
- OR clause ordering in the actual file is different than expected

**Verify actual order**:
```bash
grep -n "parse_blocks\(\[{h2_or_hr" src/erlmd_ast.erl
grep -n "parse_blocks\(\[{normal.*h2_or_hr" src/erlmd_ast.erl
```

---

## Task 8: Check Edge Cases

### Edge Case A: Empty Line Before h2_or_hr

**Input**:
```markdown
Text

---
```

**Expected**: Should be hr (not h2), because of the blank line.

**Current behavior**: The blank line would be a separate typed line, so we'd have:
```erlang
[{normal, ...}, {blank, ...}, {h2_or_hr, ...}]
```

The look-behind clause won't match because there's a blank in between. Good!

### Edge Case B: h2_or_hr at Start of Document

**Input**:
```markdown
---
```

**Expected**: HR

**Current behavior**: Standalone clause should match. Good!

### Edge Case C: Multiple h2_or_hr Lines

**Input**:
```markdown
---
---
```

**Expected**: Two HRs

**Current behavior**: 
- First `h2_or_hr` + second `h2_or_hr`
- Does greedy clause match? It's `[{h2_or_hr, P1}, {normal, P2} | T]`
- Second line is `{h2_or_hr, ...}` not `{normal, ...}`
- So greedy doesn't match, first becomes hr
- Then second becomes hr
- Result: Two HRs ✓

---

## Task 9: Apply Any Necessary Fixes

Based on testing, apply fixes if needed.

### If Clause Order is Wrong

**File**: `src/erlmd_ast.erl`

**Ensure this order**:
```erlang
% 1. Look-behind clauses (FIRST)
parse_blocks([{normal, P}, {h2_or_hr, _} | T], Refs, Acc) -> ...
parse_blocks([{blockquote, P}, {h2_or_hr, _} | T], Refs, Acc) -> ...
parse_blocks([{{codeblock, P}, _}, {h2_or_hr, _} | T], Refs, Acc) -> ...

% 2. Greedy consumption (MIDDLE)
parse_blocks([{h2_or_hr, P1}, {normal, P2} | T], Refs, Acc) -> ...

% 3. Standalone hr (LAST)
parse_blocks([{h2_or_hr, _} | T], Refs, Acc) -> ...
```

**If the standalone clause is elsewhere**, move it to be AFTER the greedy clause.

### If Whitespace Handling is Wrong

**File**: `erlmd_html.erl`  
**Function**: `render_block/1` for headers (line 16)

**Current**:
```erlang
ContentStr = string:strip(lists:flatten(InlineHTML), right),
```

**Change to** (strip both sides):
```erlang
ContentStr = string:strip(string:strip(lists:flatten(InlineHTML), left, $ ), right, $ ),
```

---

## Task 10: Test the Fixes

**Action**: Run all h2_or_hr test cases.

```erlang
test_h2_hr("test_h2_setext.md").
test_h2_hr("test_hr_standalone.md").
test_h2_hr("test_h2_hr_greedy.md").
test_h2_hr("test_hr_multiple.md").
test_h2_hr("test_h2_setext_long.md").
test_h2_hr("test_blockquote_h2.md").
test_h2_hr("test_hr_between_paras.md").
```

**Expected**: All should match between Original and AST.

**Run full test suite**:
```bash
rebar3 ct
```

**Check**:
1. How many tests pass now?
2. Any h2_or_hr tests fixed?
3. Any regressions?

---

## Task 11: Document Findings

**Action**: Create a summary report.

```markdown
## Phase 7 Results

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Changes made**:
[If any changes were needed, list them. Otherwise:]
No changes were needed - the implementation was already correct.

**Verification**:
Verified that:
1. Look-behind clauses are in correct order (first)
2. Greedy consumption clause is before standalone
3. Standalone hr clause is last
4. All clause patterns match the original's behavior

**Test results**:
- ✓/✗ Setext h2 (look-behind)
- ✓/✗ Standalone hr
- ✓/✗ Greedy consumption
- ✓/✗ Multiple dashes
- ✓/✗ Long header text
- ✓/✗ Blockquote + dashes = h2
- ✓/✗ HR between paragraphs

**Remaining issues**:
[List any h2_or_hr tests still failing]

**Next steps**:
- [If all pass]: Move to Phase 8 (Tag Handling)
- [If some fail]: Investigate [specific issue]
```

---

## Success Criteria

Phase 7 is complete when:

- ✅ Normal + h2_or_hr becomes setext h2
- ✅ Blockquote + h2_or_hr becomes setext h2
- ✅ Codeblock + h2_or_hr becomes setext h2
- ✅ h2_or_hr + normal merges into paragraph (greedy)
- ✅ Standalone h2_or_hr becomes hr
- ✅ All h2_or_hr tests pass
- ✅ No regressions in other tests
- ✅ At least 1-2 additional tests passing overall

---

## Troubleshooting Guide

### Issue: Setext h2 not working

**Symptom**: Normal + h2_or_hr becomes hr instead of h2.

**Cause**: Look-behind clause not matching, or standalone clause matching first.

**Check**: Clause order. Look-behind must be BEFORE standalone.

**Debug**:
```erlang
parse_blocks([{normal, P}, {h2_or_hr, _} | T], Refs, Acc) ->
    io:format("MATCHED: normal + h2_or_hr~n"),
    ...
```

If you don't see this output, the clause isn't matching.

### Issue: Greedy consumption not working

**Symptom**: h2_or_hr + normal becomes hr + paragraph instead of merged paragraph.

**Cause**: Standalone clause matching before greedy clause.

**Fix**: Move greedy clause BEFORE standalone clause.

### Issue: Extra whitespace in headers

**Symptom**: Headers have leading/trailing spaces.

**Check**: The renderer's strip logic.

**Fix**: Strip both left and right in header rendering.

### Issue: Blockquote not becoming h2

**Symptom**: Blockquote + h2_or_hr stays as blockquote instead of h2.

**Cause**: Look-behind clause for blockquote not matching, or is after standalone.

**Check**: Is the blockquote clause present and before standalone?

---

## Reference Code

### Original h2_or_hr handling (erlmd.erl lines 153-172, 226-228)

```erlang
%% setext h2 look-behind
p1([{normal, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    P2 = string:strip(make_str(snip(P), R), both, ?SPACE),
    p1(T, R, I, [pad(I) ++ "<h2>" ++ P2 ++ "</h2>\n\n" | Acc]);

p1([{blockquote, P}, {h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ make_str(snip(P), R)
                        ++ "</h2>\n\n" | Acc]);

p1([{{codeblock, P}, _}, {h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  [pad(I) ++ "<h2>" ++ make_str(snip(P), R)
                        ++ "</h2>\n\n" | Acc]);

% ... later ...

%% h2_or_hr is greedy for normal lines
p1([{h2_or_hr, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{normal, flatten([P1 | P2])} | T], R, I, Acc);

%% standalone hr
p1([{h2_or_hr, _} | T], R, I, Acc) ->
    p1(T, R, I,  ["<hr />" | Acc]);
```

---

## Key Insights

1. **Clause order is critical**: Look-behind, greedy, standalone - in that order
2. **Three behaviors**: h2 (look-behind), paragraph merge (greedy), hr (standalone)
3. **Blank lines break look-behind**: Blank between normal and h2_or_hr prevents h2
4. **Simple logic**: Most of the complexity is in clause ordering, not implementation

---

## Complexity Assessment

**Phase 7 is SIMPLE**:
- Logic already implemented correctly
- Just need to verify clause order
- Possibly a small whitespace fix
- Mostly verification work
- No state management
- No complex interactions

---

## Deliverables

After completing Phase 7, provide:

1. **Test results**: Before and after numbers
2. **Clause order verification**: Confirm order is correct
3. **Test file results**: Output from all test files
4. **Changes**: List any fixes applied (may be none)

This information will guide Phase 8 (Tag Handling).

---

## Notes

**Likely outcome**: Phase 7 is probably already correct, just needs verification.

**Relationship to other phases**: h2_or_hr doesn't interact with other phases much - it's self-contained.

**Testing**: Clear pass/fail - either it creates the right element or it doesn't.

**If already working**: This phase might be "verification only" with no code changes needed!
