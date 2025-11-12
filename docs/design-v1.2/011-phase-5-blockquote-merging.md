# Phase 5: Blockquote Merging - Implementation Instructions for Claude Code

## Overview

**Objective**: Verify and fix blockquote merging logic in `erlmd_ast.erl` to ensure consecutive blockquotes merge correctly with `<br />` separators, and that blockquotes properly consume following normal lines.

**Estimated Time**: 1-2 hours  
**Expected Impact**: Fixes 3 failing tests  
**Priority**: MEDIUM  
**Prerequisites**: Phases 1-4 should be complete

---

## Context

Blockquotes in markdown have special merging behavior:

### Blockquote Consumes Normal Line

**Input**:
```markdown
> Quote line 1
Normal continuation
```

**Expected output** (merged into single blockquote):
```html
<blockquote>
  <p>Quote line 1 Normal continuation</p>
</blockquote>
```

### Consecutive Blockquotes Merge with `<br />`

**Input**:
```markdown
> Quote line 1
> Quote line 2
```

**Expected output** (merged with line break):
```html
<blockquote>
  <p>Quote line 1<br /> Quote line 2</p>
</blockquote>
```

### Key Point: Whitespace After `>`

When merging blockquotes, the second `>` is replaced, and any whitespace after it should be stripped.

**Input tokens**:
```erlang
[{blockquote, [{{md, gt}, ">"}, {{ws, sp}, " "}, {string, "text"}]}]
```

After stripping `>`, we get:
```erlang
[{{ws, sp}, " "}, {string, "text"}]
```

The leading whitespace should be removed too:
```erlang
[{string, "text"}]
```

---

## Current State Analysis

### What's Already Implemented

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_blocks/3`  
**Lines**: 145-164

```erlang
%% Consecutive normal lines merge into one paragraph
parse_blocks([{normal, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{normal, Merged} | T], Refs, Acc);

%% Blockquotes that consume normals
parse_blocks([{blockquote, P1}, {blockquote, [_ | P2]} | T], Refs, Acc) ->
    % After stripping '>', also strip leading whitespace
    P2Trimmed = case P2 of
        [{{ws, _}, _} | Rest] -> Rest;
        _ -> P2
    end,
    Merged = merge_with_br(P1, P2Trimmed),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);

parse_blocks([{blockquote, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);

%% Single blockquote
parse_blocks([{blockquote, P} | T], Refs, Acc) ->
    [{{md, gt}, _} | T1] = P,
    % Trim leading whitespace token after stripping '>'
    T2 = case T1 of
        [{{ws, _}, _} | Rest] -> Rest;
        _ -> T1
    end,
    Content = build_inline(T2, Refs),
    % Blockquotes contain paragraphs
    BQ = #blockquote{blocks = [#paragraph{content = Content}]},
    parse_blocks(T, Refs, [BQ | Acc]);
```

### Helper Function

**Function**: `merge_with_br/2`  
**Lines**: 573

```erlang
merge_with_br(P1, P2) ->
    NewP1 = make_br(P1),
    lists:flatten([NewP1, {tags, "<br /> "} | P2]).
```

### The Original Behavior

**Reference**: `src/erlmd.erl` lines 178-183

```erlang
%% blockquotes swallow each other
%% replace the first blockquote mark with a space...
p1([{blockquote, P1}, {blockquote, [_ | P2]} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1++[{tags, "<br />"}], [], P2)} | T], R, I, Acc);
%% blockquotes swallow normal
p1([{blockquote, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1, [], P2)} | T], R, I, Acc);
```

**Key observations**:

1. **Blockquote + blockquote**: Uses `merge(P1++[{tags, "<br />"}], [], P2)`
   - Appends `<br />` tag token to P1
   - Then merges with P2
   - Uses empty string `[]` as padding

2. **Blockquote + normal**: Uses `merge(P1, [], P2)`
   - Just merges with empty padding

3. **Stripping `>`**: Pattern `[_ | P2]` removes the first element (the `>`)

---

## Task 1: Run Blockquote Tests

**Action**: Identify which blockquote tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct --suite test/erlmd_SUITE | grep -i "blockquote\|quote"
```

**Also search for blockquote tests**:
```bash
grep -rn "blockquote" test/erlmd_SUITE.erl | grep "test_"
```

**Capture**:
1. How many blockquote tests exist?
2. Which are failing?
3. What's the expected vs actual output?

---

## Task 2: Create Minimal Test Cases

**Action**: Create test files to isolate blockquote scenarios.

### Test Case A: Single Blockquote

**File**: `test_blockquote_single.md`
```markdown
> This is a quote
```

**Expected output**:
```html
<blockquote>
  <p>This is a quote</p>
</blockquote>
```

### Test Case B: Consecutive Blockquotes

**File**: `test_blockquote_consecutive.md`
```markdown
> Quote line 1
> Quote line 2
```

**Expected output** (merged with `<br />`):
```html
<blockquote>
  <p>Quote line 1<br /> Quote line 2</p>
</blockquote>
```

### Test Case C: Blockquote Consumes Normal

**File**: `test_blockquote_consumes_normal.md`
```markdown
> Quote line 1
Normal continuation
```

**Expected output**:
```html
<blockquote>
  <p>Quote line 1 Normal continuation</p>
</blockquote>
```

### Test Case D: Multiple Consecutive Blockquotes

**File**: `test_blockquote_multiple.md`
```markdown
> Line 1
> Line 2
> Line 3
```

**Expected output**:
```html
<blockquote>
  <p>Line 1<br /> Line 2<br /> Line 3</p>
</blockquote>
```

### Test Case E: Blockquote Then Normal Line

**File**: `test_blockquote_separate.md`
```markdown
> This is a quote

Normal paragraph.
```

**Expected output** (separate blocks):
```html
<blockquote>
  <p>This is a quote</p>
</blockquote>

<p>Normal paragraph.</p>
```

### Test Case F: Nested Blockquotes

**File**: `test_blockquote_nested.md`
```markdown
> Level 1
> > Level 2
```

**Expected output** (this is complex - may not be tested):
```html
<blockquote>
  <p>Level 1</p>
  <blockquote>
    <p>Level 2</p>
  </blockquote>
</blockquote>
```

---

## Task 3: Test Original vs AST

**Action**: Run each test case through both implementations.

```erlang
test_blockquote(File) ->
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
            % Show hex for whitespace differences
            io:format("Original bytes: ~w~n", [Original]),
            io:format("AST bytes: ~w~n", [AST])
    end.

% Test all files
test_blockquote("test_blockquote_single.md").
test_blockquote("test_blockquote_consecutive.md").
test_blockquote("test_blockquote_consumes_normal.md").
test_blockquote("test_blockquote_multiple.md").
test_blockquote("test_blockquote_separate.md").
```

**Capture**: Which test cases fail and how?

---

## Task 4: Analyze merge_with_br/2

**Action**: Verify that `merge_with_br/2` matches the original's behavior.

**Current implementation** (line 573):
```erlang
merge_with_br(P1, P2) ->
    NewP1 = make_br(P1),
    lists:flatten([NewP1, {tags, "<br /> "} | P2]).
```

**Original implementation**:
```erlang
merge(P1++[{tags, "<br />"}], [], P2)
```

Which expands to:
```erlang
NewP1 = make_br(P1++[{tags, "<br />"}]),
flatten([NewP1, {string, []} | P2])
```

**Differences**:

1. **Original**: Appends `{tags, "<br />"}` to P1 BEFORE calling `make_br`
2. **Current**: Calls `make_br` on P1, THEN appends `{tags, "<br /> "}`

3. **Original**: Uses `{string, []}` (empty string) between
4. **Current**: Puts `<br />` directly between

**Potential issue**: The original's `make_br` might interact with the appended `<br />` tag.

Let's check what `make_br` does:

```erlang
make_br(List) -> make_br1(reverse(List)).

make_br1([{{lf, _}, _}, {{ws, comp}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1([{{lf, _}, _}, {{ws, tab}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1(List) -> 
    reverse(List).
```

If the last element of `P1++[{tags, "<br />"}]` is `{tags, "<br />"}`, it won't match the pattern, so it just returns the list as-is.

**So the original flow**:
```erlang
P1 = [tokens...],
P1_with_br = P1 ++ [{tags, "<br />"}],
NewP1 = make_br(P1_with_br),  % Just returns P1_with_br unchanged
Result = flatten([NewP1, {string, []}, P2])
       = flatten([P1, {tags, "<br />"}, {string, []}, P2])
```

**Our current flow**:
```erlang
P1 = [tokens...],
NewP1 = make_br(P1),  % Might convert trailing ws+lf to <br />
Result = flatten([NewP1, {tags, "<br /> "}, P2])
```

**Key difference**: We add space after `<br />`, original doesn't (uses empty string).

**Test this**:
```erlang
% Original style
P1 = [{string, "text"}].
P2 = [{string, "more"}].
Original = lists:flatten([P1 ++ [{tags, "<br />"}], {string, []}, P2]).
io:format("Original: ~p~n", [Original]).
% Should be: [{string, "text"}, {tags, "<br />"}, {string, []}, {string, "more"}]

% Our style
Ours = lists:flatten([P1, {tags, "<br /> "}, P2]).
io:format("Ours: ~p~n", [Ours]).
% Should be: [{string, "text"}, {tags, "<br /> "}, {string, "more"}]
```

When these are rendered to HTML:
- Original: `text<br />more`
- Ours: `text<br /> more`

**The space might matter!** Let's check the original more carefully.

Actually, looking at line 178 again:
```erlang
p1([{blockquote, P1}, {blockquote, [_ | P2]} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1++[{tags, "<br />"}], [], P2)} | T], R, I, Acc);
```

The `merge` call is:
```erlang
merge(P1++[{tags, "<br />"}], [], P2)
```

Where the second argument `[]` is the padding (empty list/string).

Looking at `merge/3`:
```erlang
merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    flatten([NewP1, {string, Pad} | P2]).
```

So:
```erlang
merge(P1++[{tags, "<br />"}], [], P2)
  = NewP1 = make_br(P1++[{tags, "<br />"}])
  = flatten([NewP1, {string, []} | P2])
```

If padding is `[]` (empty list), then `{string, []}` is an empty string token.

**Our implementation** uses `{tags, "<br /> "}` with a space.

**Fix needed**: Match the original exactly:

```erlang
merge_with_br(P1, P2) ->
    P1_with_br = P1 ++ [{tags, "<br />"}],
    NewP1 = make_br(P1_with_br),
    lists:flatten([NewP1, {string, []} | P2]).
```

Actually wait, let me check what the space in `"<br /> "` does. Looking at HTML, both `<br />` and `<br /> ` (with trailing space) should render the same in browsers. But for exact matching with tests, we need to match the original.

---

## Task 5: Check Whitespace Trimming

**Action**: Verify that whitespace after `>` is correctly trimmed.

**Current implementation** (lines 146-152):
```erlang
parse_blocks([{blockquote, P1}, {blockquote, [_ | P2]} | T], Refs, Acc) ->
    % After stripping '>', also strip leading whitespace
    P2Trimmed = case P2 of
        [{{ws, _}, _} | Rest] -> Rest;
        _ -> P2
    end,
    Merged = merge_with_br(P1, P2Trimmed),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);
```

**Question**: Does the original do this?

Looking at the original (line 178):
```erlang
p1([{blockquote, P1}, {blockquote, [_ | P2]} | T], R, I, Acc) ->
```

The pattern `[_ | P2]` strips the first element (the `>` token), but doesn't explicitly strip whitespace.

**However**, the lexer might handle this. Let's check what the typed line looks like for:
```markdown
> Quote line 1
> Quote line 2
```

After lexing and typing, we'd get something like:
```erlang
[
  {blockquote, [{{md, gt}, ">"}, {{ws, sp}, " "}, {string, "Quote line 1"}, {{lf, lf}, "\n"}]},
  {blockquote, [{{md, gt}, ">"}, {{ws, sp}, " "}, {string, "Quote line 2"}, {{lf, lf}, "\n"}]}
]
```

When we pattern match `[_ | P2]`, we get:
```erlang
P2 = [{{ws, sp}, " "}, {string, "Quote line 2"}, {{lf, lf}, "\n"}]
```

So the whitespace IS there, and we need to remove it.

**Our trimming looks correct!** The question is whether it's sufficient.

**Edge case**: What if there's no space after `>`?
```markdown
>Quote line 1
>Quote line 2
```

Then P2 would be:
```erlang
P2 = [{string, "Quote line 2"}, {{lf, lf}, "\n"}]
```

And our trimming code:
```erlang
P2Trimmed = case P2 of
    [{{ws, _}, _} | Rest] -> Rest;
    _ -> P2
end,
```

Would return `P2` unchanged. Good!

---

## Task 6: Check Single Blockquote Processing

**Action**: Verify that single blockquotes are processed correctly.

**Current implementation** (lines 158-164):
```erlang
%% Single blockquote
parse_blocks([{blockquote, P} | T], Refs, Acc) ->
    [{{md, gt}, _} | T1] = P,
    % Trim leading whitespace token after stripping '>'
    T2 = case T1 of
        [{{ws, _}, _} | Rest] -> Rest;
        _ -> T1
    end,
    Content = build_inline(T2, Refs),
    % Blockquotes contain paragraphs
    BQ = #blockquote{blocks = [#paragraph{content = Content}]},
    parse_blocks(T, Refs, [BQ | Acc]);
```

**Original implementation** (lines 185-189):
```erlang
%% blockquote
p1([{blockquote, P} | T], R, I, Acc) ->
    [{{md, gt}, _} | T1] = P,
    T2 = string:strip(make_str(T1, R)),
    p1(T, R, I,
       ["\n<blockquote>\n" ++ pad(I + 1) ++ "<p>" ++ T2 ++ "</p>\n</blockquote>" | Acc]);
```

**Differences**:

1. **Original**: Uses `string:strip(make_str(T1, R))` - converts to string first, then strips
2. **Current**: Strips whitespace token, then converts to inline AST

The original's approach means it strips leading/trailing spaces from the FINAL STRING, not just the whitespace token.

**Our approach** only strips the first whitespace token. If there are multiple whitespace tokens, or trailing whitespace, we might not handle it correctly.

**Potential issue**: What if the blockquote is:
```markdown
>   Quote with multiple spaces
```

Tokens: `[{{md, gt}, ">"}, {{ws, sp}, "   "}, {string, "Quote with multiple spaces"}]`

After stripping `>`: `[{{ws, sp}, "   "}, {string, "Quote with multiple spaces"}]`

After our trim: `[{string, "Quote with multiple spaces"}]` ✓ Good!

But what if the quote ends with spaces?
```markdown
> Quote with trailing spaces   
```

Tokens: `[{{md, gt}, ">"}, {{ws, sp}, " "}, {string, "Quote with trailing spaces"}, {{ws, sp}, "   "}, {{lf, lf}, "\n"}]`

After stripping `>`: `[{{ws, sp}, " "}, {string, "Quote with trailing spaces"}, {{ws, sp}, "   "}, {{lf, lf}, "\n"}]`

After our trim: `[{string, "Quote with trailing spaces"}, {{ws, sp}, "   "}, {{lf, lf}, "\n"}]`

When converted to string: `"Quote with trailing spaces   \n"`

**The original would strip this to**: `"Quote with trailing spaces"`

**We would keep**: `"Quote with trailing spaces   \n"` (before snip removes \n)

**Potential fix**: Use `string:strip` on the final string like the original does.

But wait, we're building an AST, not a string. The stripping should happen in the renderer.

Actually, looking at our renderer (`erlmd_html.erl` line 25):
```erlang
render_block(#paragraph{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    ContentStr = lists:flatten(InlineHTML),
    Stripped = string:strip(string:strip(ContentStr, left, $ ), right, $ ),
    "<p>" ++ Stripped ++ "</p>\n";
```

We DO strip leading/trailing spaces when rendering paragraphs! So this should be fine.

---

## Task 7: Apply Fixes

**Action**: Update `merge_with_br/2` to exactly match the original.

**File**: `src/erlmd_ast.erl`  
**Location**: Around line 573

**Change from**:
```erlang
merge_with_br(P1, P2) ->
    NewP1 = make_br(P1),
    lists:flatten([NewP1, {tags, "<br /> "} | P2]).
```

**Change to**:
```erlang
merge_with_br(P1, P2) ->
    % Match original: append <br /> tag to P1, then call make_br
    P1_with_br = P1 ++ [{tags, "<br />"}],
    NewP1 = make_br(P1_with_br),
    % Use empty string as separator (like original's [] padding)
    lists:flatten([NewP1, {string, []} | P2]).
```

**Alternatively** (simpler, since make_br won't change anything):
```erlang
merge_with_br(P1, P2) ->
    % Append <br /> tag to P1 (make_br won't modify it)
    lists:flatten([P1, {tags, "<br />"} | P2]).
```

Wait, let me think about this more carefully.

If P1 ends with `ws + lf`, `make_br(P1)` would convert that to a hard break. But we're adding ANOTHER `<br />` tag. That would give us two `<br />` tags!

Let me check the original's logic again:

```erlang
merge(P1++[{tags, "<br />"}], [], P2)
```

This appends `<br />` to P1, then calls `make_br` on the combined list.

If P1 ends with `ws + lf`, the combined list would be:
```erlang
[..., {{ws, sp}, "  "}, {{lf, lf}, "\n"}, {tags, "<br />"}]
```

Reversed:
```erlang
[{tags, "<br />"}, {{lf, lf}, "\n"}, {{ws, sp}, "  "}, ...]
```

`make_br1` checks if the list (reversed) starts with `lf + ws`:
```erlang
make_br1([{{lf, _}, _}, {{ws, comp}, _} | T]) -> ...
```

In our case, it starts with `{tags, "<br />"}`, so it doesn't match. Returns the list unchanged.

**So**: Appending `{tags, "<br />"}` BEFORE `make_br` prevents `make_br` from converting the trailing `ws + lf` to a hard break, because the pattern no longer matches.

**This is intentional!** For blockquote merging, we want the `<br />` we're adding, not a hard break from trailing whitespace.

**So our fix should be**:
```erlang
merge_with_br(P1, P2) ->
    % Append <br /> BEFORE make_br to prevent it from converting trailing ws+lf
    P1_with_br = P1 ++ [{tags, "<br />"}],
    NewP1 = make_br(P1_with_br),
    lists:flatten([NewP1 | P2]).
```

No separator needed - we already have the `<br />` in the list.

---

## Task 8: Test the Fix

**Action**: Run all blockquote test cases.

```erlang
test_blockquote("test_blockquote_single.md").
test_blockquote("test_blockquote_consecutive.md").
test_blockquote("test_blockquote_consumes_normal.md").
test_blockquote("test_blockquote_multiple.md").
test_blockquote("test_blockquote_separate.md").
```

**Expected**: All should match between Original and AST.

**Run full test suite**:
```bash
rebar3 ct --suite test/erlmd_SUITE | grep -i blockquote
```

**Check**:
1. How many blockquote tests pass now?
2. Any regressions?

---

## Task 9: Check Blockquote in HTML Renderer

**Action**: Verify that blockquotes render correctly.

**In `erlmd_html.erl`, function `render_block/1`** (line 26):

```erlang
render_block(#blockquote{blocks = Blocks}) ->
    BlockHTML = lists:map(fun render_block/1, Blocks),
    "\n<blockquote>\n  " ++ lists:flatten(BlockHTML) ++ "</blockquote>";
```

**Compare with original output**:
```erlang
"\n<blockquote>\n" ++ pad(I + 1) ++ "<p>" ++ T2 ++ "</p>\n</blockquote>"
```

**Differences**:

1. **Original**: Uses `pad(I + 1)` (indentation: 2 spaces for I=0)
2. **Current**: Uses `"  "` (always 2 spaces)

For root-level blockquotes (I=0), these are the same. For nested blockquotes, the original would add more indentation.

**This should be fine** unless we have nested blockquotes, which are rare.

---

## Task 10: Edge Case - Empty Blockquote

**Action**: Test what happens with an empty blockquote.

**Test file**: `test_blockquote_empty.md`
```markdown
>
```

**Run**:
```erlang
test_blockquote("test_blockquote_empty.md").
```

**Observe**: What does the original output? What does ours output?

This might not be a valid test case, but worth checking.

---

## Task 11: Edge Case - Blockquote with Code

**Action**: Test blockquote containing code.

**Test file**: `test_blockquote_code.md`
```markdown
> Quote with `code` inside
```

**Expected output**:
```html
<blockquote>
  <p>Quote with <code>code</code> inside</p>
</blockquote>
```

**Run**:
```erlang
test_blockquote("test_blockquote_code.md").
```

---

## Task 12: Edge Case - Blockquote with Emphasis

**Action**: Test blockquote containing emphasis.

**Test file**: `test_blockquote_emphasis.md`
```markdown
> Quote with *emphasis*
```

**Expected output**:
```html
<blockquote>
  <p>Quote with <em>emphasis</em></p>
</blockquote>
```

**Run**:
```erlang
test_blockquote("test_blockquote_emphasis.md").
```

---

## Task 13: Document Findings

**Action**: Create a summary report.

```markdown
## Phase 5 Results

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Changes made**:
1. Updated `merge_with_br/2` to append `<br />` tag BEFORE calling `make_br`
2. Removed extra space after `<br />` tag
3. Removed separator between merged blockquotes

**Root cause**:
The `merge_with_br/2` function was adding `<br />` in the wrong order and including an extra space. The original appends the `<br />` tag to P1 before calling `make_br`, which prevents `make_br` from converting trailing whitespace to a hard break.

**Why the fix works**:
By appending `{tags, "<br />"}` to P1 before `make_br`, we:
1. Prevent double `<br />` tags (from both make_br and our explicit tag)
2. Match the original's token structure exactly
3. Use no separator (empty string in original, nothing in ours)

**Test results**:
- ✓/✗ Single blockquote
- ✓/✗ Consecutive blockquotes
- ✓/✗ Blockquote consumes normal
- ✓/✗ Multiple consecutive
- ✓/✗ Blockquote separate from normal
- ✓/✗ Blockquote with code
- ✓/✗ Blockquote with emphasis

**Remaining issues**:
[List any blockquote tests still failing]

**Next steps**:
- [If all pass]: Move to Phase 6 (Tab Expansion)
- [If some fail]: Investigate [specific issue]
```

---

## Success Criteria

Phase 5 is complete when:

- ✅ Single blockquotes render correctly
- ✅ Consecutive blockquotes merge with `<br />`
- ✅ Blockquotes consume following normal lines
- ✅ Multiple consecutive blockquotes merge correctly
- ✅ Blockquotes work with inline formatting (code, emphasis)
- ✅ All blockquote tests pass
- ✅ No regressions in other tests
- ✅ At least 3 additional tests passing overall

---

## Troubleshooting Guide

### Issue: Extra spaces in blockquotes

**Symptom**: Output has extra spaces like `<br /> ` instead of `<br />`.

**Cause**: Our `merge_with_br` was adding a space.

**Solution**: Already fixed in Task 7 - remove the space.

### Issue: Double `<br />` tags

**Symptom**: Output has `<br /><br />` instead of just `<br />`.

**Cause**: Both `make_br` and our explicit tag are creating `<br />`.

**Solution**: Append tag BEFORE `make_br` so the pattern doesn't match.

### Issue: Blockquotes don't merge

**Symptom**: Each blockquote line creates a separate `<blockquote>` block.

**Cause**: The merging clauses aren't matching.

**Check**: Are the merging clauses in the right order? They should be BEFORE the single blockquote clause.

### Issue: Whitespace not stripped

**Symptom**: Extra spaces at the beginning of blockquote content.

**Cause**: Whitespace trimming not working.

**Check**: Verify the `P2Trimmed` logic in the consecutive blockquote clause.

### Issue: Blockquote doesn't consume normal line

**Symptom**: Normal line after blockquote is separate paragraph.

**Cause**: The consumption clause isn't matching.

**Check**: Is there a blank line between them? Blank lines should prevent consumption.

---

## Reference Code

### Original blockquote merging (erlmd.erl lines 178-189)

```erlang
%% blockquotes swallow each other
%% replace the first blockquote mark with a space...
p1([{blockquote, P1}, {blockquote, [_ | P2]} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1++[{tags, "<br />"}], [], P2)} | T], R, I, Acc);
%% blockquotes swallow normal
p1([{blockquote, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1, [], P2)} | T], R, I, Acc);
%% blockquote
p1([{blockquote, P} | T], R, I, Acc) ->
    [{{md, gt}, _} | T1] = P,
    T2 = string:strip(make_str(T1, R)),
    p1(T, R, I,
       ["\n<blockquote>\n" ++ pad(I + 1) ++ "<p>" ++ T2 ++ "</p>\n</blockquote>" | Acc]);
```

### Original merge/3 (erlmd.erl line 263)

```erlang
merge(P1, Pad, P2) ->
    NewP1 = make_br(P1),
    flatten([NewP1, {string, Pad} | P2]).
```

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

---

## Key Insights

1. **Order matters**: Appending `<br />` BEFORE `make_br` prevents double breaks
2. **No separator needed**: The `<br />` tag is in the merged list, no extra separator
3. **Whitespace trimming**: First token after `>` is often whitespace, must be removed
4. **Simple phase**: Most logic already in place, just needed fine-tuning

---

## Complexity Assessment

**Phase 5 is SIMPLE compared to Phase 4**:
- Logic mostly already implemented
- Just needed to match original's exact token structure
- One main fix (merge_with_br)
- No complex state management
- No retroactive propagation

---

## Deliverables

After completing Phase 5, provide:

1. **Test results**: Before and after numbers for blockquote tests
2. **Code changes**: Diff showing the merge_with_br fix
3. **Test file results**: Output from all 5+ test files
4. **Analysis**: Why the token order mattered
5. **Edge case results**: Which edge cases pass/fail

This information will guide Phase 6 (Tab Expansion).

---

## Notes

**Relationship to Phase 3**: If Phase 3 fixed hard line breaks, blockquotes should already handle trailing `ws + lf` correctly via `make_br`.

**Relationship to Phase 1**: Blockquote merging doesn't interact with list consumption.

**Simplicity**: This phase is mostly verification - the code was already ~90% correct, just needed the `merge_with_br` fix.
