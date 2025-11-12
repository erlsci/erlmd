# Phase 6: Tab Expansion - Implementation Instructions for Claude Code

## Overview

**Objective**: Ensure tabs in text content are expanded to 4 spaces in the rendered HTML output.

**Estimated Time**: 30 minutes  
**Expected Impact**: Fixes 1 failing test  
**Priority**: LOW  
**Prerequisites**: Phases 1-5 should be complete

---

## Context

In markdown, tabs should be expanded to 4 spaces when rendered. This applies to:
- Text content in paragraphs
- Text in emphasis/strong
- Text in blockquotes

**But NOT to**:
- Code blocks (tabs should be literal)
- Code spans (tabs should be literal)

### Example

**Input**:
```markdown
This is text	with a tab.
```
(Note: There's a literal tab character between "text" and "with")

**Expected output**:
```html
<p>This is text    with a tab.</p>
```
(The tab becomes 4 spaces)

---

## Current State Analysis

### What's Already Implemented

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_emphasis_and_code/2`  
**Line**: 444

```erlang
%% Tab - expand to 4 spaces (hard breaks with tab already handled above)
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

**This looks correct!** Tabs are being expanded to 4 spaces during inline parsing.

### The Problem

If tab expansion is already implemented but tests are still failing, the issue is likely:

1. **Tabs in tokens not reaching inline parsing**: If tabs are in `{string, "text\tmore"}` tokens, they won't go through the character-by-character parsing
2. **Tabs in code blocks being expanded**: They should remain literal
3. **Tab ordering**: The tab expansion might be happening before hard break detection (already fixed in Phase 3)

---

## Task 1: Run Tab Tests

**Action**: Identify which tab-related tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct --suite test/erlmd_SUITE | grep -i "tab"
```

**Also search for tab tests**:
```bash
grep -rn "tab\|\\\\t" test/erlmd_SUITE.erl | grep "test_"
```

**Capture**:
1. How many tab tests exist?
2. Which are failing?
3. What's the expected vs actual output?

---

## Task 2: Create Minimal Test Cases

**Action**: Create test files to isolate tab expansion scenarios.

### Test Case A: Tab in Normal Text

**File**: `test_tab_text.md`
```markdown
Text with	tab inside.
```
(Note: Literal tab character between "with" and "tab")

**Expected output**:
```html
<p>Text with    tab inside.</p>
```

### Test Case B: Tab at Start of Line

**File**: `test_tab_start.md`
```markdown
	Indented text
```
(Note: Tab at the very beginning)

**Expected output** (might be a code block if 4-space indent rule applies):
```html
<p>    Indented text</p>
```
OR
```html
<pre><code>Indented text
</code></pre>
```

### Test Case C: Multiple Tabs

**File**: `test_tab_multiple.md`
```markdown
Text	with	multiple	tabs.
```

**Expected output**:
```html
<p>Text    with    multiple    tabs.</p>
```

### Test Case D: Tab in Emphasis

**File**: `test_tab_emphasis.md`
```markdown
*Text	with tab*
```

**Expected output**:
```html
<p><em>Text    with tab</em></p>
```

### Test Case E: Tab in Code Block (Should NOT Expand)

**File**: `test_tab_codeblock.md`
```markdown
    code	with	tab
```

**Expected output** (tab should be literal):
```html
<pre><code>code	with	tab
</code></pre>
```

### Test Case F: Tab in Code Span (Should NOT Expand)

**File**: `test_tab_codespan.md`
```markdown
Inline `code	with	tab` here.
```

**Expected output** (tab should be literal):
```html
<p>Inline <code>code	with	tab</code> here.</p>
```

---

## Task 3: Test Original vs AST

**Action**: Run each test case through both implementations.

```erlang
test_tab(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    Original = erlmd:conv_original(MD),
    AST = erlmd:conv_ast(MD),
    
    io:format("~n=== Testing ~s ===~n", [File]),
    io:format("Original:~n~s~n", [Original]),
    io:format("AST:~n~s~n", [AST]),
    
    % Show hex to see tabs vs spaces
    io:format("Original bytes: ~w~n", [Original]),
    io:format("AST bytes: ~w~n", [AST]),
    
    case Original =:= AST of
        true -> io:format("✓ MATCH~n");
        false -> 
            io:format("✗ MISMATCH~n"),
            % Check if it's a tab vs space issue
            OrigTabCount = length([C || C <- Original, C =:= 9]),  % 9 = tab
            ASTTabCount = length([C || C <- AST, C =:= 9]),
            io:format("Original has ~p tabs, AST has ~p tabs~n", [OrigTabCount, ASTTabCount])
    end.

% Test all files
test_tab("test_tab_text.md").
test_tab("test_tab_start.md").
test_tab("test_tab_multiple.md").
test_tab("test_tab_emphasis.md").
test_tab("test_tab_codeblock.md").
test_tab("test_tab_codespan.md").
```

**Capture**: Which test cases fail and how?

---

## Task 4: Check How Tabs Flow Through System

**Action**: Trace where tabs exist in the token stream.

**Add debug to lexer output**:
```erlang
test_tab_flow(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    io:format("~n=== Tab Flow for ~s ===~n", [File]),
    io:format("Input bytes: ~w~n", [MD]),
    
    % Check lexer output
    Lex = erlmd:lex(MD),
    io:format("~nAfter lex:~n"),
    TabTokens = [T || T <- Lex, element(1, element(1, T)) =:= ws, element(2, element(1, T)) =:= tab],
    io:format("  Found ~p tab tokens: ~p~n", [length(TabTokens), TabTokens]),
    
    % Check typed lines
    Lines = erlmd:make_lines(Lex),
    {TypedLines, Refs} = erlmd:type_lines(Lines),
    io:format("~nAfter type_lines:~n"),
    io:format("  TypedLines: ~p~n", [TypedLines]),
    
    % Check if tabs survive
    HasTabs = lists:any(fun(Line) ->
        case Line of
            {normal, Tokens} -> 
                lists:any(fun(T) ->
                    case T of
                        {{ws, tab}, _} -> true;
                        _ -> false
                    end
                end, Tokens);
            _ -> false
        end
    end, TypedLines),
    io:format("  Typed lines have tab tokens: ~p~n", [HasTabs]).

test_tab_flow("test_tab_text.md").
```

**This will show**:
1. Are tabs in the input?
2. Does the lexer create tab tokens?
3. Do tabs survive through line typing?

---

## Task 5: Check Inline Parsing

**Action**: Verify tabs reach `parse_emphasis_and_code/2`.

**Add debug to `collect_text_tokens/0`** (around line 372):

```erlang
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    io:format("DEBUG collect_text_tokens: string ~p~n", [S]),
    case lists:member(9, S) of  % 9 = tab
        true -> io:format("  ^^^ Contains TAB!~n");
        false -> ok
    end,
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
```

**Add debug to `parse_emphasis_and_code/2`** (around line 444):

```erlang
parse_emphasis_and_code([?TAB | T], Acc) ->
    io:format("DEBUG: Found tab, expanding to 4 spaces~n"),
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

**Run test**:
```erlang
{ok, C} = file:read_file("test_tab_text.md").
erlmd:conv_ast(binary_to_list(C)).
```

**Observe**:
- Do we see "Contains TAB!"?
- Do we see "Found tab, expanding"?

---

## Task 6: Identify the Problem

Based on debug output, you should see one of these patterns:

### Pattern A: Tabs never reach inline parsing

**Symptom**: No "Found tab, expanding" debug output.

**Cause**: Tabs are embedded in `{string, "text\tmore"}` tokens and collected as strings without character-by-character processing.

**Solution**: Need to expand tabs BEFORE or DURING string collection.

### Pattern B: Tabs are expanded but in wrong place

**Symptom**: Tabs become spaces, but in code blocks/spans too.

**Cause**: Tab expansion happening globally, not just in text.

**Solution**: Only expand in text context, not in code.

### Pattern C: Tab expansion works but order is wrong

**Symptom**: Hard breaks with tabs don't work.

**Cause**: Already fixed in Phase 3 - tab expansion should come AFTER hard break detection.

---

## Task 7: Check Original's Tab Handling

**Action**: Study how the original handles tabs.

**Reference**: `src/erlmd.erl`

**Lexer** (line 1016):
```erlang
l1([?TAB | T], A1, A2) -> l1(T, [], [{{ws, tab}, "\t"}, l2(A1) | A2]);
```

The lexer creates `{{ws, tab}, "\t"}` tokens - preserving the literal tab.

**In htmlchars** (line 706):
```erlang
htmlchars1([?TAB | T], Acc)          -> htmlchars1(T, ["    " | Acc]);
```

This is where tabs get expanded to 4 spaces in the original!

**In htmlencode** (for code blocks, line 840):
```erlang
htmlencode([Else | Rest], Acc) -> htmlencode(Rest, [Else | Acc]).
```

Tabs are NOT expanded in code blocks - they pass through unchanged.

**So the original**:
1. Preserves tabs in tokens
2. Expands tabs in `htmlchars1` (for normal text)
3. Doesn't expand tabs in `htmlencode` (for code blocks/spans)

**Our approach**:
1. Preserves tabs in tokens ✓
2. Expands tabs in `parse_emphasis_and_code/2` ✓
3. Doesn't expand in code blocks/spans ✓

**So our approach matches!** The issue must be that tabs in string tokens bypass character-level parsing.

---

## Task 8: Fix Tab Expansion in Strings

**Action**: Expand tabs when collecting string tokens.

**File**: `src/erlmd_ast.erl`  
**Function**: `collect_text_tokens/0`  
**Location**: Around line 372

**Current code**:
```erlang
collect_text_tokens([{string, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
```

**Change to** (expand tabs in strings):
```erlang
collect_text_tokens([{string, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    % Expand tabs in string content
    Expanded = expand_tabs(S),
    {Rest, Expanded ++ More};
```

**Add helper function** (add near other helpers, around line 590):
```erlang
%% @doc Expand tabs to 4 spaces in a string
expand_tabs(String) ->
    expand_tabs(String, []).

expand_tabs([], Acc) ->
    lists:reverse(Acc);
expand_tabs([?TAB | T], Acc) ->
    expand_tabs(T, lists:reverse("    ") ++ Acc);
expand_tabs([H | T], Acc) ->
    expand_tabs(T, [H | Acc]).
```

**But wait**: We also handle tabs character-by-character in `parse_emphasis_and_code/2`. Will this double-expand?

**Check the flow**:
1. `collect_text_tokens` gathers `{string, ...}` tokens and concatenates them
2. The result is a plain string
3. That string goes to `parse_emphasis_and_code/2` for character-level parsing

So if we expand tabs in `collect_text_tokens`, they become spaces, and won't match `[?TAB | T]` in `parse_emphasis_and_code`.

**BUT**: There are also `{{ws, tab}, "\t"}` tokens (not `{string, ...}`). Do those get collected?

Looking at `collect_text_tokens`, the clause:
```erlang
collect_text_tokens([{_, S} | T]) when is_list(S) ->
```

This matches ANY two-element tuple where the second element is a list. So `{{ws, tab}, "\t"}` would match and the `"\t"` would be collected.

**So we need to**:
1. Expand tabs in `{string, ...}` tokens during collection
2. Keep tab expansion in `parse_emphasis_and_code` for `{{ws, tab}, "\t"}` tokens

Actually, let's think about this differently. The `{{ws, tab}, "\t"}` tokens should STOP collection (they're special tokens), not be collected as text.

**Check**: Do we have a clause to stop at whitespace tokens?

Looking at `collect_text_tokens` (lines 387-420), I see:
```erlang
% CRITICAL FIX: Handle special whitespace token {{ws, none}, none}
collect_text_tokens([{{ws, none}, none} | T]) ->
    collect_text_tokens(T);
```

But no clause for `{{ws, tab}, ...}` or `{{ws, sp}, ...}`.

**The last clause**:
```erlang
% Handle other token types as text - but check if content is a list
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
```

This DOES collect `{{ws, tab}, "\t"}` as text!

**So the fix is**:
1. Expand tabs in this clause
2. OR stop collecting at tab tokens

**Option A** (expand during collection):
```erlang
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    Expanded = expand_tabs(S),
    {Rest, Expanded ++ More};
```

**Option B** (stop at tab tokens):
```erlang
% Stop at tab tokens
collect_text_tokens([{{ws, tab}, _} | _] = List) ->
    {List, ""};

% Continue for other tokens
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
```

**Which is better?**

If we stop at tab tokens, they'll go through `parse_emphasis_and_code/2` which already expands them. This is cleaner separation of concerns.

But tabs might be embedded in `{string, "text\tmore"}` tokens too, which need expansion.

**Best approach**: Expand all tabs during collection (Option A), and remove the tab expansion clause from `parse_emphasis_and_code/2` since tabs will already be spaces by that point.

Actually, no - keep both. If tabs come through as tokens, `parse_emphasis_and_code` handles them. If tabs are in strings, collection handles them.

---

## Task 9: Apply the Fix

**Action**: Add tab expansion to `collect_text_tokens`.

**File**: `src/erlmd_ast.erl`  
**Location**: Around line 410

**Find this clause**:
```erlang
% Handle other token types as text - but check if content is a list
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
```

**Change to**:
```erlang
% Handle other token types as text - but check if content is a list
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    % Expand tabs in the string content
    Expanded = lists:flatten([case C of ?TAB -> "    "; _ -> C end || C <- S]),
    {Rest, Expanded ++ More};
```

**This is a one-line change** - just adding the tab expansion inline using a list comprehension.

---

## Task 10: Verify Code Blocks Don't Expand Tabs

**Action**: Ensure tabs in code blocks remain literal.

**Code blocks are processed by** `make_plain_str/1` (line 575), which just concatenates token strings without processing:

```erlang
make_plain_str([{_, Str} | T], Acc) when is_list(Str) ->
    make_plain_str(T, [Str | Acc]);
```

This doesn't expand tabs - it just takes the string as-is. Good!

**Code blocks are then rendered by** `erlmd_html.erl`:

```erlang
render_block(#code_block{content = Content}) ->
    Encoded = htmlencode(Content),
    "<pre><code>" ++ Encoded ++ "\n</code></pre>\n\n";
```

The `htmlencode/1` function (line 82) doesn't expand tabs:

```erlang
htmlencode([?NBSP | Rest], Acc) ->
    htmlencode(Rest, ["&nbsp;" | Acc]);
htmlencode([Else | Rest], Acc) ->
    htmlencode(Rest, [Else | Acc]).
```

Tabs pass through as character 9, which gets included in the HTML. Perfect!

**Code spans** work similarly - content is preserved, not expanded.

---

## Task 11: Test the Fix

**Action**: Run all tab test cases.

```erlang
test_tab("test_tab_text.md").
test_tab("test_tab_start.md").
test_tab("test_tab_multiple.md").
test_tab("test_tab_emphasis.md").
test_tab("test_tab_codeblock.md").
test_tab("test_tab_codespan.md").
```

**Expected**:
- Text tabs: ✓ Expanded to 4 spaces
- Code block tabs: ✓ Remain literal
- Code span tabs: ✓ Remain literal

**Run full test suite**:
```bash
rebar3 ct
```

**Check**:
1. How many tests pass now?
2. Did tab test(s) pass?
3. Any regressions?

---

## Task 12: Edge Case - Tab in Link Text

**Action**: Test tab in link text.

**File**: `test_tab_link.md`
```markdown
[Link	text](http://example.com)
```

**Expected output**:
```html
<p><a href="http://example.com">Link    text</a></p>
```

**Run**:
```erlang
test_tab("test_tab_link.md").
```

---

## Task 13: Document Findings

**Action**: Create a summary report.

```markdown
## Phase 6 Results

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Changes made**:
1. Added tab expansion to `collect_text_tokens` for string content
2. Used inline list comprehension to expand `\t` to 4 spaces

**Root cause**:
Tabs embedded in `{string, ...}` tokens were being collected without character-level processing, so the tab expansion in `parse_emphasis_and_code/2` never saw them.

**Why the fix works**:
By expanding tabs during string collection, we ensure ALL text content has tabs expanded, whether they come from:
- `{string, "text\tmore"}` tokens (expanded in collect_text_tokens)
- `{{ws, tab}, "\t"}` tokens (expanded in parse_emphasis_and_code)

Code blocks and code spans use different processing paths that don't expand tabs, so they remain literal.

**Test results**:
- ✓/✗ Tab in normal text
- ✓/✗ Tab at start of line
- ✓/✗ Multiple tabs
- ✓/✗ Tab in emphasis
- ✓/✗ Tab in code block (literal)
- ✓/✗ Tab in code span (literal)
- ✓/✗ Tab in link text

**Remaining issues**:
[List any tab tests still failing]

**Next steps**:
- [If all pass]: Move to Phase 7 (h2_or_hr Handling)
- [If some fail]: Investigate [specific issue]
```

---

## Success Criteria

Phase 6 is complete when:

- ✅ Tabs in text expand to 4 spaces
- ✅ Multiple tabs expand correctly
- ✅ Tabs in emphasis/strong expand correctly
- ✅ Tabs in code blocks remain literal
- ✅ Tabs in code spans remain literal
- ✅ All tab tests pass
- ✅ No regressions in other tests
- ✅ At least 1 additional test passing overall

---

## Troubleshooting Guide

### Issue: Tabs not expanding

**Symptom**: Tabs remain as tab characters (ASCII 9) in output.

**Check**: Is the tab expansion line actually executing?

**Debug**:
```erlang
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    io:format("DEBUG: Processing string: ~w~n", [S]),
    TabCount = length([C || C <- S, C =:= 9]),
    io:format("  Contains ~p tabs~n", [TabCount]),
    {Rest, More} = collect_text_tokens(T),
    Expanded = lists:flatten([case C of ?TAB -> "    "; _ -> C end || C <- S]),
    io:format("  After expansion: ~w~n", [Expanded]),
    {Rest, Expanded ++ More};
```

### Issue: Tabs expanding in code blocks

**Symptom**: Code blocks show 4 spaces instead of tabs.

**Cause**: Tab expansion happening in code block processing.

**Check**: Verify that code blocks use `make_plain_str/1` which doesn't expand tabs.

### Issue: Double expansion

**Symptom**: Tabs become 16 spaces (4x4).

**Cause**: Tabs being expanded twice - once in collection, once in parsing.

**Solution**: Only expand once. Either remove expansion from `parse_emphasis_and_code/2` OR from `collect_text_tokens`.

### Issue: Some tabs expand, others don't

**Symptom**: Inconsistent tab handling.

**Cause**: Different token types taking different code paths.

**Solution**: Ensure both `{string, ...}` tokens and `{{ws, tab}, ...}` tokens get expanded.

---

## Reference Code

### Original tab handling (erlmd.erl line 706)

```erlang
htmlchars1([?TAB | T], Acc) -> htmlchars1(T, ["    " | Acc]);
```

Simple and clear - tabs become 4 spaces in text.

### Original code block encoding (erlmd.erl line 840)

```erlang
htmlencode([Else | Rest], Acc) -> htmlencode(Rest, [Else | Acc]).
```

Tabs pass through unchanged - remain as ASCII 9.

---

## Key Insights

1. **Two paths for tabs**: Token-level (`{{ws, tab}, "\t"}`) and string-level (`{string, "...\t..."}`)
2. **Expand during collection**: Simplest place to handle both paths
3. **Code blocks are separate**: They use different processing that preserves tabs
4. **One-line fix**: Just add expansion to existing collection clause

---

## Complexity Assessment

**Phase 6 is the SIMPLEST phase**:
- One line of code to change
- Clear problem and solution
- No state management
- No interaction with other phases
- Easy to test

---

## Deliverables

After completing Phase 6, provide:

1. **Test results**: Before and after numbers
2. **Code changes**: One-line diff
3. **Test file results**: Output from tab test files
4. **Verification**: Tabs in text expand, tabs in code don't

This information will guide Phase 7 (h2_or_hr Handling).

---

## Notes

**Relationship to Phase 3**: Tab expansion must come AFTER hard break detection (already correct in Phase 3).

**Relationship to Phase 2**: Code blocks don't go through inline parsing, so tabs are naturally preserved.

**Simplicity**: This is a one-line fix - just add tab expansion to string collection.

**Testing**: Very straightforward to test - just check if tab character (9) becomes 4 spaces (32,32,32,32) in text but not in code.
