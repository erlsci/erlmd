# Phase 8: Tag Handling - Implementation Instructions for Claude Code

## Overview

**Objective**: Verify and fix the handling of single HTML tags (non-block tags) to match the original's context-sensitive behavior.

**Estimated Time**: 1 hour  
**Expected Impact**: Fixes 1-2 failing tests  
**Priority**: LOW  
**Prerequisites**: Phases 1-7 should be complete

**🎉 THIS IS THE FINAL PHASE!** After this, we should have 100% test compatibility!

---

## Context

HTML tags in markdown have special handling depending on context:

### Block Tags vs Single Tags

**Block tags** (like `<div>`, `<table>`) are already handled by the `blocktag` type and work correctly.

**Single tags** (like `<flame on>`, `<br>`, etc.) are handled by the `tag` type and need special processing.

### Context-Sensitive Behavior

According to the original implementation, single tags have different behavior depending on what follows them:

#### Case 1: Tag at End of Document

**Input**:
```markdown
Text before
<flame on>
```

**Expected behavior**: Tag is wrapped with `<p>` and `</p>`

**Original code** (erlmd.erl line 124):
```erlang
[]  -> p1([], R, I, ["</p>", make_tag_str(Tag, R), "<p>" | Acc]);
```

#### Case 2: Tag Followed by Blank

**Input**:
```markdown
<flame on>

Next paragraph.
```

**Expected behavior**: Tag stands alone (not wrapped in `<p>`)

**Original code** (line 126):
```erlang
[{blank, _} | T2] -> p1(T2, R, I, [make_tag_str(Tag, R) | Acc]);
```

#### Case 3: Tag Followed by Other Content

**Input**:
```markdown
<flame on>
More text
```

**Expected behavior**: Tag has padding before it

**Original code** (line 128):
```erlang
_Other -> p1(T, R, I, [pad(I) ++ make_tag_str(Tag, R) | Acc])
```

---

## Current State Analysis

### What's Already Implemented

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_blocks/3`  
**Lines**: 84-92

```erlang
%% Tag handling - single tags should be wrapped in paragraphs (like unknown HTML)
parse_blocks([{tag, Tag} | T], Refs, Acc) ->
    % Single tags like <flame on> should be treated as inline HTML within a paragraph
    % Extract the raw HTML from the tag tokens
    TagStr = make_tag_str(Tag),
    Content = [#html_inline{content = TagStr}],
    Para = #paragraph{content = Content},
    parse_blocks(T, Refs, [Para | Acc]);
```

**Current behavior**: ALWAYS wraps tags in paragraphs, regardless of context.

**Original behavior**: Context-sensitive - checks what follows the tag.

### The Problem

Our implementation is simpler but doesn't match the original's nuanced behavior. We always create a paragraph, but the original:
- Adds `<p>` before and `</p>` after at end of document
- Outputs tag standalone if followed by blank
- Adds padding if followed by other content

However, the original's behavior seems complex and might not be well-tested. Let's verify if this actually causes test failures.

---

## Task 1: Run Tag Tests

**Action**: Identify which tag-related tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct --suite test/erlmd_SUITE | grep -i "tag\|html"
```

**Also search for specific tests**:
```bash
grep -rn "tag\|<flame\|<br\|<hr" test/erlmd_SUITE.erl | grep "test_"
```

**Capture**:
1. How many tag tests exist?
2. Which are failing?
3. What's the expected vs actual output?
4. Do any tests actually use single tags like `<flame on>`?

---

## Task 2: Understand the Original's Logic

**Action**: Study the original tag handling carefully.

**Reference**: `src/erlmd.erl` lines 122-131

```erlang
%% Tags have the highest precedence...
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

**Key observations**:

1. **End of document**: Outputs `</p> + tag + <p>` (seems to assume we're in a paragraph context)
2. **Followed by blank**: Outputs just the tag
3. **Other cases**: Outputs `padding + tag` and continues processing

**This is VERY different from normal block processing!** It seems to assume tags are part of the HTML output stream, not separate blocks.

### What is `make_tag_str`?

**Reference**: `src/erlmd.erl` lines 828-834

```erlang
make_tag_str(L, R) -> make_tag1(L, R, []).

make_tag1([], _R, Acc) -> lists:reverse(Acc);
make_tag1([{{{tag, _Type}, _Tag}, B} | T], R, Acc) ->
    make_tag1(T, R, [B | Acc]);
make_tag1([H | T], R, Acc) ->
    make_tag1(T, R, [make_str([H], R) | Acc]).
```

This extracts the raw HTML string from tag tokens. For `{tag, [{{{tag, Type}, Tag}, HTML}]}`, it returns the `HTML` string.

---

## Task 3: Create Minimal Test Cases

**Action**: Create test files to isolate tag scenarios.

### Test Case A: Tag at End

**File**: `test_tag_end.md`
```markdown
Text before
<br>
```

**Expected output** (based on original):
```html
<p>Text before</p>
</p><br><p>
```

Wait, that doesn't make sense. Let me re-read the original...

Actually, looking at the original again:
```erlang
[]  -> p1([], R, I, ["</p>", make_tag_str(Tag, R), "<p>" | Acc]);
```

This PREPENDS `"</p>"`, the tag, then `"<p>"` to the accumulator. Then it processes the empty list `[]` with that accumulator.

So if we had a paragraph before, the `</p>` closes it, the tag is output, then `<p>` opens a new (empty) paragraph.

But the final `<p>` might get cleaned up. This is confusing!

### Test Case B: Tag Followed by Blank

**File**: `test_tag_blank.md`
```markdown
<br>

Next paragraph.
```

**Expected output**:
```html
<br>

<p>Next paragraph.</p>
```

### Test Case C: Tag in Middle

**File**: `test_tag_middle.md`
```markdown
<br>
More text
```

**Expected output**:
```html
<br>
<p>More text</p>
```

### Test Case D: Multiple Tags

**File**: `test_tag_multiple.md`
```markdown
<br>
<br>
<br>
```

**Expected output**:
```html
<br>
<br>
<br>
```

### Test Case E: Tag with Content

**File**: `test_tag_content.md`
```markdown
<flame on>
```

**Expected output**:
```html
<flame on>
```

OR maybe:
```html
</p><flame on><p>
```

This is very unclear!

---

## Task 4: Test Original Implementation

**Action**: Run test cases through ONLY the original to see what it actually does.

```erlang
test_tag_original(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    Original = erlmd:conv_original(MD),
    
    io:format("~n=== Original output for ~s ===~n", [File]),
    io:format("~s~n", [Original]),
    io:format("Bytes: ~w~n", [Original]).

% Test each case
test_tag_original("test_tag_end.md").
test_tag_original("test_tag_blank.md").
test_tag_original("test_tag_middle.md").
test_tag_original("test_tag_multiple.md").
test_tag_original("test_tag_content.md").
```

**This will show us**: What does the original ACTUALLY output for these cases?

---

## Task 5: Test AST Implementation

**Action**: Run the same tests through our AST implementation.

```erlang
test_tag_ast(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    AST = erlmd:conv_ast(MD),
    
    io:format("~n=== AST output for ~s ===~n", [File]),
    io:format("~s~n", [AST]),
    io:format("Bytes: ~w~n", [AST]).

test_tag_ast("test_tag_end.md").
test_tag_ast("test_tag_blank.md").
test_tag_ast("test_tag_middle.md").
test_tag_ast("test_tag_multiple.md").
test_tag_ast("test_tag_content.md").
```

---

## Task 6: Compare and Analyze

**Action**: Compare original vs AST for each test case.

```erlang
test_tag_compare(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    Original = erlmd:conv_original(MD),
    AST = erlmd:conv_ast(MD),
    
    io:format("~n=== ~s ===~n", [File]),
    case Original =:= AST of
        true -> 
            io:format("✓ MATCH~n");
        false -> 
            io:format("✗ MISMATCH~n"),
            io:format("Original: ~s~n", [Original]),
            io:format("AST:      ~s~n", [AST])
    end.

test_tag_compare("test_tag_end.md").
test_tag_compare("test_tag_blank.md").
test_tag_compare("test_tag_middle.md").
test_tag_compare("test_tag_multiple.md").
test_tag_compare("test_tag_content.md").
```

**Capture**:
1. Which cases match?
2. Which cases differ?
3. What are the differences?

---

## Task 7: Check if Tag Tests Actually Exist

**Action**: Verify if the test suite actually tests this functionality.

```bash
# Search for tests with HTML tags
grep -rn "<flame\|<br>\|<hr>" test/erlmd_SUITE.erl

# Search for tag-related test names
grep -n "test.*tag\|test.*html" test/erlmd_SUITE.erl
```

**Important question**: Do any real tests actually depend on this behavior?

If there are NO tests for single tags, then the current implementation (always wrapping in paragraph) might be fine!

---

## Task 8: Decide on Approach

Based on findings from Tasks 1-7, decide:

### Option A: Match Original Exactly

If tests fail due to tag handling, implement the original's context-sensitive logic.

**Changes needed**:
```erlang
%% Tag handling - context sensitive
parse_blocks([{tag, Tag} | T], Refs, Acc) ->
    TagStr = make_tag_str(Tag),
    case T of
        % End of document
        [] ->
            % Close current paragraph, output tag, open new paragraph
            Content = [#html_inline{content = TagStr}],
            Para = #paragraph{content = Content},
            parse_blocks([], Refs, [Para | Acc]);
        
        % Followed by blank
        [{blank, _} | Rest] ->
            % Output tag standalone
            HTMLBlock = #html_inline{content = TagStr},
            parse_blocks(Rest, Refs, [HTMLBlock | Acc]);
        
        % Other cases
        _ ->
            % Wrap in paragraph
            Content = [#html_inline{content = TagStr}],
            Para = #paragraph{content = Content},
            parse_blocks(T, Refs, [Para | Acc])
    end;
```

Wait, but `#html_inline` is for inline HTML, not block-level. Let me reconsider...

Actually, the original outputs tags directly to the HTML string stream, not as blocks. This is fundamentally different from our AST approach.

### Option B: Keep Current Implementation

If NO tests fail, keep the current simple approach (always wrap in paragraph).

**Rationale**:
- Simpler code
- Single tags in markdown are rare
- Wrapping in paragraph is safe default

### Option C: Minimal Fix

If specific test cases fail, fix only those cases.

---

## Task 9: Implement Fix (If Needed)

**Only if tests are actually failing**, implement the fix.

### Approach 1: Context-Sensitive Wrapping

```erlang
parse_blocks([{tag, Tag} | T], Refs, Acc) ->
    TagStr = make_tag_str(Tag),
    
    case T of
        % Tag followed by blank - output as standalone HTML block
        [{blank, _} | Rest] ->
            HTMLBlock = #html_block{
                tag = "unknown",
                content = TagStr,
                type = self_closing
            },
            parse_blocks(Rest, Refs, [HTMLBlock | Acc]);
        
        % Tag in other contexts - wrap in paragraph
        _ ->
            Content = [#html_inline{content = TagStr}],
            Para = #paragraph{content = Content},
            parse_blocks(T, Refs, [Para | Acc])
    end;
```

### Approach 2: Always Standalone

```erlang
parse_blocks([{tag, Tag} | T], Refs, Acc) ->
    TagStr = make_tag_str(Tag),
    HTMLBlock = #html_block{
        tag = "unknown",
        content = TagStr,
        type = self_closing
    },
    parse_blocks(T, Refs, [HTMLBlock | Acc]);
```

---

## Task 10: Test the Fix

**Action**: Run all tests after applying fixes.

```bash
rebar3 ct
```

**Check**:
1. How many tests pass now?
2. Did tag tests pass?
3. Any regressions?

**Target**: 296/296 tests passing! 🎉

---

## Task 11: Verify make_tag_str Implementation

**Action**: Ensure our `make_tag_str` function works correctly.

**Location**: `src/erlmd_ast.erl`, around line 640

**Current implementation**:
```erlang
make_tag_str(Tokens) ->
    make_tag_str(Tokens, []).

make_tag_str([], Acc) ->
    lists:flatten(lists:reverse(Acc));
make_tag_str([{{{tag, _Type}, _Tag}, Content} | T], Acc) ->
    % Extract the raw tag content (e.g., "<flame on>")
    make_tag_str(T, [Content | Acc]);
make_tag_str([{_, Str} | T], Acc) when is_list(Str) ->
    make_tag_str(T, [Str | Acc]);
make_tag_str([_ | T], Acc) ->
    make_tag_str(T, Acc).
```

**Test it**:
```erlang
% Create a tag token
TagToken = [{{{tag, self_closing}, "br"}, "<br>"}].
Result = erlmd_ast:make_tag_str(TagToken).
io:format("Result: ~p~n", [Result]).
% Should be: "<br>"
```

---

## Task 12: Edge Cases

### Edge Case A: Self-Closing Tags

**Input**: `<br />`

**Expected**: Pass through as-is

### Edge Case B: Unknown Tags

**Input**: `<flame on>`

**Expected**: Pass through as-is (not recognized HTML)

### Edge Case C: Tags with Attributes

**Input**: `<span class="highlight">`

**Expected**: Pass through as-is

---

## Task 13: Document Findings

**Action**: Create a summary report.

```markdown
## Phase 8 Results - FINAL PHASE! 🎉

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Target**: 296/296 tests passing ✓

**Changes made**:
[List any changes, or "No changes needed - implementation already correct"]

**Root cause** (if any issues found):
[Explain what was wrong]

**Why the fix works** (if fix applied):
[Explain the solution]

**Test results**:
- ✓/✗ Tag at end of document
- ✓/✗ Tag followed by blank
- ✓/✗ Tag in middle of content
- ✓/✗ Multiple consecutive tags
- ✓/✗ Self-closing tags
- ✓/✗ Unknown tags

**Final Status**:
- Total tests: 296
- Passing: [count]
- Failing: [count]
- Success rate: [percentage]%

**Remaining issues** (if any):
[List any tests still failing]

**Completion**:
✅ Phase 1: List Consumption
✅ Phase 2: Codeblock Consumption
✅ Phase 3: Hard Line Breaks
✅ Phase 4: List Tight/Loose
✅ Phase 5: Blockquote Merging
✅ Phase 6: Tab Expansion
✅ Phase 7: h2_or_hr Handling
✅ Phase 8: Tag Handling

**🎉 ALL PHASES COMPLETE!**
```

---

## Success Criteria

Phase 8 (and the entire project) is complete when:

- ✅ All tag-related tests pass
- ✅ Tags are handled consistently with original
- ✅ No regressions in other tests
- ✅ **296/296 tests passing (100%)**

---

## Troubleshooting Guide

### Issue: Tests fail with wrong tag output

**Symptom**: Tags wrapped in `<p>` when they shouldn't be, or vice versa.

**Solution**: Implement context-sensitive handling (Approach 1 in Task 9).

### Issue: make_tag_str returns wrong format

**Symptom**: Tag HTML is malformed.

**Check**: Verify the tag token structure. Add debug:
```erlang
make_tag_str([{{{tag, Type}, Tag}, Content} | T], Acc) ->
    io:format("DEBUG: Tag type=~p, tag=~p, content=~p~n", [Type, Tag, Content]),
    make_tag_str(T, [Content | Acc]);
```

### Issue: No tag tests fail

**Symptom**: All tests pass even without changes.

**Conclusion**: Current implementation is sufficient! Tag handling might not be heavily tested.

---

## Reference Code

### Original tag handling (erlmd.erl lines 122-131)

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

### Original make_tag_str (erlmd.erl lines 828-834)

```erlang
make_tag_str(L, R) -> make_tag1(L, R, []).

make_tag1([], _R, Acc) -> lists:reverse(Acc);
make_tag1([{{{tag, _Type}, _Tag}, B} | T], R, Acc) ->
    make_tag1(T, R, [B | Acc]);
make_tag1([H | T], R, Acc) ->
    make_tag1(T, R, [make_str([H], R) | Acc]).
```

---

## Key Insights

1. **Tags are rare**: Single HTML tags in markdown content are uncommon
2. **Original is complex**: Context-sensitive behavior that might not be well-tested
3. **AST approach differs**: We create blocks, original manipulates HTML strings
4. **Simple might win**: Current implementation might be sufficient

---

## Complexity Assessment

**Phase 8 difficulty**: UNCERTAIN
- Depends on whether tests actually fail
- If no tests fail: TRIVIAL (no changes needed)
- If tests fail: MODERATE (implement context-sensitive logic)
- Original's logic is complex but may be rarely used

---

## Deliverables

After completing Phase 8 (FINAL PHASE!), provide:

1. **Final test results**: 296/296 or X/296 with explanation
2. **Code changes**: Any fixes applied
3. **Test file results**: Tag test outputs
4. **Project completion report**: Summary of all 8 phases
5. **Known issues**: Any remaining test failures with analysis

---

## Project Completion Checklist

After Phase 8, create a final report:

```markdown
# erlmd AST Refactoring - Project Completion Report

## Executive Summary

**Goal**: Achieve 100% test compatibility (296/296 tests passing)
**Phases completed**: 8/8
**Final test count**: [X/296]
**Success rate**: [X%]

## Phase Summary

| Phase | Description | Status | Tests Fixed |
|-------|-------------|--------|-------------|
| 1 | List Consumption | ✅ | ~11-14 |
| 2 | Codeblock Consumption | ✅ | ~3-5 |
| 3 | Hard Line Breaks | ✅ | ~3 |
| 4 | List Tight/Loose | ✅ | ~4 |
| 5 | Blockquote Merging | ✅ | ~3 |
| 6 | Tab Expansion | ✅ | ~1 |
| 7 | h2_or_hr Handling | ✅ | ~1-2 |
| 8 | Tag Handling | ✅ | ~1-2 |

## Achievements

- ✅ Created comprehensive AST type system
- ✅ Built modular HTML renderer
- ✅ Maintained backward compatibility
- ✅ Preserved all original functionality
- ✅ [Achieved 100% test compatibility] OR [Identified remaining issues]

## Architecture

**Before**:
```
Lexer → Type Lines → Parse → HTML String
```

**After**:
```
Lexer → Type Lines → Parse Blocks → AST → HTML Renderer
```

## Benefits

1. **Separation of concerns**: Parsing separate from rendering
2. **AST manipulation**: Can transform markdown before rendering
3. **Multiple renderers**: Easy to add JSON, XML, etc.
4. **Better testing**: Can test AST structure directly
5. **Maintainability**: Clearer code organization

## Next Steps (Future Work)

1. **Module organization**: Split into erlmd_lexer, erlmd_parser, etc.
2. **Documentation**: Add comprehensive EDoc
3. **Examples**: Show AST manipulation use cases
4. **Additional renderers**: JSON AST output, custom HTML
5. **Performance**: Profile and optimize if needed

## Conclusion

[Success statement or remaining work description]

**Total time invested**: [estimate based on phase times]
**Lines of code changed**: [estimate]
**Test compatibility**: [X/296 = Y%]

🎉 **Project Complete!** 🎉
```

---

## Notes

**Last phase**: This completes the 8-phase roadmap!

**Likely outcome**: Phase 8 may require no changes - tag handling might already be sufficient.

**Celebration worthy**: Reaching 296/296 tests is a MAJOR achievement!

**Next session**: Module organization and documentation (future work).

---

## 🎉 Congratulations!

If you've implemented all 8 phases, you've:
- Fixed 28-40+ failing tests
- Achieved or nearly achieved 100% compatibility
- Created a robust AST-based architecture
- Maintained backward compatibility
- Built a solid foundation for future work

**This is a significant accomplishment!** Well done! 🚀
