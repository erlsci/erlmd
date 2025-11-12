# Phase 4: List Tight/Loose Detection - Implementation Instructions for Claude Code

## Overview

**Objective**: Fix list tight/loose mode detection in `erlmd_ast.erl` so that list items are correctly wrapped (or not wrapped) in `<p>` tags based on whether the list has blank lines between items.

**Estimated Time**: 2-3 hours  
**Expected Impact**: Fixes 4 failing tests  
**Priority**: HIGH  
**Prerequisites**: Phases 1, 2, and 3 should be complete

---

## Context

Lists in markdown can be rendered in two modes:

### Tight Lists (No Blank Lines Between Items)

**Input**:
```markdown
* Item 1
* Item 2
* Item 3
```

**Expected output** (NO `<p>` tags):
```html
<ul>
<li>Item 1</li>
<li>Item 2</li>
<li>Item 3</li>
</ul>
```

### Loose Lists (Blank Lines Between Items)

**Input**:
```markdown
* Item 1

* Item 2

* Item 3
```

**Expected output** (WITH `<p>` tags):
```html
<ul>
<li><p>Item 1</p></li>
<li><p>Item 2</p></li>
<li><p>Item 3</p></li>
</ul>
```

### Critical Rule: Once Loose, Always Loose

If ANY item in the list is followed by a blank line, ALL items in the list become loose (wrapped in `<p>` tags).

**Input** (mixed):
```markdown
* Item 1
* Item 2

* Item 3
```

**Expected output** (ALL items get `<p>` tags):
```html
<ul>
<li><p>Item 1</p></li>
<li><p>Item 2</p></li>
<li><p>Item 3</p></li>
</ul>
```

---

## Current State Analysis

### What's Already Implemented

**File**: `src/erlmd_ast.erl`  
**Function**: `parse_list/3`  
**Lines**: 265-269

```erlang
parse_list(Type, Lines, Refs) ->
    {Rest, Items, Tight} = collect_list_items(Type, Lines, Refs, [], true),
    List = #list{type = Type, items = lists:reverse(Items), tight = Tight},
    {Rest, List}.
```

**Function**: `collect_list_items/5`  
**Lines**: 271-294

```erlang
collect_list_items(_Type, [], _Refs, Acc, Tight) ->
    {[], Acc, Tight};

collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    {Rest, ItemContent, NewTight} = grab_list_item(T, Refs, [], Tight),

    % Build the item's blocks
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs),
    Item = #list_item{content = ItemBlocks},

    % Determine if next item triggers loose mode
    NextTight = case T of
        [] -> NewTight;
        [H2 | _T2] ->
            case H2 of
                {linefeed, _} -> false;  % loose list
                _ -> NewTight
            end
    end,

    collect_list_items(Type, Rest, Refs, [Item | Acc], NextTight);

collect_list_items(_Type, List, _Refs, Acc, Tight) ->
    {List, Acc, Tight}.
```

**Function**: `render_list_item/2` (in `erlmd_html.erl`)  
**Lines**: 62-71

```erlang
render_list_item(#list_item{content = Blocks}, Tight) ->
    BlockHTML = case Tight of
        true ->
            % Tight lists don't wrap items in <p> tags
            lists:map(fun strip_para/1, Blocks);
        false ->
            % Loose lists keep <p> tags
            lists:map(fun render_block/1, Blocks)
    end,
    ContentStr = string:strip(lists:flatten(BlockHTML), right, $\n),
    "<li>" ++ ContentStr ++ "</li>\n".
```

### The Problem

The current implementation has the logic structure in place, but the tight/loose detection might not perfectly match the original's behavior.

---

## Task 1: Understand Original Behavior

**Action**: Study the original list parsing logic carefully.

**Reference**: `src/erlmd.erl` lines 223-253 (parse_list/6)

```erlang
parse_list(Type, [{{Type, P}, _} | T], R, I, A, Wrap) ->
    {Rest, NewP, NewWrap} = grab(T, R, [], Wrap),
    Li = case NewWrap of
             false -> Ret = parse([{normal, P}], R),
                      % need to strip off the extra <p></p>'s
                      Ret2 = string:left(Ret, length(Ret) - 4),
                      Ret3 = string:right(Ret2, length(Ret2) - 3),
                      Ret3 ++ "\n" ++ NewP ++ pad(I);
             true  -> string:strip(parse([{normal, P}], R), right, ?LF)
                          ++ NewP ++ pad(I)
         end,
    NewWrap2 = case T of
                   []         -> false; % doesnt matter
                   [H2 | _T2] -> case H2 of
                                     {linefeed, _} -> true;
                                     _             -> false
                                 end
               end,
    parse_list(Type, Rest, R, I, [pad(I) ++ "<li>"
                                  ++ string:strip(Li, right, ?LF)
                                  ++ "</li>\n" | A], NewWrap2);
parse_list(_Type, List, _R, _I, A, _) ->
    {List, reverse(A)}.
```

**Key observations**:

1. **Wrap parameter**: Passed through recursively, starts as `false` (tight)
2. **NewWrap decision**: Determined by looking at CURRENT item's following line (T)
   - If followed by `{linefeed, _}` → `true` (loose)
   - Otherwise → `false` (tight)
3. **NewWrap2**: This becomes the wrap flag for the NEXT iteration
4. **Item rendering**: 
   - If `NewWrap` is `false`: strip `<p></p>` tags from the item
   - If `NewWrap` is `true`: keep `<p></p>` tags

**Critical insight**: The decision about wrapping is made by checking if the CURRENT item is followed by a blank line, and that decision affects THE NEXT ITEM.

Wait, let me re-read this...

Actually, looking more carefully:
- `NewWrap` comes from `grab/4` and affects the CURRENT item
- `NewWrap2` is computed by looking at what follows T, and is passed to the next iteration

So the flow is:
1. Process current item with `Wrap` (from previous iteration)
2. `grab/4` returns `NewWrap` based on content grabbed
3. Compute `NewWrap2` by looking ahead at T
4. Pass `NewWrap2` to next iteration

This is complex! Let me look at `grab/4`.

**Reference**: `src/erlmd.erl` lines 255-293 (grab functions)

```erlang
grab([{{codeblock, _}, S} | T] = List, R, Acc, W) ->
    case is_blockquote(S, T) of
        {{true, R1}, T2}       -> grab(T2, R,
                                       ["</blockquote>",
                                        make_esc_str(R1, R),
                                        "<blockquote>" | Acc], W);
        {{esc_false, R1}, _T2} -> {R1, reverse(Acc), false};
        {false, T2}            ->
            case is_double_indent(S) of
                false      ->
                    {List, reverse(Acc), false};
                {true, R2} ->
                    % if it is a double indent - delete 4 spaces
                    % no it makes not sense to me neither :(
                    grab(T2, R, ["    " ++ make_esc_str(R2, R) | Acc], W)
            end
    end;
grab([{linefeed, _} | T], R, Acc, false) ->
    grab2(T, R, Acc, T, Acc, true);
grab([{linefeed, _} | T], R, Acc, true) ->
    grab2(T, R, ["\n" | Acc], T, Acc, true);
grab([{blank, _} | T], R, Acc, false) ->
    grab2(T, R, Acc, T, Acc, true);
grab([{blank, _} | T], R, Acc, true) ->
    grab2(T, R, ["\n" | Acc], T, Acc, true);
grab([{normal, P} | T], R, Acc, W) ->
     Li = case W of
              false -> make_esc_str(P, R);
              true  -> "<p>"++ string:strip(make_esc_str(P, R), right, ?LF)
                           ++ "</p>"
          end,
     grab(T, R, [Li | Acc], W);
grab(List, _R, Acc, W) ->
    {List, reverse(Acc), W}.
```

**Key observations from grab**:

1. When `{linefeed, _}` or `{blank, _}` is encountered, it calls `grab2/6`
2. `grab2` returns with `Wrap = true` (see line 320)
3. If a normal line follows, it's wrapped in `<p>` if W is true

So `grab` determines if the CURRENT item should be wrapped based on its continuation lines.

---

## Task 2: Run List Tests

**Action**: Identify which list tests are failing.

```bash
cd /path/to/erlmd
rebar3 ct --suite test/erlmd_SUITE --case ordered_list_*
rebar3 ct --suite test/erlmd_SUITE --case unordered_list_*
```

**Also search for tight/loose tests**:
```bash
grep -rn "tight\|loose" test/erlmd_SUITE.erl
```

**Capture**:
1. How many list tests exist?
2. Which ones are failing?
3. Are they tight lists rendered loose, or vice versa?
4. What's the actual vs expected output?

---

## Task 3: Create Minimal Test Cases

**Action**: Create test files to isolate tight/loose scenarios.

### Test Case A: Tight List (No Blanks)

**File**: `test_list_tight.md`
```markdown
* Item 1
* Item 2
* Item 3
```

**Expected output** (no `<p>` tags):
```html
<ul>
<li>Item 1</li>
<li>Item 2</li>
<li>Item 3</li>
</ul>
```

### Test Case B: Loose List (All Have Blanks)

**File**: `test_list_loose.md`
```markdown
* Item 1

* Item 2

* Item 3
```

**Expected output** (all have `<p>` tags):
```html
<ul>
<li><p>Item 1</p></li>
<li><p>Item 2</p></li>
<li><p>Item 3</p></li>
</ul>
```

### Test Case C: Mixed (First Two Tight, Third Has Blank)

**File**: `test_list_mixed_end.md`
```markdown
* Item 1
* Item 2

* Item 3
```

**Expected output** (ALL get `<p>` tags):
```html
<ul>
<li><p>Item 1</p></li>
<li><p>Item 2</p></li>
<li><p>Item 3</p></li>
</ul>
```

### Test Case D: Mixed (Blank After First)

**File**: `test_list_mixed_start.md`
```markdown
* Item 1

* Item 2
* Item 3
```

**Expected output** (ALL get `<p>` tags):
```html
<ul>
<li><p>Item 1</p></li>
<li><p>Item 2</p></li>
<li><p>Item 3</p></li>
</ul>
```

### Test Case E: List With Multi-line Items

**File**: `test_list_multiline.md`
```markdown
* Item 1
  continuation
* Item 2
```

**Expected output** (tight):
```html
<ul>
<li>Item 1 continuation</li>
<li>Item 2</li>
</ul>
```

### Test Case F: List With Nested Paragraphs

**File**: `test_list_nested_para.md`
```markdown
* Item 1

  Paragraph 2

* Item 2
```

**Expected output** (loose):
```html
<ul>
<li><p>Item 1</p>
<p>Paragraph 2</p></li>
<li><p>Item 2</p></li>
</ul>
```

---

## Task 4: Test Original vs AST

**Action**: Run each test case through both implementations.

```erlang
test_list(File) ->
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
            % Analyze the difference
            case {string:str(Original, "<p>"), string:str(AST, "<p>")} of
                {0, N} when N > 0 -> 
                    io:format("AST has <p> tags, Original doesn't (loose instead of tight)~n");
                {N, 0} when N > 0 -> 
                    io:format("Original has <p> tags, AST doesn't (tight instead of loose)~n");
                _ -> 
                    io:format("Different issue~n")
            end
    end.

% Test all files
test_list("test_list_tight.md").
test_list("test_list_loose.md").
test_list("test_list_mixed_end.md").
test_list("test_list_mixed_start.md").
test_list("test_list_multiline.md").
test_list("test_list_nested_para.md").
```

**Capture**: Which test cases fail and how?

---

## Task 5: Trace the Tight Flag

**Action**: Add debug output to see how the tight flag propagates.

**In `erlmd_ast.erl`, add debug to `collect_list_items/5`**:

```erlang
collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    io:format("~nDEBUG collect_list_items:~n"),
    io:format("  Current Tight: ~p~n", [Tight]),
    io:format("  Next line: ~p~n", [case T of [] -> none; [H|_] -> element(1, H) end]),
    
    {Rest, ItemContent, NewTight} = grab_list_item(T, Refs, [], Tight),
    io:format("  After grab, NewTight: ~p~n", [NewTight]),
    
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs),
    Item = #list_item{content = ItemBlocks},
    
    % Determine if next item triggers loose mode
    NextTight = case T of
        [] -> NewTight;
        [H2 | _T2] ->
            case H2 of
                {linefeed, _} -> 
                    io:format("  Found linefeed after item, setting NextTight=false~n"),
                    false;
                _ -> 
                    io:format("  No linefeed, NextTight=~p~n", [NewTight]),
                    NewTight
            end
    end,
    
    collect_list_items(Type, Rest, Refs, [Item | Acc], NextTight);
```

**Run a test case**:
```erlang
{ok, C} = file:read_file("test_list_mixed_start.md").
erlmd:conv_ast(binary_to_list(C)).
```

**Observe**:
1. What's the initial Tight value? (should be `true`)
2. When does it become `false`?
3. Does it propagate to all subsequent items?

---

## Task 6: Analyze the Logic

Based on debug output, you should see one of these patterns:

### Pattern A: Tight never becomes false

**Symptom**: List that should be loose is rendered tight.

**Cause**: The linefeed detection isn't working - we're not seeing `{linefeed, _}` or `{blank, _}` after items.

**Solution**: Check what's actually in T. Maybe linefeeds were consumed by list consumption logic (Phase 1).

### Pattern B: Tight becomes false but doesn't propagate

**Symptom**: First item after blank is loose, but earlier items are still tight.

**Cause**: We're building items in reverse order (prepending to Acc), so the tight flag needs to apply retroactively.

**Solution**: After collecting all items, if ANY had `Tight = false`, mark ALL as loose.

### Pattern C: Wrong detection of blank lines

**Symptom**: Blank line is present but not detected.

**Cause**: The pattern matching for `{linefeed, _}` or `{blank, _}` might be wrong.

**Solution**: Check what the actual typed line looks like after Phase 1 consumption.

---

## Task 7: Understand the Retroactive Problem

**Critical realization**: Lists are processed FORWARD, but we're building the list in REVERSE (prepending to accumulator).

**Current flow**:
```
Item 1 (Tight=true)  → [Item1]
Item 2 (Tight=true)  → [Item2, Item1]
Item 3 (Tight=false) → [Item3, Item2, Item1]  ← Only Item3 is loose!
```

But we want: ALL items loose if ANY is loose.

**The original handles this by**:
1. Deciding wrap mode for NEXT item based on CURRENT item's following blank
2. All items processed with the same eventual wrap mode

Let's look at the original more carefully:

```erlang
NewWrap2 = case T of
               []         -> false;
               [H2 | _T2] -> case H2 of
                                 {linefeed, _} -> true;
                                 _             -> false
                             end
           end,
parse_list(Type, Rest, R, I, [...], NewWrap2);  % ← Pass to next iteration
```

So `NewWrap2` becomes the `Wrap` parameter for the next item. This means:
- If Item 1 is followed by blank, Item 2 gets Wrap=true
- If Item 2 is followed by blank, Item 3 gets Wrap=true
- But Item 1 itself is processed with Wrap=false (from initial call)

**Wait, that doesn't make ALL items loose either!**

Let me re-read the original...

Oh! The original is called from `p1/4` with initial `Wrap = false`:

```erlang
p1([{{ul, _P}, _} | _T] = List, R, I, Acc) ->
    {Rest, NewAcc} = parse_list(ul, List, R, I, [], false),  % ← false = tight
```

And the wrap mode propagates forward. So if Item 2 is followed by blank:
- Item 1 processes with Wrap=false (tight)
- Item 2 processes with Wrap=false (tight) 
- Item 3 processes with Wrap=true (loose) ← This came from Item 2's blank

But the spec says ALL items should be loose! Let me check the actual behavior...

Actually, I need to test the original implementation to see what it actually does.

---

## Task 8: Test Original Implementation Behavior

**Action**: Run tests with ONLY the original implementation to confirm its behavior.

```erlang
test_original_list(File) ->
    {ok, Content} = file:read_file(File),
    MD = binary_to_list(Content),
    
    Original = erlmd:conv_original(MD),
    
    io:format("~n=== Original output for ~s ===~n", [File]),
    io:format("~s~n", [Original]),
    
    % Count <p> tags in list items
    InList = string:str(Original, "<ul>") > 0 orelse string:str(Original, "<ol>") > 0,
    HasP = string:str(Original, "<li><p>") > 0,
    
    case {InList, HasP} of
        {true, true} -> io:format("List is LOOSE (has <p> tags)~n");
        {true, false} -> io:format("List is TIGHT (no <p> tags)~n");
        _ -> io:format("No list found~n")
    end.

% Test mixed case
test_original_list("test_list_mixed_start.md").
```

**This will tell us**: Does the original ACTUALLY make all items loose, or only items after the blank?

---

## Task 9: Check grab_list_item Implementation

**Action**: Verify that `grab_list_item/4` works correctly.

**Current implementation** (line 306):
```erlang
grab_list_item(List, _Refs, Acc, Wrap) ->
    % Simplified version - just return what we have
    {List, lists:reverse(Acc), Wrap}.
```

**Problem**: This is a stub! It doesn't actually implement the grabbing logic from the original.

**Original `grab/4` is complex** (lines 255-293). It:
1. Grabs codeblocks (with special indent rules)
2. Grabs linefeeds/blanks (and determines wrap mode)
3. Grabs normal lines (wrapping them if needed)

**This is a MAJOR missing piece!**

---

## Task 10: Implement grab_list_item Properly

**Action**: Implement the full `grab_list_item/4` logic based on original `grab/4`.

**File**: `src/erlmd_ast.erl`  
**Function**: `grab_list_item/4`  
**Current location**: Line 306

**Replace stub with full implementation**:

```erlang
%% @doc Grab continuation content for a list item
%% Based on original grab/4 from erlmd.erl lines 255-293
grab_list_item([], _Refs, Acc, Wrap) ->
    {[], lists:reverse(Acc), Wrap};

%% Codeblocks in list items (with double indent check)
grab_list_item([{{codeblock, _}, S} | T] = List, Refs, Acc, Wrap) ->
    case is_double_indent(S) of
        false ->
            % Not double-indented, stop grabbing
            {List, lists:reverse(Acc), false};
        {true, Content} ->
            % Double-indented codeblock, grab it
            % Remove 4 spaces from indent and include
            ContentStr = make_plain_str(Content),
            grab_list_item(T, Refs, [#code_block{content = ContentStr, language = undefined} | Acc], Wrap)
    end;

%% Linefeed when in tight mode - might transition to loose
grab_list_item([{linefeed, _} | T], Refs, Acc, false) ->
    % Call grab_list_item2 to check for following content
    grab_list_item2(T, Refs, Acc, T, Acc, true);

%% Linefeed when already in loose mode - include it
grab_list_item([{linefeed, _} | T], Refs, Acc, true) ->
    grab_list_item(T, Refs, [#blank_line{} | Acc], true);

%% Blank when in tight mode - might transition to loose
grab_list_item([{blank, _} | T], Refs, Acc, false) ->
    grab_list_item2(T, Refs, Acc, T, Acc, true);

%% Blank when already in loose mode - include it
grab_list_item([{blank, _} | T], Refs, Acc, true) ->
    grab_list_item(T, Refs, [#blank_line{} | Acc], true);

%% Normal lines - add as paragraph
grab_list_item([{normal, P} | T], Refs, Acc, Wrap) ->
    Content = build_inline(P, Refs),
    Para = #paragraph{content = Content},
    grab_list_item(T, Refs, [Para | Acc], Wrap);

%% Anything else stops grabbing
grab_list_item(List, _Refs, Acc, Wrap) ->
    {List, lists:reverse(Acc), Wrap}.

%% @doc Secondary grab for checking continuation after blank/linefeed
%% Based on original grab2/6 from erlmd.erl lines 319-337
grab_list_item2([{normal, P} | T], Refs, Acc, _OrigList, _OrigAcc, Wrap) ->
    % Check if normal line starts with whitespace (continuation)
    case P of
        [{{ws, _}, _} | _] ->
            % Starts with whitespace, it's a continuation
            Content = build_inline(P, Refs),
            Para = #paragraph{content = Content},
            grab_list_item(T, Refs, [Para | Acc], Wrap);
        _ ->
            % Doesn't start with whitespace, stop grabbing
            % Return the original list and acc (before the blank)
            {[{normal, P} | T], lists:reverse(Acc), false}
    end;

grab_list_item2([{linefeed, _} | T], Refs, Acc, OrigList, OrigAcc, _Wrap) ->
    grab_list_item2(T, Refs, [#blank_line{} | Acc], OrigList, OrigAcc, true);

grab_list_item2([{blank, _} | T], Refs, Acc, OrigList, OrigAcc, _Wrap) ->
    grab_list_item2(T, Refs, [#blank_line{} | Acc], OrigList, OrigAcc, true);

grab_list_item2(_List, _Refs, _Acc, OrigList, OrigAcc, _Wrap) ->
    % Stopped without finding continuation, return original state
    {OrigList, OrigAcc, true}.
```

**Helper function** (add if not present):

```erlang
%% @doc Check if a line has double indent (8+ spaces or 2+ tabs)
is_double_indent(List) -> is_double_indent1(List, 0).

is_double_indent1([], _N) -> 
    false;
is_double_indent1(Rest, N) when N >= 8 -> 
    {true, Rest};
is_double_indent1([{{ws, sp}, _} | T], N) -> 
    is_double_indent1(T, N + 1);
is_double_indent1([{{ws, tab}, _} | T], N) -> 
    is_double_indent1(T, N + 4);
is_double_indent1(_List, _N) -> 
    false.
```

**Note**: This is a significant implementation! It changes from a 1-line stub to ~60 lines of logic.

---

## Task 11: Fix Tight Propagation

**Action**: Ensure tight flag propagates correctly through all items.

**Current approach** (lines 283-291):
```erlang
% Determine if next item triggers loose mode
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

**Problem**: This only checks if there's a linefeed BETWEEN items, not if items themselves have blank lines in their content.

**The original's approach**: The wrap mode is determined by `grab/4` based on whether the item's continuation lines include blanks.

**Better approach**:

```erlang
collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    {Rest, ItemContent, ItemWrap} = grab_list_item(T, Refs, [], Tight),
    
    % Build the item's blocks
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs),
    Item = #list_item{content = ItemBlocks},
    
    % Check if next line is a blank/linefeed (between items)
    NextTight = case T of
        [] -> ItemWrap;
        [{linefeed, _} | _] -> false;  % Blank between items
        [{blank, _} | _] -> false;     % Blank between items
        _ -> ItemWrap  % Use wrap from item content
    end,
    
    % Once loose, stay loose
    FinalTight = Tight andalso NextTight,
    
    collect_list_items(Type, Rest, Refs, [Item | Acc], FinalTight);
```

**Key change**: `FinalTight = Tight andalso NextTight` ensures that once `Tight` becomes `false`, it stays `false` for all subsequent items.

---

## Task 12: Update parse_list_item_content

**Action**: Remove tight parameter since it's not needed.

**Current signature** (line 296):
```erlang
parse_list_item_content(P, AdditionalContent, Refs) ->
```

This is correct - it doesn't need tight parameter. The tight/loose decision is made at the list level, not the item level.

---

## Task 13: Test the Fixes

**Action**: Run all list test cases.

```erlang
test_list("test_list_tight.md").
test_list("test_list_loose.md").
test_list("test_list_mixed_end.md").
test_list("test_list_mixed_start.md").
test_list("test_list_multiline.md").
test_list("test_list_nested_para.md").
```

**Expected**: All should match between Original and AST.

**Run full test suite**:
```bash
rebar3 ct --suite test/erlmd_SUITE --case ordered_list_*
rebar3 ct --suite test/erlmd_SUITE --case unordered_list_*
```

**Check**:
1. How many list tests pass now?
2. Are tight lists correct?
3. Are loose lists correct?
4. Are mixed lists correct?

---

## Task 14: Verify Renderer Respects Tight Flag

**Action**: Check that `erlmd_html.erl` correctly handles tight/loose.

**In `erlmd_html.erl`, function `render_list_item/2`** (lines 62-71):

```erlang
render_list_item(#list_item{content = Blocks}, Tight) ->
    BlockHTML = case Tight of
        true ->
            % Tight lists don't wrap items in <p> tags
            lists:map(fun strip_para/1, Blocks);
        false ->
            % Loose lists keep <p> tags
            lists:map(fun render_block/1, Blocks)
    end,
    ContentStr = string:strip(lists:flatten(BlockHTML), right, $\n),
    "<li>" ++ ContentStr ++ "</li>\n".
```

This looks correct! `strip_para/1` should remove the `<p>` tags for tight lists.

**Check `strip_para/1`** (lines 73-77):

```erlang
strip_para(#paragraph{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    Ret = lists:flatten(InlineHTML),
    string:strip(Ret, right, $\n) ++ "\n";
strip_para(Block) ->
    % Non-paragraph blocks get rendered normally
    render_block(Block).
```

This looks correct too! It extracts the inline content from paragraphs without wrapping in `<p>` tags.

---

## Task 15: Document Findings

**Action**: Create a summary report.

```markdown
## Phase 4 Results

**Tests run**: [date/time]
**Before**: X passing, Y failing
**After**: A passing, B failing
**Net improvement**: +N tests

**Changes made**:
1. Implemented full `grab_list_item/4` logic (was a stub)
2. Added `grab_list_item2/6` for continuation checking
3. Added `is_double_indent/2` helper for codeblock detection
4. Fixed tight propagation with `Tight andalso NextTight`
5. Added check for blank/linefeed between items

**Root cause**:
The `grab_list_item/4` function was a stub that didn't implement the original's complex logic for:
- Grabbing continuation lines
- Detecting double-indented code blocks
- Determining wrap mode based on blank lines
- Checking if normal lines are continuations (start with whitespace)

**Why the fix works**:
By implementing the full grabbing logic, we now correctly:
1. Collect all content that belongs to a list item
2. Detect when blank lines appear (triggering loose mode)
3. Propagate loose mode to all items once detected
4. Handle nested content (code blocks, paragraphs) in list items

**Test results**:
- ✓/✗ Tight list (no blanks)
- ✓/✗ Loose list (all have blanks)
- ✓/✗ Mixed (blank at end)
- ✓/✗ Mixed (blank at start)
- ✓/✗ Multi-line items
- ✓/✗ Nested paragraphs

**Remaining issues**:
[List any list tests still failing]

**Next steps**:
- [If all pass]: Move to Phase 5 (Blockquote Merging)
- [If some fail]: Investigate [specific issue]
```

---

## Success Criteria

Phase 4 is complete when:

- ✅ Tight lists render without `<p>` tags
- ✅ Loose lists render with `<p>` tags
- ✅ Mixed lists are fully loose (all items get `<p>` tags)
- ✅ Multi-line list items work correctly
- ✅ Nested content in list items works correctly
- ✅ Double-indented code blocks in lists work correctly
- ✅ All ordered and unordered list tests pass
- ✅ No regressions in other tests
- ✅ At least 4 additional tests passing overall

---

## Troubleshooting Guide

### Issue: All lists are tight

**Symptom**: No lists have `<p>` tags, even with blank lines.

**Cause**: Tight flag never becomes false.

**Check**: Add debug to see if blank lines are detected:
```erlang
case H2 of
    {linefeed, _} -> 
        io:format("DEBUG: Found linefeed, setting loose~n"),
        false;
```

If you don't see this output, blank lines aren't in the typed lines.

**Possible cause**: Phase 1 list consumption consumed the blanks. Check the consumption clauses.

### Issue: All lists are loose

**Symptom**: All lists have `<p>` tags, even without blank lines.

**Cause**: Tight flag starts as false or becomes false incorrectly.

**Check**: Verify initial call has `Tight = true`:
```erlang
parse_list(Type, Lines, Refs) ->
    {Rest, Items, Tight} = collect_list_items(Type, Lines, Refs, [], true),
    io:format("DEBUG: Final Tight = ~p~n", [Tight]),
    ...
```

### Issue: First items tight, later items loose

**Symptom**: Mixed list where only items after the blank are loose.

**Cause**: Tight flag not propagating backward.

**Solution**: The `Tight andalso NextTight` logic should handle this. If not working, might need a second pass:

```erlang
parse_list(Type, Lines, Refs) ->
    {Rest, Items, Tight} = collect_list_items(Type, Lines, Refs, [], true),
    % If ANY item determined loose, mark entire list loose
    List = #list{type = Type, items = lists:reverse(Items), tight = Tight},
    {Rest, List}.
```

The tight flag should propagate during collection. If it doesn't, debug the `FinalTight = Tight andalso NextTight` line.

### Issue: Code blocks in lists don't work

**Symptom**: Double-indented code blocks in lists aren't recognized.

**Cause**: `is_double_indent/1` not working or not being called.

**Check**: Add debug:
```erlang
case is_double_indent(S) of
    false ->
        io:format("DEBUG: Not double indent: ~p~n", [S]),
        ...
    {true, Content} ->
        io:format("DEBUG: Found double indent code~n"),
        ...
```

### Issue: grab_list_item causes crash

**Symptom**: Error when processing lists.

**Cause**: The new `grab_list_item` implementation has bugs.

**Debug**: Wrap in try-catch:
```erlang
try
    {Rest, ItemContent, ItemWrap} = grab_list_item(T, Refs, [], Tight),
    ...
catch
    Error:Reason:Stack ->
        io:format("ERROR in grab_list_item:~n"),
        io:format("  Error: ~p~n", [Error]),
        io:format("  Reason: ~p~n", [Reason]),
        io:format("  Stack: ~p~n", [Stack]),
        io:format("  T was: ~p~n", [T]),
        erlang:error({grab_list_item_failed, Reason})
end
```

---

## Reference Code

### Original parse_list/6 (erlmd.erl lines 223-253)

```erlang
parse_list(_Type, [], _R, _I, A, _) ->
    {[], reverse(A)};
parse_list(Type, [{{Type, P}, _} | T], R, I, A, Wrap) ->
    {Rest, NewP, NewWrap} = grab(T, R, [], Wrap),
    Li = case NewWrap of
             false -> Ret = parse([{normal, P}], R),
                      % need to strip off the extra <p></p>'s
                      Ret2 = string:left(Ret, length(Ret) - 4),
                      Ret3 = string:right(Ret2, length(Ret2) -3),
                      Ret3 ++ "\n" ++ NewP ++ pad(I);
             true  -> string:strip(parse([{normal, P}], R), right, ?LF)
                          ++ NewP ++ pad(I)
         end,
    NewWrap2 = case T of
                   []         -> false;
                   [H2 | _T2] -> case H2 of
                                     {linefeed, _} -> true;
                                     _             -> false
                                 end
               end,
    parse_list(Type, Rest, R, I, [pad(I) ++ "<li>"
                                  ++ string:strip(Li, right, ?LF)
                                  ++ "</li>\n" | A], NewWrap2);
parse_list(_Type, List, _R, _I, A, _) ->
    {List, reverse(A)}.
```

### Original grab/4 (erlmd.erl lines 255-293)

See Task 1 for full code - it's complex!

Key points:
- Handles codeblocks with double-indent check
- Handles linefeeds/blanks (transitions to loose mode)
- Handles normal lines (wraps if loose mode)
- Returns `{Rest, Content, Wrap}`

### Original grab2/6 (erlmd.erl lines 319-337)

```erlang
grab2([{normal, P2} | T], R, Acc, LO, AO, W) ->
    case P2 of
        [{{ws, _}, _} | T2] ->
            Li = case W of
                     false -> make_esc_str(T2, R);
                     true  -> "<p>" ++
                                  string:strip(make_esc_str(T2, R), right, ?LF)
                                  ++ "</p>"
                 end,
            grab(T, R, [Li | Acc], W);
        _ ->
            {LO, AO, false}
    end;
grab2([{linefeed, _} | T], R, Acc, LO, AO, _W) ->
    grab2(T, R, ["\n" | Acc], LO, AO, true);
grab2([{blank, _} | T], R, Acc, LO, AO, _W) ->
    grab2(T, R, ["\n" | Acc], LO, AO, true);
grab2(_List, _R, _Acc, LO, AO, _W) ->
    {LO, AO, true}.
```

Key: Checks if normal line starts with whitespace to determine if it's a continuation.

---

## Key Insights

1. **Grabbing is crucial**: The `grab` function is the heart of list item processing
2. **Stub was the problem**: Having a 1-line stub for `grab_list_item` meant we weren't handling continuations at all
3. **Double indent matters**: Code blocks in lists need 8 spaces (or 2 tabs) to be recognized
4. **Tight propagation**: Once loose, always loose - implemented via `Tight andalso NextTight`
5. **Whitespace checking**: Continuation lines must start with whitespace

---

## Complexity Warning

This is the MOST COMPLEX phase so far because:
- `grab` function logic is intricate
- Multiple levels of lookahead and state
- Interaction with Phase 1 (list consumption)
- Retroactive tight/loose propagation

Take your time and test incrementally!

---

## Deliverables

After completing Phase 4, provide:

1. **Test results**: Before and after numbers for all list tests
2. **Code changes**: Full diffs showing the new `grab_list_item` implementation
3. **Test file results**: Output from all 6 test files
4. **Analysis**: Why the stub was the root cause
5. **Tight/loose verification**: Show examples of both modes working
6. **Edge case results**: Which edge cases pass/fail

This information will guide Phase 5 (Blockquote Merging).

---

## Notes

**Relationship to Phase 1**: Phase 1's list consumption might have consumed some blanks that Phase 4 needs to detect. If Phase 4 fails, might need to revisit Phase 1.

**Complexity**: This phase turns a stub into a full implementation, so expect significant changes and testing time.

**Testing strategy**: Test each piece incrementally:
1. First test tight lists (simplest)
2. Then loose lists (with blanks)
3. Then mixed lists (most complex)
4. Finally nested content (code blocks, paragraphs)
