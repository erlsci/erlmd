# Roadmap to 100% Test Compatibility for erlmd AST Refactoring

## Executive Summary

Achieving 100% test compatibility requires faithfully replicating the original `p1/4` function's behavior in `parse_blocks/3`. This is a **significant undertaking** that goes beyond simple bug fixes.

## Current Status

- **Tests:** 296 total
- **Failures:** ~40 (after initial fixes)
- **Target:** 0 failures

## Core Issue

The original `erlmd.erl` processes typed lines with complex lookahead and consumption logic BEFORE generating HTML. Our AST builder tries to create nodes immediately, missing critical preprocessing steps.

## Required Changes

### 1. List Consumption Logic (Critical - 14 tests)

**Original behavior:**
```erlang
%% From erlmd.erl lines 196-198
p1([{{ul, P1}, S1}, {{normal, P2}, S2} | T], R, I , Acc) ->
    p1([{{ul, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
```

**What this does:**
- When a list item is followed by a normal line, MERGE them
- Continue until no more normal/codeblock lines follow
- THEN create the list

**Required fix in parse_blocks:**
```erlang
%% Add BEFORE the general ul/ol parsing clauses

%% Unordered lists swallow normal lines
parse_blocks([{{ul, P1}, S1}, {normal, P2} | T], Refs, Acc) ->
    % Merge the normal line into the list item
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ul, Merged}, S1} | T], Refs, Acc);

%% Unordered lists swallow codeblock lines  
parse_blocks([{{ul, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ul, Merged}, S1 ++ S2} | T], Refs, Acc);

%% Same for ordered lists
parse_blocks([{{ol, P1}, S1}, {normal, P2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ol, Merged}, S1} | T], Refs, Acc);

parse_blocks([{{ol, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{{ol, Merged}, S1 ++ S2} | T], Refs, Acc);
```

**Impact:** Fixes 11 tests immediately

---

### 2. Codeblock Consumption Logic (Critical)

**Original behavior:**
```erlang
%% From erlmd.erl lines 211-218
p1([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], R, I, Acc) ->
    p1([{{codeblock, merge(P1, pad(I), P2)}, S1 ++ S2} | T], R, I, Acc);
p1([{{codeblock, P1}, S1} | T1], R, I, Acc) ->
    case grab_empties(T1) of
        {[{{codeblock, P2}, S2} | T2], E} ->
            p1([{{codeblock, merge(P1, pad(I), E ++ P2)}, S1 ++ E ++ S2} | T2],
               R, I, Acc);
```

**Required fix:**
```erlang
%% Codeblocks consume following codeblocks
parse_blocks([{{codeblock, P1}, S1}, {{codeblock, P2}, S2} | T], Refs, Acc) ->
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{{codeblock, Merged}, S1 ++ S2} | T], Refs, Acc);

%% Codeblocks consume empty lines then more codeblocks
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

---

### 3. parse_list Must Match Original's parse_list (Critical - 4 tests)

**Original logic for loose/tight detection:**
```erlang
%% From erlmd.erl lines 223-230
NewWrap2 = case T of
               []         -> false;
               [H2 | _T2] -> case H2 of
                                 {linefeed, _} -> true;
                                 _             -> false
                             end
           end,
```

**Key insight:** The decision about wrapping happens DURING iteration, checking if the CURRENT item is followed by linefeed.

**Required fix:**
```erlang
collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    % Check if THIS item is followed by blank/linefeed for NEXT item's wrap status
    NextWrap = case T of
        [] -> false;
        [{linefeed, _} | _] -> true;
        [{blank, _} | _] -> true;
        _ -> false
    end,
    
    {Rest, ItemContent, _} = grab_list_item(T, Refs, [], Tight),
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs, Tight),
    Item = #list_item{content = ItemBlocks},
    
    % If we're wrapping, all subsequent items wrap
    NewTight = Tight andalso (not NextWrap),
    
    collect_list_items(Type, Rest, Refs, [Item | Acc], NewTight);
```

**Impact:** Fixes tight/loose list rendering

---

### 4. Blockquote Merging (Critical - 3 tests)

**Original logic:**
```erlang
%% From erlmd.erl lines 178-183
p1([{blockquote, P1}, {blockquote, [_ | P2]} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1++[{tags, "<br />"}], [], P2)} | T], R, I, Acc);
p1([{blockquote, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{blockquote, merge(P1, [], P2)} | T], R, I, Acc);
```

**Required fix:**
```erlang
%% Blockquotes swallow each other - replace first > with space
parse_blocks([{blockquote, P1}, {blockquote, [_ | P2]} | T], Refs, Acc) ->
    Merged = merge_with_br(P1, P2),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);

%% Blockquotes swallow normal lines
parse_blocks([{blockquote, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);
```

---

### 5. Hard Line Break Detection (Critical - 3 tests)

**Problem:** We need to detect `"  \n"` or `"\t\n"` patterns and convert to hard breaks.

**Original approach:** Done in `merge/3` via `make_br/1`:
```erlang
make_br1([{{lf, _}, _}, {{ws, comp}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
make_br1([{{lf, _}, _}, {{ws, tab}, _} | T]) -> 
    reverse([{tags, " <br />\n"} | T]);
```

**Our challenge:** This happens at the TOKEN level before string conversion.

**Solution:** Detect pattern in tokens when building inline content:

```erlang
build_inline(Tokens, Refs) ->
    % Detect and mark hard breaks
    ProcessedTokens = mark_hard_breaks(Tokens),
    parse_inline_elements(ProcessedTokens, Refs, []).

mark_hard_breaks(Tokens) ->
    mark_hard_breaks(Tokens, []).

mark_hard_breaks([], Acc) ->
    lists:reverse(Acc);
% Two spaces before LF
mark_hard_breaks([{{ws, sp}, "  "}, {{lf, _}, LF} | T], Acc) ->
    mark_hard_breaks(T, [{{lf, hard}, LF} | Acc]);
% Tab before LF
mark_hard_breaks([{{ws, tab}, _}, {{lf, _}, LF} | T], Acc) ->
    mark_hard_breaks(T, [{{lf, hard}, LF} | Acc]);
mark_hard_breaks([H | T], Acc) ->
    mark_hard_breaks(T, [H | Acc]).
```

Then in `parse_emphasis_and_code`, check for hard breaks:
```erlang
%% Check for hard break marker
parse_emphasis_and_code([{{lf, hard}, _} | Rest], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(Rest, [LB | Acc]);
```

But wait - we're processing strings not tokens at that point. Need different approach.

**Better solution:** Convert tokens to string BUT preserve hard break info:
- When we see `  \n` in the string, check if it came from tokens with the pattern
- This requires tracking context...

**Simplest solution:** Expand tabs and detect two-space pattern in `parse_emphasis_and_code`:
```erlang
%% Hard line breaks - TWO spaces before newline
parse_emphasis_and_code([?SPACE, ?SPACE, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    % Consume the spaces
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?SPACE, ?SPACE, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Tab before newline is also hard break
parse_emphasis_and_code([?TAB, ?LF | T], Acc) ->
    % Don't expand tab, just create hard break
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?TAB, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% IMPORTANT: These must come BEFORE the general line break handlers
```

---

### 6. Tab Expansion (1 test)

**Issue:** Tabs in text should become 4 spaces.

**Fix already in code but not working** because tabs inside `{string, "text\tmore"}` aren't being processed.

**Solution:** Pre-expand tabs in strings during token collection:
```erlang
collect_text_tokens([{string, S} | T]) ->
    {Rest, More} = collect_text_tokens(T),
    % Expand tabs in strings
    Expanded = expand_tabs_in_string(S),
    {Rest, Expanded ++ More};

expand_tabs_in_string(S) ->
    lists:flatten([case C of ?TAB -> "    "; _ -> C end || C <- S]).
```

---

### 7. h2_or_hr Greedy Consumption

**Original:**
```erlang
p1([{h2_or_hr, P1}, {normal, P2} | T], R, I, Acc) ->
    p1([{normal, flatten([P1 | P2])} | T], R, I, Acc);
```

**Required:**
```erlang
%% h2_or_hr is greedy for normal lines
parse_blocks([{h2_or_hr, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{normal, Merged} | T], Refs, Acc);

%% h2_or_hr with no lookbehind is hr
parse_blocks([{h2_or_hr, _} | T], Refs, Acc) ->
    parse_blocks(T, Refs, [#horizontal_rule{} | Acc]);
```

---

### 8. Tag Handling

**Original treats tags specially:**
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

**Our current approach wraps in paragraph, which is different.**

---

## Implementation Strategy

### Phase 1: Add Consumption Clauses (Highest Priority)

Update `parse_blocks/3` to add these clauses **before** the general list/codeblock handlers:

1. List + normal/codeblock consumption (4 clauses)
2. Codeblock + codeblock consumption (2 clauses)
3. Blockquote + blockquote/normal consumption (2 clauses)
4. h2_or_hr + normal consumption (1 clause)

### Phase 2: Fix Hard Line Breaks

1. Add hard break detection to `parse_emphasis_and_code` (4 clauses - MUST be before general line breaks)
2. Fix tab handling in `collect_text_tokens`

### Phase 3: Fix List Tight/Loose Detection

1. Update `collect_list_items` to check for blank/linefeed after each item
2. Update `parse_list_item_content` to respect wrap flag

### Phase 4: Test and Iterate

Run tests after EACH change to verify:
- Failures decrease
- No new failures introduced
- Getting closer to 0 failures

## Estimated Effort

- **Phase 1:** 2-3 hours (8 new clauses, testing each)
- **Phase 2:** 1-2 hours (hard breaks are tricky)
- **Phase 3:** 1-2 hours (list logic is complex)
- **Phase 4:** 2-4 hours (iteration and debugging)

**Total:** 6-11 hours of focused development

## Risk Assessment

**High Risk Items:**
- List continuation logic (complex state management)
- Hard line break detection (token vs string issue)
- Tight/loose list detection (order-dependent logic)

**Medium Risk:**
- Blockquote merging
- Tab expansion
- h2_or_hr handling

**Low Risk:**
- Codeblock consumption (straightforward)

## Recommendation

Given the scope, I recommend:

1. **Create a feature branch** for this work
2. **Implement in phases** with tests after each
3. **Document each fix** as we go
4. **Be prepared for iteration** - some fixes may reveal new issues

Would you like me to:
- **A)** Start implementing Phase 1 right now (add all consumption clauses)
- **B)** Create a complete reference implementation showing all changes
- **C)** Focus on the highest-impact fixes first (lists, hard breaks)

Your call on how to proceed!
