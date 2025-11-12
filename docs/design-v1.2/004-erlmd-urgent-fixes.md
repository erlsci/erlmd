# Urgent Fixes for erlmd_ast Test Failures

## Critical Fix 1: collect_text_tokens Crash (MUST FIX FIRST)

**Error:** `erlang,'++' [none,"***blah\na"]` - trying to concatenate atom `none` with string

**Location:** `src/erlmd_ast.erl` line 427 in `collect_text_tokens`

**Problem:** The special whitespace token `{{ws, none}, none}` has `none` as the content, which is an atom not a string.

**Fix:**
```erlang
%% @doc Collect text tokens until we hit a special marker
collect_text_tokens([]) ->
    {[], ""};
collect_text_tokens([{string, S} | T]) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
% Stop at inline markup (links, images)
collect_text_tokens([{{inline, _}, _} | _] = List) ->
    {List, ""};
% Stop at images (bang followed by inline open)
collect_text_tokens([{{punc, bang}, _}, {{inline, open}, _} | _] = List) ->
    {List, ""};
% Stop at URLs and email addresses
collect_text_tokens([{url, _} | _] = List) ->
    {List, ""};
collect_text_tokens([{email, _} | _] = List) ->
    {List, ""};
% Stop at HTML tags
collect_text_tokens([{tags, _} | _] = List) ->
    {List, ""};
collect_text_tokens([{{{tag, _}, _}, _Content} | _] = List) ->
    {List, ""};
% CRITICAL FIX: Handle special whitespace token {{ws, none}, none}
collect_text_tokens([{{ws, none}, none} | T]) ->
    % This is non-space-filling whitespace - skip it
    collect_text_tokens(T);
% Handle other token types - but check if content is a list
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
% Skip tokens with non-list content (atoms, numbers, etc)
collect_text_tokens([{_, _} | T]) ->
    collect_text_tokens(T).
```

## Critical Fix 2: Backslash Handling

**Problem:** Backslashes before non-markdown-special characters should be kept literal

**Current behavior:** `\a` becomes `a` (wrong)
**Expected:** `\a` should stay `\a`

**Fix in parse_emphasis_and_code:**

```erlang
%% General backslash escape for markdown special characters
parse_emphasis_and_code([$\\ | T], Acc) ->
    case T of
        [C | Rest] when C =:= $*; C =:= $_; C =:= $`; C =:= $\\; 
                        C =:= $[; C =:= $]; C =:= $(; C =:= $); 
                        C =:= $!; C =:= $#; C =:= $+; C =:= $-; 
                        C =:= $.; C =:= $> ->
            % Markdown special character - consume backslash, output character
            TextNode = #text{content = [C]},
            parse_emphasis_and_code(Rest, [TextNode | Acc]);
        [_C | _Rest] ->
            % NOT a markdown special - keep the backslash
            TextNode = #text{content = "\\"},
            parse_emphasis_and_code(T, [TextNode | Acc]);
        [] ->
            % Trailing backslash
            TextNode = #text{content = "\\"},
            parse_emphasis_and_code([], [TextNode | Acc])
    end;
```

## Fix 3: Trailing Spaces in URLs

**Problem:** URLs like `(path/jpg.jpg )` keep the trailing space

**Location:** In `parse_url_title` and URL/title parsing

**Fix in split_url_title:**

```erlang
%% @doc Split URL from title (title is in quotes)
split_url_title([], Acc) ->
    % Strip trailing whitespace from URL
    {strip_trailing_ws(lists:reverse(Acc)), []};
split_url_title([{{punc, doubleq}, _} | Rest], Acc) ->
    {Title, _} = collect_until_quote(Rest, []),
    {strip_trailing_ws(lists:reverse(Acc)), Title};
split_url_title([{{punc, singleq}, _} | Rest], Acc) ->
    {Title, _} = collect_until_single_quote(Rest, []),
    {strip_trailing_ws(lists:reverse(Acc)), Title};
split_url_title([{{ws, _}, _} | Rest], Acc) ->
    % Collect whitespace but continue - might be before title
    split_url_title(Rest, [{{ws, sp}, " "} | Acc]);
split_url_title([H | Rest], Acc) ->
    split_url_title(Rest, [H | Acc]).

%% Strip trailing whitespace tokens from URL
strip_trailing_ws(Tokens) ->
    strip_trailing_ws(lists:reverse(Tokens), []).

strip_trailing_ws([{{ws, _}, _} | Rest], Acc) ->
    strip_trailing_ws(Rest, Acc);
strip_trailing_ws(List, Acc) ->
    lists:reverse(Acc, List).
```

## Fix 4: Image with Space Before Paren

**Problem:** `![Alt] (path)` is not recognized (space between `]` and `(`)

**Current code handles this for images but might not be working**

**Verify this clause exists and is BEFORE the normal link parsing:**

```erlang
%% Images: ![alt](url "title") - note: space allowed before (
parse_inline_elements([{{punc, bang}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    case get_inline_content(T, Refs, img) of
        {Rest, {Url, Title, AltText}} ->
            AltInline = [#text{content = AltText}],
            Image = #image{alt_text = AltInline, url = Url, title = Title},
            parse_inline_elements(Rest, Refs, [Image | Acc]);
        {Rest, Text} ->
            % Not a valid image, treat as text
            TextNode = #text{content = "![" ++ Text},
            parse_inline_elements(Rest, Refs, [TextNode | Acc])
    end;
```

And in `parse_inline_link_or_image`, the space handling:

```erlang
parse_inline_link_or_image(Tokens, Refs, Type) ->
    case get_link_text(Tokens, []) of
        {Rest1, Text, close} ->
            % Check for optional space before paren (allowed for images)
            Rest2 = case Rest1 of
                [{{ws, _}, _}, {bra, _} | R] when Type =:= img -> 
                    [{bra, _} | R];  % Skip the space for images
                _ -> 
                    Rest1
            end,
            case Rest2 of
                [{bra, _} | Rest3] ->
                    % Direct link/image: [text](url)
                    case get_url_and_title(Rest3, []) of
                        {Rest4, Url, Title} ->
                            {Rest4, {Url, Title, Text}};
                        normal ->
                            normal
                    end;
                %% ... rest of clauses
```

## Fix 5: List Continuation (Complex - Requires Rethinking)

**Problem:** Lists don't absorb continuation lines

The original parser has this logic in `grab/4` and `grab2/6` which is extremely complex. Our simplified `grab_list_item` doesn't implement this.

**This is a major refactoring issue - the list parsing logic is incomplete.**

For now, note that this requires implementing the full `grab` logic from the original. This is beyond a quick fix.

## Fix 6: Hard Line Breaks

**Problem:** Two spaces or tab before newline should create `<br />` but don't

**Issue:** We're not detecting hard line breaks in the token stream

The original creates hard breaks during parsing by detecting whitespace before linefeeds. Our AST needs to do the same.

**In the lexer output, look for patterns like:**
- `[{{ws, sp}, "  "}, {{lf, _}, _}]` → hard break
- `[{{ws, tab}, "\t"}, {{lf, _}, _}]` → hard break

**We need to handle this in parse_emphasis_and_code:**

```erlang
%% Hard line breaks - two spaces or tab before LF
parse_emphasis_and_code([?SPACE, ?SPACE, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?SPACE, ?SPACE, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?TAB, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?TAB, ?CR, ?LF | T], Acc) ->
    LB = #line_break{type = hard},
    parse_emphasis_and_code(T, [LB | Acc]);

%% Regular line breaks (must come AFTER hard breaks)
parse_emphasis_and_code([?CR, ?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);
```

But wait - this won't work because we're processing strings, not tokens here. The issue is that tokens have already been converted to strings by the time we reach `parse_emphasis_and_code`.

**The real fix:** We need to check for hard breaks at the TOKEN level before converting to strings.

## Fix 7: Tab Character Rendering

**Problem:** Tabs should be converted to 4 spaces in output

**Current:** `xyz\tab:c` stays as tab
**Expected:** `xyz    ab:c` (4 spaces)

**Fix in parse_emphasis_and_code (already there but verify):**

```erlang
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},  % 4 spaces
    parse_emphasis_and_code(T, [TextNode | Acc]);
```

This is already in the code, so the issue must be that tabs in token strings aren't being processed. The problem is that `{string, "xyz\tab:c"}` is already a string with a tab character, and we're not expanding it.

**We need to process tabs during text collection or during string-to-inline conversion.**

## Fix 8: Blockquote Extra Space

**Problem:** `> blah\na` generates `<p>blah\n a</p>` instead of `<p>blah\na</p>`

The extra space before `a` suggests that when blockquotes are followed by normal text, we're adding padding incorrectly.

**Check the blockquote merging logic in parse_blocks:**

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

But the issue is probably in how normal lines after blockquotes are handled. When a blockquote is followed by `a` on the next line, it should be a separate paragraph, not part of the blockquote.

## Summary of Fixes by Priority

1. **URGENT: Fix collect_text_tokens crash** (none atom issue)
2. **HIGH: Fix backslash escaping** (keep backslash for non-special chars)
3. **HIGH: Fix trailing spaces in URLs** (strip whitespace from URLs)
4. **MEDIUM: Fix tab expansion** (tabs to 4 spaces)
5. **MEDIUM: Fix hard line breaks** (2 spaces or tab + newline)
6. **COMPLEX: Fix list continuation** (requires major rework)
7. **LOW: Fix image space before paren** (allowed for images)
8. **LOW: Fix blockquote spacing** (extra space issue)

## Testing After Each Fix

Apply fixes one at a time and run tests:

```bash
rebar3 eunit --module=erlmd_tests 2>&1 | grep -E "tests,|failures"
```

The crash fix (#1) should immediately reduce failures from 40 to ~35.
The backslash fix (#2) should reduce by another ~5.
