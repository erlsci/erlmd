# erlmd AST - Token-Level Escape Fixes

## Problem: Escaped Inline Markers Not Working

### Current Behavior
```erlang
erlmd:conv("[id]: /a/path\nSome text \\[id] there\na")
% Returns: "<p>Some text <a href=\"/a/path\">id</a> there\na</p>"
% Expected: "<p>Some text [id] there\na</p>"
```

### Root Cause

The issue is that **tokenization happens before AST building**. When the lexer sees:
```
Some text \[id] there
```

It produces tokens:
```erlang
[{string, "Some text "}, 
 {{punc, bslash}, "\\"}, 
 {{inline, open}, "["}, 
 {string, "id"}, 
 {{inline, close}, "]"}, 
 {string, " there"}]
```

Our current AST code in `parse_inline_elements` sees these as separate tokens and tries to parse them, but it doesn't have logic to look back at the previous token to check for an escape.

### Solution: Handle Escaped Tokens in parse_inline_elements

The fix must happen where we process inline tokens, **before** we try to parse links/images.

## Fix 1: Add Escaped Inline Open Handler

In `src/erlmd_ast.erl`, in the `parse_inline_elements/3` function, we need to handle the escaped bracket sequence BEFORE the normal link handler.

**Current Code (around line 334-348):**
```erlang
%% Escaped inline open
parse_inline_elements([{{punc, bslash}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "["},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

%% Links: [text](url "title") - text can contain images!
parse_inline_elements([{{inline, open}, _} | T], Refs, Acc) ->
    case get_inline_content(T, Refs, url) of
        {Rest, {Url, Title, LinkText}} ->
            % Parse the link text as inline content (may contain images, emphasis, etc.)
            TextTokens = retokenize_string(LinkText),
            TextInline = parse_inline_elements(TextTokens, Refs, []),
            Link = #link{text = TextInline, url = Url, title = Title},
            parse_inline_elements(Rest, Refs, [Link | Acc]);
        {Rest, Text} ->
            TextNode = #text{content = "[" ++ Text},
            parse_inline_elements(Rest, Refs, [TextNode | Acc])
    end;
```

**Issue:** The escaped inline open clause is correct, but when we're processing text tokens that ALREADY INCLUDE the backslash and bracket as part of a string, we never see this pattern.

## Fix 2: The REAL Problem - Text Token Processing

The actual issue is in how we handle `{_, Orig}` tokens. When we have:

```erlang
[{string, "Some text "}, {{punc, bslash}, "\\"}, {{inline, open}, "["}, ...]
```

Our current code processes the string "Some text " as text, THEN sees the backslash and bracket as separate tokens. This is correct behavior!

The problem is that after we correctly create the text node for "[", we continue processing and the NEXT time around we see `{{inline, close}, "]"}` which closes a link we never opened.

### The Missing Piece

We need to handle the **close bracket** more carefully. When we see a close bracket without a matching open, we should treat it as literal text:

**Add to parse_inline_elements (around line 380):**

```erlang
%% Close brackets without matching open - treat as literal text
parse_inline_elements([{{inline, close}, _} | T], Refs, Acc) ->
    % If we reach here, there was no matching open bracket, so treat as literal
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc]);
```

But this would break valid markdown like `[id]` (implicit reference links).

## Fix 3: The ACTUAL Root Cause

Looking more carefully at the token flow, I realize the real issue is in `parse_text_formatting`. When we call:

```erlang
parse_inline_elements([{_, Orig} | T], Refs, Acc) ->
    % Accumulate text until we hit a special token
    {Rest, TextStr} = collect_text_tokens([{string, Orig} | T]),
    % Parse the text for inline formatting
    Inlines = parse_text_formatting(TextStr, Refs),
    parse_inline_elements(Rest, Refs, lists:reverse(Inlines, Acc)).
```

We convert `{_, Orig}` to `{string, Orig}` and pass it to `collect_text_tokens`. But this means we're treating ALL token types as strings, including backslash tokens!

### The Real Fix: Don't Collect Backslash Tokens as Text

**In `parse_inline_elements`, we need to handle backslash BEFORE the generic token handler:**

```erlang
%% Regular text and emphasis/strong/code - process as text
parse_inline_elements([{_, Orig} | T], Refs, Acc) ->
```

Should become:

```erlang
%% Backslash escapes for inline markers - handle BEFORE text collection
parse_inline_elements([{{punc, bslash}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "["},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

parse_inline_elements([{{punc, bslash}, _}, {{inline, close}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

%% Regular text - but DON'T include backslash or inline markers in text collection
parse_inline_elements([{Type, Orig} | T], Refs, Acc) when Type =/= punc, Type =/= inline ->
    % Only collect actual text/string tokens
    {Rest, TextStr} = collect_text_tokens([{string, Orig} | T]),
    Inlines = parse_text_formatting(TextStr, Refs),
    parse_inline_elements(Rest, Refs, lists:reverse(Inlines, Acc));

%% Unhandled punctuation - treat as text
parse_inline_elements([{{punc, _}, Orig} | T], Refs, Acc) ->
    TextNode = #text{content = Orig},
    parse_inline_elements(T, Refs, [TextNode | Acc]).
```

Wait, but we already have the escaped inline open handler at the top. Let me check the original code flow again...

## Fix 4: The ACTUAL Actual Problem

I think the issue is simpler. We have this:

```erlang
%% Escaped inline open
parse_inline_elements([{{punc, bslash}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "["},
    parse_inline_elements(T, Refs, [TextNode | Acc]);
```

This SHOULD work. But after creating the text node with "[", we then continue processing `T` which starts with `{string, "id"}`. Eventually we hit `{{inline, close}, "]"}` and this tries to close a link.

The problem is that `{{inline, close}, "]"}` needs to be handled differently when there's no matching open.

## Fix 5: Track Bracket Depth

The cleanest solution is to **track bracket depth** in the accumulator or add a depth parameter:

```erlang
parse_inline_elements(Tokens, Refs) ->
    parse_inline_elements(Tokens, Refs, [], 0).

parse_inline_elements([], _Refs, Acc, _Depth) ->
    lists:reverse(Acc);

%% Escaped brackets - don't affect depth
parse_inline_elements([{{punc, bslash}, _}, {{inline, open}, _} | T], Refs, Acc, Depth) ->
    TextNode = #text{content = "["},
    parse_inline_elements(T, Refs, [TextNode | Acc], Depth);

parse_inline_elements([{{punc, bslash}, _}, {{inline, close}, _} | T], Refs, Acc, Depth) ->
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc], Depth);

%% Opening bracket - increment depth
parse_inline_elements([{{inline, open}, _} | T], Refs, Acc, Depth) ->
    case get_inline_content(T, Refs, url) of
        {Rest, {Url, Title, LinkText}} ->
            TextTokens = retokenize_string(LinkText),
            TextInline = parse_inline_elements(TextTokens, Refs, []),
            Link = #link{text = TextInline, url = Url, title = Title},
            parse_inline_elements(Rest, Refs, [Link | Acc], Depth);
        {Rest, Text} ->
            TextNode = #text{content = "[" ++ Text},
            parse_inline_elements(Rest, Refs, [TextNode | Acc], Depth)
    end;

%% Closing bracket without matching open - literal text
parse_inline_elements([{{inline, close}, _} | T], Refs, Acc, Depth) when Depth =:= 0 ->
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc], Depth);
```

Actually, this is getting too complex. Let me think about this differently...

## The Simple Fix That Should Work

Looking at the original code in `erlmd.erl`, the handling is done during HTML generation in `m_str1`:

```erlang
m_str1([{{punc, bslash}, _}, {{inline, open}, O} | T], R, A) ->
    m_str1(T, R, [O | A]);
```

The original processes tokens sequentially and when it sees backslash followed by bracket, it just outputs the bracket and skips both tokens.

Our AST code SHOULD do the same thing, and we already do! The clause is there. So why isn't it working?

## Debug: Let's Trace Token Flow

For input: `Some text \[id] there`

After tokenization we have:
```erlang
[{string, "Some text "}, {{punc, bslash}, "\\"}, {{inline, open}, "["}, 
 {string, "id"}, {{inline, close}, "]"}, {string, " there"}]
```

Parse flow:
1. `{string, "Some text "}` → goes to text processing → creates #text{content="Some text "}
2. `{{punc, bslash}, "\\"}` → Hmm, this should match our escaped inline open pattern...

OH! The pattern is:
```erlang
parse_inline_elements([{{punc, bslash}, _}, {{inline, open}, _} | T], ...)
```

But after step 1, we're calling `parse_inline_elements(Rest, ...)` where `Rest` starts with `{{punc, bslash}, "\\"}`.

So step 2: We DO match the escaped bracket pattern! Good.
   - Create #text{content="["}
   - Continue with T = `[{string, "id"}, {{inline, close}, "]"}, {string, " there"}]`

3. `{string, "id"}` → text processing → #text{content="id"}
4. `{{inline, close}, "]"}` → ??? What matches this?

AH HA! We don't have a handler for standalone `{{inline, close}, "]"}`! It must be falling through to the generic `[{_, Orig} | T]` handler which treats it as text, but then something goes wrong.

## The Actual Fix

Add a handler for unmatched close brackets:

```erlang
%% Escaped inline close (for completeness, though less common)
parse_inline_elements([{{punc, bslash}, _}, {{inline, close}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

%% Images: ![alt](url "title")
parse_inline_elements([{{punc, bang}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    ...

%% Escaped inline open (THIS IS ALREADY THERE)
parse_inline_elements([{{punc, bslash}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "["},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

%% Links: [text](url "title")
parse_inline_elements([{{inline, open}, _} | T], Refs, Acc) ->
    ...

%% ADD THIS: Unmatched close bracket - treat as literal text
parse_inline_elements([{{inline, close}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

%% Regular text processing...
parse_inline_elements([{_, Orig} | T], Refs, Acc) ->
    ...
```

This should handle the escaped bracket case correctly by:
1. Matching `\[` and outputting literal `[`
2. Processing `id` as text
3. Matching standalone `]` and outputting literal `]`
4. Processing remaining text

## Implementation

**Add this clause** to `src/erlmd_ast.erl` in `parse_inline_elements/3`, **after** the Links clause and **before** the Email/URL clauses:

```erlang
%% Unmatched close bracket - treat as literal text
%% This handles cases like: \[id] where the open bracket was escaped
parse_inline_elements([{{inline, close}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc]);
```

This single fix should resolve the `\[id]` issue.

## Testing

After adding this clause, test:

```erlang
erlmd:conv("[id]: /a/path\nSome text \\[id] there\na").
% Expected: "<p>Some text [id] there\na</p>"

erlmd:conv("Some text [id] there\na\n[id]: /a/path").  
% Expected: "<p>Some text <a href=\"/a/path\">id</a> there\na</p>"
```

## Why This Works

1. Lexer produces separate tokens for `\`, `[`, text, `]`
2. Parser sees `\[` pattern, outputs `[` as text
3. Parser sees `id`, outputs it as text
4. Parser sees standalone `]`, outputs it as text (NEW HANDLER)
5. Final result: `[id]` as literal text

Without this handler, the `]` token would either:
- Fall through to generic handler and cause issues
- Try to close a non-existent link
- Get processed incorrectly

This is a minimal, targeted fix that should resolve the token-level escape issue.
