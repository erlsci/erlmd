# erlmd AST Refactoring Implementation Plan

## Overview

This document provides a comprehensive, AI-executable plan to refactor the erlmd library by introducing an Abstract Syntax Tree (AST) abstraction layer between Markdown parsing and HTML output generation. The refactoring will maintain 100% backward compatibility while enabling future extensibility.

## Goals

1. Separate parsing logic from HTML rendering logic
2. Introduce an intermediate AST representation
3. Maintain all existing functionality and test compatibility
4. Enable future alternative renderers (JSON, Markdown round-trip, etc.)
5. Support literate programming use cases

## Architecture

```
Current:  Input → Lexer → Type Lines → Parser (generates HTML) → HTML
Proposed: Input → Lexer → Type Lines → Parser (generates AST) → Renderer → HTML
```

## Implementation Phases

### Phase 1: Define AST Types

**File:** `include/types.hrl`

Create a new header file defining all AST node types.

**Tasks:**

1. Create the `include/` directory if it doesn't exist
2. Create `include/types.hrl` with the following type definitions:

```erlang
%%% AST Type Definitions for erlmd
%%% This file defines the Abstract Syntax Tree structure for parsed Markdown documents

%% Top-level document structure
-record(document, {
    blocks :: [block()]
}).

%% Block-level elements
-type block() :: paragraph()
               | header()
               | blockquote()
               | code_block()
               | horizontal_rule()
               | list()
               | html_block()
               | blank_line().

-record(paragraph, {
    content :: [inline()]
}).

-record(header, {
    level :: 1..6,
    content :: [inline()],
    type :: atx | setext  % track header style for potential round-tripping
}).

-record(blockquote, {
    blocks :: [block()]  % blockquotes can contain multiple block elements
}).

-record(code_block, {
    content :: string(),
    language :: string() | undefined  % for future extension (fenced code blocks)
}).

-record(horizontal_rule, {}).

-record(blank_line, {}).

%% List structures
-record(list, {
    type :: ul | ol,
    items :: [list_item()],
    tight :: boolean()  % tight vs loose list spacing
}).

-record(list_item, {
    content :: [block()]  % list items can contain multiple block elements
}).

%% HTML block (pass-through)
-record(html_block, {
    tag :: string(),
    content :: string(),
    type :: open | close | self_closing
}).

%% Inline elements
-type inline() :: text()
                | emphasis()
                | strong()
                | code_span()
                | link()
                | image()
                | line_break()
                | html_inline().

-record(text, {
    content :: string()
}).

-record(emphasis, {
    content :: [inline()],
    delimiter :: $* | $_  % track which delimiter was used
}).

-record(strong, {
    content :: [inline()],
    delimiter :: $* | $_
}).

-record(code_span, {
    content :: string(),
    delimiter :: single | double  % ` vs ``
}).

-record(link, {
    text :: [inline()],
    url :: string(),
    title :: string()
}).

-record(image, {
    alt_text :: [inline()],
    url :: string(),
    title :: string()
}).

-record(line_break, {
    type :: soft | hard  % regular newline vs two-space hard break
}).

-record(html_inline, {
    content :: string()
}).

%% Export types for use in other modules
-export_type([
    document/0,
    block/0,
    inline/0,
    paragraph/0,
    header/0,
    blockquote/0,
    code_block/0,
    horizontal_rule/0,
    list/0,
    list_item/0,
    html_block/0,
    blank_line/0,
    text/0,
    emphasis/0,
    strong/0,
    code_span/0,
    link/0,
    image/0,
    line_break/0,
    html_inline/0
]).
```

**Verification:**
- File compiles without errors when included in a test module
- All record definitions are syntactically correct

---

### Phase 2: Create AST Builder Module

**File:** `src/erlmd_ast.erl`

Create a new module responsible for building the AST from typed lines.

**Tasks:**

1. Create module skeleton with includes and exports:

```erlang
-module(erlmd_ast).

-include_lib("erlmd/include/types.hrl").

-export([
    build/2,           % build(TypedLines, Refs) -> #document{}
    build_inline/2     % build_inline(Tokens, Refs) -> [inline()]
]).

%% Internal exports for testing
-export([
    parse_block/3,     % for unit testing individual block parsing
    parse_inline/2     % for unit testing inline parsing
]).
```

2. Implement the main `build/2` function that mirrors the current `parse/2` structure:

```erlang
%% @doc Build an AST document from typed lines and references
-spec build([typed_line()], refs()) -> #document{}.
build(TypedLines, Refs) ->
    Blocks = parse_blocks(TypedLines, Refs, []),
    #document{blocks = lists:reverse(Blocks)}.

%% Internal type for typed lines (as produced by type_lines/1)
-type typed_line() :: {atom(), term()} | {{atom(), term()}, term()}.
-type refs() :: [{string(), {string(), string()}}].
```

3. Implement `parse_blocks/3` - this is the core function that will replace `p1/4`:

```erlang
%% @doc Parse blocks recursively, building AST nodes
-spec parse_blocks([typed_line()], refs(), [block()]) -> [block()].

%% Terminal clause
parse_blocks([], _Refs, Acc) ->
    Acc;

%% Tag handling (currently pass-through as HTML blocks)
parse_blocks([{tag, Tag} | T], Refs, Acc) ->
    HTMLBlock = parse_tag(Tag, Refs),
    parse_blocks(T, Refs, [HTMLBlock | Acc]);

%% Block tag handling
parse_blocks([{blocktag, [{{{tag, Type}, TagName}, Content}]} | T], Refs, Acc) ->
    HTMLBlock = #html_block{
        tag = TagName,
        content = Content,
        type = Type
    },
    parse_blocks(T, Refs, [HTMLBlock | Acc]);

%% Blank lines and linefeeds - create blank_line nodes
parse_blocks([{Type, _} | T], Refs, Acc) 
  when Type =:= blank orelse Type =:= linefeed ->
    {Rest, _} = grab_empties(T),
    parse_blocks(Rest, Refs, [#blank_line{} | Acc]);

%% Normal line followed by setext_h1
parse_blocks([{normal, P}, {setext_h1, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 1, content = Content, type = setext},
    parse_blocks(T, Refs, [Header | Acc]);

%% Blockquote followed by setext_h1
parse_blocks([{blockquote, P}, {setext_h1, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 1, content = Content, type = setext},
    parse_blocks(T, Refs, [Header | Acc]);

%% Codeblock followed by setext_h1
parse_blocks([{{codeblock, P}, _}, {setext_h1, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 1, content = Content, type = setext},
    parse_blocks(T, Refs, [Header | Acc]);

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

%% Standalone setext_h1 (becomes normal)
parse_blocks([{setext_h1, P} | T], Refs, Acc) ->
    parse_blocks([{normal, P} | T], Refs, Acc);

%% Blockquotes that consume normals
parse_blocks([{blockquote, P1}, {blockquote, [_ | P2]} | T], Refs, Acc) ->
    Merged = merge_with_br(P1, P2),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);

parse_blocks([{blockquote, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = merge_tokens(P1, P2),
    parse_blocks([{blockquote, Merged} | T], Refs, Acc);

%% Single blockquote
parse_blocks([{blockquote, P} | T], Refs, Acc) ->
    [{{md, gt}, _} | T1] = P,
    Content = build_inline(T1, Refs),
    % Blockquotes contain paragraphs
    BQ = #blockquote{blocks = [#paragraph{content = Content}]},
    parse_blocks(T, Refs, [BQ | Acc]);

%% Normal paragraph (standalone)
parse_blocks([{normal, P} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Para = #paragraph{content = Content},
    parse_blocks(T, Refs, [Para | Acc]);

%% ATX headers (h1-h6)
parse_blocks([{{h1, P}, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 1, content = Content, type = atx},
    parse_blocks(T, Refs, [Header | Acc]);

parse_blocks([{{h2, P}, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 2, content = Content, type = atx},
    parse_blocks(T, Refs, [Header | Acc]);

parse_blocks([{{h3, P}, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 3, content = Content, type = atx},
    parse_blocks(T, Refs, [Header | Acc]);

parse_blocks([{{h4, P}, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 4, content = Content, type = atx},
    parse_blocks(T, Refs, [Header | Acc]);

parse_blocks([{{h5, P}, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 5, content = Content, type = atx},
    parse_blocks(T, Refs, [Header | Acc]);

parse_blocks([{{h6, P}, _} | T], Refs, Acc) ->
    Content = build_inline(snip(P), Refs),
    Header = #header{level = 6, content = Content, type = atx},
    parse_blocks(T, Refs, [Header | Acc]);

%% Unordered lists
parse_blocks([{{ul, _P}, _} | _T] = List, Refs, Acc) ->
    {Rest, ListNode} = parse_list(ul, List, Refs),
    parse_blocks(Rest, Refs, [ListNode | Acc]);

%% Ordered lists
parse_blocks([{{ol, _P}, _} | _T] = List, Refs, Acc) ->
    {Rest, ListNode} = parse_list(ol, List, Refs),
    parse_blocks(Rest, Refs, [ListNode | Acc]);

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

%% Horizontal rules
parse_blocks([{hr, _} | T], Refs, Acc) ->
    parse_blocks(T, Refs, [#horizontal_rule{} | Acc]);

parse_blocks([{h2_or_hr, P1}, {normal, P2} | T], Refs, Acc) ->
    Merged = lists:flatten([P1 | P2]),
    parse_blocks([{normal, Merged} | T], Refs, Acc);

parse_blocks([{h2_or_hr, _} | T], Refs, Acc) ->
    parse_blocks(T, Refs, [#horizontal_rule{} | Acc]);

%% Inline refs (skip)
parse_blocks([{inlineref, _P} | T], Refs, Acc) ->
    parse_blocks(T, Refs, Acc).
```

4. Implement list parsing:

```erlang
%% @doc Parse a list (ul or ol) and return the AST node
-spec parse_list(ul | ol, [typed_line()], refs()) -> {[typed_line()], #list{}}.
parse_list(Type, Lines, Refs) ->
    {Rest, Items, Tight} = collect_list_items(Type, Lines, Refs, [], false),
    List = #list{type = Type, items = Items, tight = Tight},
    {Rest, List}.

%% @doc Collect list items recursively
collect_list_items(_Type, [], _Refs, Acc, Tight) ->
    {[], lists:reverse(Acc), Tight};

collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    {Rest, ItemContent, NewTight} = grab_list_item(T, Refs, [], Tight),
    
    % Build the item's blocks
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs),
    Item = #list_item{content = ItemBlocks},
    
    % Determine if next item triggers loose mode
    NextTight = case T of
        [] -> false;
        [H2 | _T2] -> 
            case H2 of
                {linefeed, _} -> false;  % loose list
                _ -> NewTight
            end
    end,
    
    collect_list_items(Type, Rest, Refs, [Item | Acc], NextTight);

collect_list_items(_Type, List, _Refs, Acc, Tight) ->
    {List, lists:reverse(Acc), Tight}.

%% @doc Parse the content of a single list item
parse_list_item_content(P, AdditionalContent, Refs) ->
    % List items contain at least one paragraph
    Content = build_inline(P, Refs),
    Para = #paragraph{content = Content},
    
    % Additional content goes here if present
    case AdditionalContent of
        [] -> [Para];
        _ -> [Para]  % TODO: handle multi-block list items properly
    end.
```

5. Implement inline parsing - this is the most complex part:

```erlang
%% @doc Build inline elements from tokens
-spec build_inline([token()], refs()) -> [inline()].
build_inline(Tokens, Refs) ->
    parse_inline_elements(Tokens, Refs, []).

%% @doc Parse inline elements recursively
-spec parse_inline_elements([token()], refs(), [inline()]) -> [inline()].
parse_inline_elements([], _Refs, Acc) ->
    lists:reverse(Acc);

%% Images: ![alt](url "title")
parse_inline_elements([{{punc, bang}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    case get_inline_content(T, Refs, [], img) of
        {Rest, {Url, Title, AltText}} ->
            AltInline = parse_text(AltText),
            Image = #image{alt_text = AltInline, url = Url, title = Title},
            parse_inline_elements(Rest, Refs, [Image | Acc]);
        {Rest, Text} ->
            % Not a valid image, treat as text
            TextNode = #text{content = "![" ++ Text},
            parse_inline_elements(Rest, Refs, [TextNode | Acc])
    end;

%% Escaped inline open
parse_inline_elements([{{punc, bslash}, _}, {{inline, open}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "["},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

%% Links: [text](url "title")
parse_inline_elements([{{inline, open}, _} | T], Refs, Acc) ->
    case get_inline_content(T, Refs, [], url) of
        {Rest, {Url, Title, LinkText}} ->
            TextInline = parse_text(LinkText),
            Link = #link{text = TextInline, url = Url, title = Title},
            parse_inline_elements(Rest, Refs, [Link | Acc]);
        {Rest, Text} ->
            TextNode = #text{content = "[" ++ Text},
            parse_inline_elements(Rest, Refs, [TextNode | Acc])
    end;

%% Email addresses
parse_inline_elements([{email, Addie} | T], Refs, Acc) ->
    Link = #link{
        text = [#text{content = Addie}],
        url = "mailto:" ++ Addie,
        title = ""
    },
    parse_inline_elements(T, Refs, [Link | Acc]);

%% URLs
parse_inline_elements([{url, Url} | T], Refs, Acc) ->
    Link = #link{
        text = [#text{content = Url}],
        url = Url,
        title = ""
    },
    parse_inline_elements(T, Refs, [Link | Acc]);

%% HTML tags (pass through)
parse_inline_elements([{tags, _} = Tag | T], Refs, Acc) ->
    HTMLInline = #html_inline{content = tag_to_string(Tag)},
    parse_inline_elements(T, Refs, [HTMLInline | Acc]);

parse_inline_elements([{{{tag, Type}, Tag}, Content} | T], Refs, Acc) ->
    HTMLStr = format_html_tag(Type, Tag, Content),
    HTMLInline = #html_inline{content = HTMLStr},
    parse_inline_elements(T, Refs, [HTMLInline | Acc]);

%% Regular text and emphasis/strong/code
parse_inline_elements([{_, Orig} | T], Refs, Acc) ->
    % Accumulate text and process emphasis/strong/code
    {Rest, TextWithFormatting} = collect_text_and_format(T, Refs, [Orig]),
    Inlines = parse_formatted_text(TextWithFormatting, Refs),
    parse_inline_elements(Rest, Refs, lists:reverse(Inlines) ++ Acc).

%% @doc Parse text content that may contain emphasis, strong, code, etc.
%% This handles the complex inline formatting logic from htmlchars1/2
-spec parse_formatted_text(string(), refs()) -> [inline()].
parse_formatted_text(Text, _Refs) ->
    % This is where we convert the emphasis/strong/code parsing
    % from the current htmlchars1/2 function
    parse_emphasis_and_code(Text, []).

%% @doc Parse emphasis, strong, and code spans from text
parse_emphasis_and_code([], Acc) ->
    lists:reverse(Acc);

%% Escaped emphasis markers
parse_emphasis_and_code([$\\, $*, $*, $* | T], Acc) ->
    TextNode = #text{content = "***"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Super-strong: ***text***
parse_emphasis_and_code([$*, $*, $* | T], Acc) ->
    {Rest, Content} = collect_until(T, [$*, $*, $*], []),
    % Strong + emphasis
    EmContent = [#text{content = Content}],
    Em = #emphasis{content = EmContent, delimiter = $*},
    Strong = #strong{content = [Em], delimiter = $*},
    parse_emphasis_and_code(Rest, [Strong | Acc]);

%% Escaped strong
parse_emphasis_and_code([$\\, $*, $* | T], Acc) ->
    TextNode = #text{content = "**"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Strong: **text**
parse_emphasis_and_code([$*, $* | T], Acc) ->
    {Rest, Content} = collect_until(T, [$*, $*], []),
    Strong = #strong{content = [#text{content = Content}], delimiter = $*},
    parse_emphasis_and_code(Rest, [Strong | Acc]);

%% Escaped emphasis
parse_emphasis_and_code([$\\, $* | T], Acc) ->
    TextNode = #text{content = "*"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Emphasis: *text*
parse_emphasis_and_code([$* | T], Acc) ->
    {Rest, Content} = collect_until(T, [$*], []),
    Em = #emphasis{content = [#text{content = Content}], delimiter = $*},
    parse_emphasis_and_code(Rest, [Em | Acc]);

%% Underscore variants (same as above but with $_)
parse_emphasis_and_code([$\\, $_, $_, $_ | T], Acc) ->
    TextNode = #text{content = "___"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$_, $_, $_ | T], Acc) ->
    {Rest, Content} = collect_until(T, [$_, $_, $_], []),
    EmContent = [#text{content = Content}],
    Em = #emphasis{content = EmContent, delimiter = $_},
    Strong = #strong{content = [Em], delimiter = $_},
    parse_emphasis_and_code(Rest, [Strong | Acc]);

parse_emphasis_and_code([$\\, $_, $_ | T], Acc) ->
    TextNode = #text{content = "__"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$_, $_ | T], Acc) ->
    {Rest, Content} = collect_until(T, [$_, $_], []),
    Strong = #strong{content = [#text{content = Content}], delimiter = $_},
    parse_emphasis_and_code(Rest, [Strong | Acc]);

parse_emphasis_and_code([$\\, $_ | T], Acc) ->
    TextNode = #text{content = "_"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$_ | T], Acc) ->
    {Rest, Content} = collect_until(T, [$_], []),
    Em = #emphasis{content = [#text{content = Content}], delimiter = $_},
    parse_emphasis_and_code(Rest, [Em | Acc]);

%% Code spans: `code` and ``code``
parse_emphasis_and_code([$\\, $` | T], Acc) ->
    TextNode = #text{content = "`"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$`, $` | T], Acc) ->
    {Rest, Content} = collect_until(T, [$`, $`], []),
    Code = #code_span{content = Content, delimiter = double},
    parse_emphasis_and_code(Rest, [Code | Acc]);

parse_emphasis_and_code([$` | T], Acc) ->
    {Rest, Content} = collect_until(T, [$`], []),
    Code = #code_span{content = Content, delimiter = single},
    parse_emphasis_and_code(Rest, [Code | Acc]);

%% Line breaks
parse_emphasis_and_code([?CR, ?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?CR | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

%% HTML entities
parse_emphasis_and_code([?COPY | T], Acc) ->
    TextNode = #text{content = "©"},  % Store actual character
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([?AMP | T], Acc) ->
    TextNode = #text{content = "&"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Special characters that need tracking
parse_emphasis_and_code([$& | T], Acc) ->
    TextNode = #text{content = "&"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$< | T], Acc) ->
    TextNode = #text{content = "<"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([?NBSP | T], Acc) ->
    TextNode = #text{content = "\u00A0"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Regular character
parse_emphasis_and_code([H | T], Acc) ->
    % Accumulate consecutive regular characters
    {Rest, Text} = collect_regular_text([H | T], []),
    TextNode = #text{content = Text},
    parse_emphasis_and_code(Rest, [TextNode | Acc]).

%% @doc Collect characters until delimiter is found
collect_until([], _Delim, Acc) ->
    {[], lists:reverse(Acc)};
collect_until(Text, [D], Acc) when is_integer(D) ->
    case lists:prefix([D], Text) of
        true -> {lists:nthtail(1, Text), lists:reverse(Acc)};
        false -> 
            [H | T] = Text,
            collect_until(T, [D], [H | Acc])
    end;
collect_until(Text, Delim, Acc) when is_list(Delim) ->
    case lists:prefix(Delim, Text) of
        true -> {lists:nthtail(length(Delim), Text), lists:reverse(Acc)};
        false -> 
            [H | T] = Text,
            collect_until(T, Delim, [H | Acc])
    end.

%% @doc Collect regular text characters (not special markdown chars)
collect_regular_text([], Acc) ->
    {[], lists:reverse(Acc)};
collect_regular_text([H | T], Acc) when H =:= $*; H =:= $_; H =:= $`; 
                                        H =:= $\\; H =:= ?LF; H =:= ?CR ->
    {[H | T], lists:reverse(Acc)};
collect_regular_text([H | T], Acc) ->
    collect_regular_text(T, [H | Acc]).
```

6. Import necessary helper functions from erlmd.erl (or reimplement):

```erlang
%% Helper functions (copied/adapted from erlmd.erl)

snip(List) -> 
    List2 = lists:reverse(List),
    case List2 of
        [{{lf, _}, _} | T] -> lists:reverse(T);
        _ -> List
    end.

grab_empties(List) -> grab_empties1(List, []).

grab_empties1([{linefeed, _} | T], E) -> 
    grab_empties1(T, [{{lf,lf}, "\n"} | E]);
grab_empties1([{blank, _} | T], E) -> 
    grab_empties1(T, [{{lf,lf}, "\n"} | E]);
grab_empties1(List, E) -> 
    {List, E}.

merge_tokens(P1, P2) ->
    lists:flatten([P1, {string, " "} | P2]).

merge_with_br(P1, P2) ->
    NewP1 = make_br(P1),
    lists:flatten([NewP1, {tags, " <br />\n"} | P2]).

make_br(List) -> 
    case lists:reverse(List) of
        [{{lf, _}, _}, {{ws, comp}, _} | T] -> 
            lists:reverse([{tags, " <br />\n"} | T]);
        [{{lf, _}, _}, {{ws, tab}, _} | T] -> 
            lists:reverse([{tags, " <br />\n"} | T]);
        _ -> 
            List
    end.

make_plain_str(List) -> 
    make_plain_str(List, []).

make_plain_str([], Acc) -> 
    lists:flatten(lists:reverse(Acc));
make_plain_str([{{ws, none}, none} | T], Acc) -> 
    make_plain_str(T, [" " | Acc]);
make_plain_str([{_, Str} | T], Acc) -> 
    make_plain_str(T, [Str | Acc]).

parse_text(String) when is_list(String) ->
    [#text{content = String}].

tag_to_string({tags, Tag}) -> Tag.

format_html_tag(Type, Tag, Content) ->
    case Type of
        open -> "&lt;" ++ Tag ++ "&gt;";
        close -> "&lt;/" ++ Tag ++ "&gt;";
        self_closing -> "&lt;" ++ Tag ++ " /&gt;"
    end.

collect_text_and_format(Tokens, _Refs, Acc) ->
    % Simplified - just collect until we run out
    {[], lists:flatten(lists:reverse(Acc))}.

get_inline_content(T, Refs, Acc, Type) ->
    % This mirrors get_inline from erlmd.erl but returns structured data
    % Implementation would be similar to the original but return tuples
    case parse_inline_link_or_image(T, Type) of
        {Rest, {Url, Title, Text}} -> {Rest, {Url, Title, Text}};
        normal -> {T, lists:flatten(Acc)}
    end.

parse_inline_link_or_image(Tokens, _Type) ->
    % Stub - implement the full logic from get_inline/4
    normal.

grab_list_item(T, _Refs, Acc, Tight) ->
    % Stub - implement list item collection
    {T, lists:reverse(Acc), Tight}.

parse_tag(Tag, _Refs) ->
    % Convert tag tokens to HTML block
    #html_block{
        tag = "unknown",
        content = make_plain_str(Tag),
        type = self_closing
    }.
```

**Verification:**
- Module compiles successfully
- Can build AST from simple typed lines
- All helper functions are present

---

### Phase 3: Create HTML Renderer Module

**File:** `src/erlmd_html.erl`

Create a module that renders AST to HTML.

**Tasks:**

1. Create module skeleton:

```erlang
-module(erlmd_html).

-include_lib("erlmd/include/types.hrl").

-export([
    render/1,           % render(#document{}) -> string()
    render_block/1,     % render_block(block()) -> string()
    render_inline/1     % render_inline(inline()) -> string()
]).
```

2. Implement document rendering:

```erlang
%% @doc Render a document to HTML
-spec render(#document{}) -> string().
render(#document{blocks = Blocks}) ->
    HTML = lists:map(fun render_block/1, Blocks),
    string:strip(lists:flatten(HTML), both, $\n).
```

3. Implement block rendering:

```erlang
%% @doc Render a single block element
-spec render_block(block()) -> string().
render_block(#paragraph{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    "<p>" ++ lists:flatten(InlineHTML) ++ "</p>\n";

render_block(#header{level = Level, content = Content}) ->
    Tag = "h" ++ integer_to_list(Level),
    InlineHTML = lists:map(fun render_inline/1, Content),
    ContentStr = string:strip(lists:flatten(InlineHTML), right),
    "<" ++ Tag ++ ">" ++ ContentStr ++ "</" ++ Tag ++ ">\n\n";

render_block(#blockquote{blocks = Blocks}) ->
    BlockHTML = lists:map(fun render_block/1, Blocks),
    "\n<blockquote>\n  " ++ lists:flatten(BlockHTML) ++ "</blockquote>";

render_block(#code_block{content = Content}) ->
    Encoded = htmlencode(Content),
    "<pre><code>" ++ Encoded ++ "\n</code></pre>\n\n";

render_block(#horizontal_rule{}) ->
    "<hr />";

render_block(#list{type = Type, items = Items, tight = Tight}) ->
    Tag = case Type of
        ul -> "ul";
        ol -> "ol"
    end,
    ItemsHTML = lists:map(fun(Item) -> render_list_item(Item, Tight) end, Items),
    "<" ++ Tag ++ ">\n" ++ lists:flatten(ItemsHTML) ++ "</" ++ Tag ++ ">\n";

render_block(#html_block{content = Content}) ->
    Content;

render_block(#blank_line{}) ->
    "\n".

%% @doc Render a list item
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

%% @doc Strip <p> tags from paragraph for tight lists
strip_para(#paragraph{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    Ret = lists:flatten(InlineHTML),
    string:strip(Ret, right, $\n) ++ "\n".
```

4. Implement inline rendering:

```erlang
%% @doc Render a single inline element
-spec render_inline(inline()) -> string().
render_inline(#text{content = Content}) ->
    htmlencode_text(Content);

render_inline(#emphasis{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    "<em>" ++ lists:flatten(InlineHTML) ++ "</em>";

render_inline(#strong{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    "<strong>" ++ lists:flatten(InlineHTML) ++ "</strong>";

render_inline(#code_span{content = Content, delimiter = Delim}) ->
    Encoded = htmlencode(Content),
    case Delim of
        single -> "<code>" ++ Encoded ++ "</code>";
        double -> "<pre><code>" ++ Encoded ++ "</code></pre>"
    end;

render_inline(#link{text = Text, url = Url, title = Title}) ->
    TextHTML = lists:map(fun render_inline/1, Text),
    TitleAttr = case Title of
        "" -> "";
        _ -> " title=\"" ++ Title ++ "\""
    end,
    "<a href=\"" ++ Url ++ "\"" ++ TitleAttr ++ ">" ++ 
        lists:flatten(TextHTML) ++ "</a>";

render_inline(#image{alt_text = AltText, url = Url, title = Title}) ->
    AltHTML = lists:map(fun render_inline/1, AltText),
    "<img src=\"" ++ Url ++ "\""
        ++ " alt=\"" ++ lists:flatten(AltHTML) ++ "\""
        ++ " title=\"" ++ Title ++ "\""
        ++ " />";

render_inline(#line_break{type = Type}) ->
    case Type of
        soft -> "\n";
        hard -> " <br />\n"
    end;

render_inline(#html_inline{content = Content}) ->
    Content.
```

5. Implement HTML encoding:

```erlang
%% @doc Encode HTML special characters
htmlencode(List) ->
    htmlencode(List, []).

htmlencode([], Acc) ->
    lists:flatten(lists:reverse(Acc));
htmlencode([$& | Rest], Acc) -> 
    htmlencode(Rest, ["&amp;" | Acc]);
htmlencode([$< | Rest], Acc) -> 
    htmlencode(Rest, ["&lt;" | Acc]);
htmlencode([$> | Rest], Acc) -> 
    htmlencode(Rest, ["&gt;" | Acc]);
htmlencode([160 | Rest], Acc) -> 
    htmlencode(Rest, ["&nbsp;" | Acc]);
htmlencode([Else | Rest], Acc) -> 
    htmlencode(Rest, [Else | Acc]).

%% @doc Encode text content (handles special entities)
htmlencode_text(Text) ->
    htmlencode_text(Text, []).

htmlencode_text([], Acc) ->
    lists:flatten(lists:reverse(Acc));
htmlencode_text("©" ++ Rest, Acc) ->
    htmlencode_text(Rest, ["&copy;" | Acc]);
htmlencode_text([$& | Rest], Acc) ->
    htmlencode_text(Rest, ["&amp;" | Acc]);
htmlencode_text([$< | Rest], Acc) ->
    htmlencode_text(Rest, ["&lt;" | Acc]);
htmlencode_text([160 | Rest], Acc) ->
    htmlencode_text(Rest, ["&nbsp;" | Acc]);
htmlencode_text([H | Rest], Acc) ->
    htmlencode_text(Rest, [H | Acc]).
```

**Verification:**
- Module compiles successfully
- Can render simple AST nodes to HTML
- Output matches expected HTML format

---

### Phase 4: Integrate AST into Main Module

**File:** `src/erlmd.erl`

Modify the main module to use the AST pipeline.

**Tasks:**

1. Add include for types:

```erlang
-include_lib("erlmd/include/types.hrl").
```

2. Create new main conversion function:

```erlang
%% @doc Convert Markdown to HTML using AST pipeline
-spec conv_ast(list()) -> list().
conv_ast(String) ->
    Lex = lex(String),
    UntypedLines = make_lines(Lex),
    {TypedLines, Refs} = type_lines(UntypedLines),
    AST = erlmd_ast:build(TypedLines, Refs),
    erlmd_html:render(AST).
```

3. Modify existing `conv/1` to use the new pipeline:

```erlang
%% Original function - now uses AST
conv(String) -> 
    conv_ast(String).
```

**IMPORTANT:** Keep the old implementation temporarily for testing:

```erlang
%% @doc Original implementation (for comparison testing)
conv_original(String) -> 
    Lex = lex(String),
    UntypedLines = make_lines(Lex),
    {TypedLines, Refs} = type_lines(UntypedLines),
    parse(TypedLines, Refs).
```

4. Update `conv_utf8/1`:

```erlang
-spec conv_utf8(list()) -> list().
conv_utf8(Utf8) ->
    Str = xmerl_ucs:from_utf8(Utf8),
    Res = conv_ast(Str),
    xmerl_ucs:to_utf8(Res).
```

5. Update `conv_file/2`:

```erlang
conv_file(FileIn, FileOut) ->
    case file:open(FileIn, [read]) of
        {ok, Device} -> 
            Input = get_all_lines(Device,[]),
            Output = conv_ast(Input),
            write(FileOut, Output);
        _ -> 
            error
    end.
```

**Verification:**
- Module compiles with new includes
- Both `conv_ast/1` and `conv_original/1` produce output
- No functionality broken

---

### Phase 5: Testing and Verification

**File:** `test/erlmd_ast_tests.erl`

Create comprehensive tests to verify the refactoring.

**Tasks:**

1. Create test module:

```erlang
-module(erlmd_ast_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmd/include/types.hrl").

%% Test that AST pipeline produces identical output to original
original_compatibility_test_() ->
    TestCases = [
        {"# Header", "Simple header"},
        {"## Header 2", "Second level header"},
        {"Paragraph text", "Simple paragraph"},
        {"*emphasis* and **strong**", "Inline formatting"},
        {"`code`", "Inline code"},
        {"- Item 1\n- Item 2", "Unordered list"},
        {"1. Item 1\n2. Item 2", "Ordered list"},
        {"> Quote", "Blockquote"},
        {"    code block", "Code block"},
        {"---", "Horizontal rule"},
        {"[link](http://example.com)", "Link"},
        {"![image](http://example.com/img.png)", "Image"}
    ],
    [{Description, fun() -> test_compatibility(Markdown) end} 
     || {Markdown, Description} <- TestCases].

test_compatibility(Markdown) ->
    Original = erlmd:conv_original(Markdown),
    New = erlmd:conv_ast(Markdown),
    ?assertEqual(Original, New).

%% Test AST structure
ast_structure_test() ->
    AST = erlmd_ast:build([{normal, [{string, "Hello"}]}], []),
    ?assertMatch(#document{blocks = [#paragraph{}]}, AST).

header_ast_test() ->
    TypedLines = [{{h1, [{string, "Title"}]}, []}],
    AST = erlmd_ast:build(TypedLines, []),
    #document{blocks = [Header]} = AST,
    ?assertMatch(#header{level = 1}, Header).

list_ast_test() ->
    TypedLines = [
        {{ul, [{string, "Item 1"}]}, []},
        {{ul, [{string, "Item 2"}]}, []}
    ],
    AST = erlmd_ast:build(TypedLines, []),
    #document{blocks = [List]} = AST,
    ?assertMatch(#list{type = ul, items = [_, _]}, List).

%% Test inline parsing
emphasis_test() ->
    Inline = erlmd_ast:build_inline([{string, "*text*"}], []),
    ?assertMatch([#emphasis{content = [#text{content = "text"}]}], Inline).

strong_test() ->
    Inline = erlmd_ast:build_inline([{string, "**text**"}], []),
    ?assertMatch([#strong{content = [#text{content = "text"}]}], Inline).

code_span_test() ->
    Inline = erlmd_ast:build_inline([{string, "`code`"}], []),
    ?assertMatch([#code_span{content = "code"}], Inline).

link_test() ->
    Tokens = [{{inline, open}, "["}, {string, "text"}, 
              {{inline, close}, "]"}, {bra, "("}, 
              {string, "http://example.com"}, {ket, ")"}],
    Inline = erlmd_ast:build_inline(Tokens, []),
    ?assertMatch([#link{url = "http://example.com"}], Inline).
```

2. Run existing erlmd tests to ensure no regressions:

```bash
rebar3 eunit
```

3. Create comparison test script:

```erlang
%% File: test/compare_outputs.erl
-module(compare_outputs).
-export([run/0, test_file/1]).

run() ->
    TestFiles = filelib:wildcard("test/fixtures/*.md"),
    Results = lists:map(fun test_file/1, TestFiles),
    
    {Passed, Failed} = lists:partition(
        fun({_File, Result}) -> Result =:= ok end, 
        Results
    ),
    
    io:format("Passed: ~p~n", [length(Passed)]),
    io:format("Failed: ~p~n", [length(Failed)]),
    
    case Failed of
        [] -> ok;
        _ -> 
            io:format("Failed tests:~n"),
            lists:foreach(
                fun({File, _}) -> io:format("  - ~s~n", [File]) end,
                Failed
            )
    end.

test_file(File) ->
    {ok, Content} = file:read_file(File),
    Markdown = binary_to_list(Content),
    
    Original = erlmd:conv_original(Markdown),
    New = erlmd:conv_ast(Markdown),
    
    Result = case Original =:= New of
        true -> ok;
        false -> {diff, compute_diff(Original, New)}
    end,
    
    {File, Result}.

compute_diff(Original, New) ->
    % Simple line-by-line diff
    OrigLines = string:tokens(Original, "\n"),
    NewLines = string:tokens(New, "\n"),
    {length(OrigLines), length(NewLines)}.
```

**Verification:**
- All existing tests pass
- New AST tests pass
- Comparison tests show identical output

---

### Phase 6: Performance Testing

**File:** `test/erlmd_bench.erl`

Create benchmarks to ensure no performance regression.

**Tasks:**

1. Create benchmark module:

```erlang
-module(erlmd_bench).
-export([run/0, bench_original/1, bench_ast/1]).

run() ->
    Sizes = [small, medium, large],
    lists:foreach(fun run_bench/1, Sizes).

run_bench(Size) ->
    Markdown = generate_markdown(Size),
    
    {TimeOrig, _} = timer:tc(fun() -> bench_original(Markdown) end),
    {TimeAST, _} = timer:tc(fun() -> bench_ast(Markdown) end),
    
    Ratio = TimeAST / TimeOrig,
    
    io:format("~p: Original=~pμs, AST=~pμs, Ratio=~.2f~n", 
              [Size, TimeOrig, TimeAST, Ratio]).

bench_original(Markdown) ->
    lists:foreach(
        fun(_) -> erlmd:conv_original(Markdown) end,
        lists:seq(1, 100)
    ).

bench_ast(Markdown) ->
    lists:foreach(
        fun(_) -> erlmd:conv_ast(Markdown) end,
        lists:seq(1, 100)
    ).

generate_markdown(small) ->
    "# Header\n\nParagraph with *emphasis* and **strong**.\n\n- List item\n";

generate_markdown(medium) ->
    lists:flatten(lists:duplicate(10, generate_markdown(small)));

generate_markdown(large) ->
    lists:flatten(lists:duplicate(100, generate_markdown(small))).
```

**Verification:**
- AST pipeline performance within 2x of original
- No memory leaks
- Handles large documents

---

### Phase 7: Documentation

**File:** `doc/ast.md`

Document the AST structure and usage.

**Tasks:**

1. Create comprehensive AST documentation:

```markdown
# erlmd AST Documentation

## Overview

The erlmd library now uses an Abstract Syntax Tree (AST) representation to 
separate Markdown parsing from HTML generation. This enables:

- Multiple output formats (HTML, JSON, custom)
- Better testing and debugging
- Literate programming support
- Custom transformations

## AST Structure

### Document

The root node of any parsed Markdown document:

\`\`\`erlang
#document{
    blocks :: [block()]
}
\`\`\`

### Block Elements

#### Paragraph

\`\`\`erlang
#paragraph{
    content :: [inline()]
}
\`\`\`

#### Header

\`\`\`erlang
#header{
    level :: 1..6,
    content :: [inline()],
    type :: atx | setext
}
\`\`\`

... [Continue with all types]

## Usage

### Basic Conversion

\`\`\`erlang
1> Markdown = "# Hello World".
2> AST = erlmd:parse_to_ast(Markdown).
#document{blocks = [#header{level = 1, ...}]}
3> HTML = erlmd_html:render(AST).
"<h1>Hello World</h1>"
\`\`\`

### Custom Rendering

\`\`\`erlang
-module(my_renderer).
-include_lib("erlmd/include/types.hrl").

render(#document{blocks = Blocks}) ->
    % Custom rendering logic
    ok.
\`\`\`

## API Reference

### erlmd_ast

- `build/2` - Build AST from typed lines
- `build_inline/2` - Build inline elements

### erlmd_html

- `render/1` - Render document to HTML
- `render_block/1` - Render single block
- `render_inline/1` - Render single inline element
```

2. Update main README with AST information

3. Add inline code documentation (edoc)

**Verification:**
- Documentation is clear and complete
- Examples work as shown
- API is well documented

---

### Phase 8: Cleanup and Finalization

**Tasks:**

1. Remove `conv_original/1` after all tests pass
2. Update version number in `erlmd.app.src` to indicate major refactoring
3. Add CHANGELOG entry
4. Final code review for:
   - Unused imports
   - Dead code
   - Consistent naming
   - Proper error handling
5. Run dialyzer:

```bash
rebar3 dialyzer
```

6. Format code consistently:

```bash
rebar3 format
```

**Verification:**
- No compiler warnings
- Dialyzer passes with no errors
- Code is consistently formatted
- All tests pass

---

## Testing Strategy

### Test Coverage Requirements

1. **Unit Tests** (target: 90%+ coverage)
   - Each AST node type
   - Each rendering function
   - Edge cases (empty documents, special characters)

2. **Integration Tests**
   - End-to-end markdown → HTML
   - Comparison with original implementation
   - Complex nested structures

3. **Property-Based Tests** (optional, using PropEr)
   - Round-trip property: AST → HTML → Parse should be stable
   - Structural properties (e.g., nested lists always valid)

4. **Regression Tests**
   - All existing test cases must pass
   - Known edge cases from issues/bugs

### Test Files to Create

```
test/
├── erlmd_ast_tests.erl         # AST building tests
├── erlmd_html_tests.erl        # HTML rendering tests
├── erlmd_integration_tests.erl # End-to-end tests
├── compare_outputs.erl         # Comparison script
├── erlmd_bench.erl            # Performance benchmarks
└── fixtures/
    ├── simple.md
    ├── complex.md
    ├── edge_cases.md
    └── expected/
        ├── simple.html
        ├── complex.html
        └── edge_cases.html
```

---

## Error Handling

### Strategy

1. **Parse errors** - Collect and report, don't crash
2. **Malformed input** - Degrade gracefully, produce best-effort output
3. **Invalid AST** - Detect during rendering, provide helpful messages

### Implementation

Add error handling to `erlmd_ast:build/2`:

```erlang
-spec build([typed_line()], refs()) -> #document{} | {error, term()}.
build(TypedLines, Refs) ->
    try
        Blocks = parse_blocks(TypedLines, Refs, []),
        #document{blocks = lists:reverse(Blocks)}
    catch
        error:Reason -> {error, {parse_error, Reason}};
        throw:Reason -> {error, {parse_error, Reason}}
    end.
```

---

## Migration Guide for Users

### For Library Users

**No changes required** - the public API remains identical:

```erlang
%% Still works exactly the same
HTML = erlmd:conv(Markdown).
```

### For Contributors/Extenders

**New capabilities available:**

```erlang
%% Get AST instead of HTML
AST = erlmd_ast:build(TypedLines, Refs).

%% Render with custom renderer
Output = my_custom_renderer:render(AST).

%% Transform AST before rendering
TransformedAST = my_transform(AST),
HTML = erlmd_html:render(TransformedAST).
```

---

## Future Enhancements

After the refactoring is complete, these become possible:

1. **JSON renderer** - `erlmd_json:render(AST)`
2. **Markdown writer** - Round-trip capability
3. **AST transformations** - Code extraction for literate programming
4. **Syntax extensions** - Tables, footnotes, etc.
5. **Better error messages** - Source location tracking
6. **Incremental parsing** - For editor integration

---

## Implementation Checklist

### Phase 1: Types ✓
- [ ] Create `include/types.hrl`
- [ ] Define all block types
- [ ] Define all inline types
- [ ] Verify types compile

### Phase 2: AST Builder ✓
- [ ] Create `src/erlmd_ast.erl`
- [ ] Implement `build/2`
- [ ] Implement `parse_blocks/3`
- [ ] Implement `parse_list/3`
- [ ] Implement `build_inline/2`
- [ ] Implement helper functions
- [ ] Module compiles

### Phase 3: HTML Renderer ✓
- [ ] Create `src/erlmd_html.erl`
- [ ] Implement `render/1`
- [ ] Implement `render_block/1`
- [ ] Implement `render_inline/1`
- [ ] Implement HTML encoding
- [ ] Module compiles

### Phase 4: Integration ✓
- [ ] Update `src/erlmd.erl`
- [ ] Add include directive
- [ ] Create `conv_ast/1`
- [ ] Keep `conv_original/1` for testing
- [ ] Update `conv_utf8/1`
- [ ] Update `conv_file/2`
- [ ] All modules compile

### Phase 5: Testing ✓
- [ ] Create `test/erlmd_ast_tests.erl`
- [ ] Create compatibility tests
- [ ] Create AST structure tests
- [ ] Create `test/compare_outputs.erl`
- [ ] Run existing tests
- [ ] All tests pass

### Phase 6: Performance ✓
- [ ] Create `test/erlmd_bench.erl`
- [ ] Run benchmarks
- [ ] Performance within acceptable range

### Phase 7: Documentation ✓
- [ ] Create `doc/ast.md`
- [ ] Update README
- [ ] Add inline documentation
- [ ] Create examples

### Phase 8: Cleanup ✓
- [ ] Remove `conv_original/1`
- [ ] Update version number
- [ ] Add CHANGELOG entry
- [ ] Run dialyzer
- [ ] Format code
- [ ] Final review

---

## Notes for AI Execution

### Key Points for Claude Code

1. **Work incrementally** - Complete each phase fully before moving to next
2. **Preserve behavior** - The output must match the original exactly
3. **Test continuously** - Run tests after each significant change
4. **Handle edge cases** - The original handles many subtle cases
5. **Keep original helpers** - Many utility functions can be reused

### Common Pitfalls to Avoid

1. **Don't oversimplify inline parsing** - It's complex for good reasons
2. **Preserve whitespace handling** - Very subtle and important
3. **Keep reference resolution** - Must work exactly as before
4. **Don't break list parsing** - Tight vs loose lists are tricky
5. **HTML encoding must be complete** - Security and correctness

### Testing Strategy

1. **Start with simple cases** - Get basic headers and paragraphs working
2. **Add complexity gradually** - Lists, then blockquotes, then inline formatting
3. **Use comparison testing** - New output must match old output exactly
4. **Test edge cases explicitly** - Empty lines, escaped characters, etc.

### Recommended Order

1. Get simple blocks working (paragraphs, headers)
2. Add lists (the most complex block type)
3. Implement inline parsing (emphasis, strong, code)
4. Handle links and images
5. Add blockquotes and code blocks
6. Polish edge cases and error handling

---

## Success Criteria

The refactoring is complete when:

1. ✅ All existing tests pass
2. ✅ Output matches original implementation exactly
3. ✅ AST can be built and rendered separately
4. ✅ Performance within 2x of original
5. ✅ Code is well documented
6. ✅ No compiler warnings or dialyzer errors
7. ✅ Clean, maintainable code structure

---

## Contact and Questions

If you encounter issues during implementation:

1. Check the original `erlmd.erl` for reference
2. Review the inline comments in helper functions
3. Test with simple cases first
4. Use comparison testing to identify divergence

The goal is a clean refactoring that enables future extensions while maintaining perfect backward compatibility.
