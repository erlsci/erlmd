%%%-------------------------------------------------------------------
%%% @author    AST Builder
%%% @copyright (C) 2024
%%% @doc
%%% AST Builder for erlmd - converts typed lines to AST
%%% @end
%%%-------------------------------------------------------------------

-module(erlmd_ast).

-include("../include/types.hrl").

-export([
    build/2,           % build(TypedLines, Refs) -> #document{}
    build_inline/2     % build_inline(Tokens, Refs) -> [inline()]
]).

%% Internal exports for testing
-export([
    parse_blocks/3,    % for unit testing individual block parsing
    parse_inline/2     % for unit testing inline parsing
]).

-import(lists, [flatten/1, reverse/1]).

-define(SPACE, 32).
-define(TAB,    9).
-define(LF,    10).
-define(CR,    13).
-define(NBSP, 160).
-define(AMP, $&, $a, $m, $p, $;).
-define(COPY, $&, $c, $o, $p, $y, $;).

%% Internal type for typed lines (as produced by type_lines/1)
-type typed_line() :: {atom(), term()} | {{atom(), term()}, term()}.
-type refs() :: [{string(), {string(), string()}}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Public API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Build an AST document from typed lines and references
-spec build([typed_line()], refs()) -> #document{}.
build(TypedLines, Refs) ->
    Blocks = parse_blocks(TypedLines, Refs, []),
    #document{blocks = lists:reverse(Blocks)}.

%% @doc Build inline elements from tokens
-spec build_inline([term()], refs()) -> [inline()].
build_inline(Tokens, Refs) ->
    % Mark hard breaks in the token stream before parsing
    ProcessedTokens = mark_hard_breaks(Tokens),
    parse_inline_elements(ProcessedTokens, Refs, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Hard Line Break Detection
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Mark hard line breaks in token stream
%% Detects patterns: two spaces before LF, or tab before LF
mark_hard_breaks(Tokens) ->
    mark_hard_breaks(Tokens, []).

mark_hard_breaks([], Acc) ->
    lists:reverse(Acc);
%% Two spaces before LF - mark as hard break (check for comp type)
mark_hard_breaks([{{ws, comp}, _}, {{lf, _}, _LF} | T], Acc) ->
    % Replace with a hard break marker token
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
%% Two spaces before LF - mark as hard break (check for sp type)
mark_hard_breaks([{{ws, sp}, "  "}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
%% Tab before LF - mark as hard break
mark_hard_breaks([{{ws, tab}, _}, {{lf, _}, _LF} | T], Acc) ->
    mark_hard_breaks(T, [{{hard_break, true}, " <br />\n"} | Acc]);
%% Keep other tokens as-is
mark_hard_breaks([H | T], Acc) ->
    mark_hard_breaks(T, [H | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Block Parsing
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse blocks recursively, building AST nodes
-spec parse_blocks([typed_line()], refs(), [block()]) -> [block()].

%% Terminal clause
parse_blocks([], _Refs, Acc) ->
    Acc;

%% Tag handling - single tags should be wrapped in paragraphs (like unknown HTML)
parse_blocks([{tag, Tag} | T], Refs, Acc) ->
    % Single tags like <flame on> should be treated as inline HTML within a paragraph
    % Extract the raw HTML from the tag tokens
    TagStr = make_tag_str(Tag),
    Content = [#html_inline{content = TagStr}],
    Para = #paragraph{content = Content},
    parse_blocks(T, Refs, [Para | Acc]);

%% Block tag handling - opening tag starts a block, must collect content
parse_blocks([{blocktag, [{{{tag, open}, TagName}, OpenTag}]} | T], Refs, Acc) ->
    {Rest, BlockContent} = grab_block_html_content(T, TagName, []),
    Content = lists:flatten([OpenTag, "\n" | BlockContent]),
    HTMLBlock = #html_block{
        tag = TagName,
        content = Content,
        type = open
    },
    parse_blocks(Rest, Refs, [HTMLBlock | Acc]);
%% Self-closing or close tags
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% HTML Block Parsing
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Grab content for block-level HTML tags until closing tag
grab_block_html_content([], Type, Acc) ->
    {[], lists:reverse(["</" ++ Type ++ ">" | Acc])};
grab_block_html_content([{blocktag, [{{{tag, close}, Type}, CloseTag}]} | T], Type, Acc) ->
    {T, lists:reverse([CloseTag | Acc])};
grab_block_html_content([{blocktag, [{{{tag, _}, _OtherType}, OtherTag}]} | T], Type, Acc) ->
    % Different blocktag inside, just include it
    grab_block_html_content(T, Type, ["\n", OtherTag | Acc]);
grab_block_html_content([{_LineType, Content} | T], Type, Acc) ->
    Str = make_plain_str(Content),
    grab_block_html_content(T, Type, [Str | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% List Parsing
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse a list (ul or ol) and return the AST node
-spec parse_list(ul | ol, [typed_line()], refs()) -> {[typed_line()], #list{}}.
parse_list(Type, Lines, Refs) ->
    {Rest, Items, Tight} = collect_list_items(Type, Lines, Refs, [], true),
    List = #list{type = Type, items = lists:reverse(Items), tight = Tight},
    {Rest, List}.

%% @doc Collect list items recursively
collect_list_items(_Type, [], _Refs, Acc, Tight) ->
    {[], Acc, Tight};

collect_list_items(Type, [{{Type, P}, _} | T], Refs, Acc, Tight) ->
    {Rest, ItemContent, ItemWrap} = grab_list_item(T, Refs, [], Tight),

    % Build the item's blocks
    ItemBlocks = parse_list_item_content(P, ItemContent, Refs),
    Item = #list_item{content = ItemBlocks},

    % Check if there's a blank/linefeed between this item and the next
    NextTight = case T of
        [] -> ItemWrap;
        [{linefeed, _} | _] -> false;  % Blank between items
        [{blank, _} | _] -> false;     % Blank between items
        _ -> ItemWrap  % Use wrap status from item content
    end,

    % Once loose, stay loose (this is the key fix!)
    FinalTight = Tight andalso NextTight,

    collect_list_items(Type, Rest, Refs, [Item | Acc], FinalTight);

collect_list_items(_Type, List, _Refs, Acc, Tight) ->
    {List, Acc, Tight}.

%% @doc Parse the content of a single list item
parse_list_item_content(P, AdditionalContent, Refs) ->
    % List items contain at least one paragraph
    % Use snip to remove trailing linefeed
    Content = build_inline(snip(P), Refs),
    Para = #paragraph{content = Content},

    % Additional content goes here if present
    case AdditionalContent of
        [] -> [Para];
        _ -> [Para | AdditionalContent]  % Include additional blocks
    end.

%% @doc Check if a line has double indent (8+ spaces or 2+ tabs)
%% Returns {true, Rest} if double-indented, false otherwise
is_double_indent(List) ->
    is_double_indent1(List, 0).

is_double_indent1([], _N) ->
    false;
is_double_indent1(Rest, N) when N >= 8 ->
    {true, Rest};
is_double_indent1([{{ws, sp}, _} | T], N) ->
    is_double_indent1(T, N + 1);
is_double_indent1([{{ws, tab}, _} | T], N) ->
    is_double_indent1(T, N + 4);
is_double_indent1([{{ws, comp}, WS} | T], N) when is_list(WS) ->
    is_double_indent1(T, N + length(WS));
is_double_indent1(_List, _N) ->
    false.

%% @doc Grab content for a list item (handles nested content)
%% Based on original grab/4 from erlmd.erl lines 255-293
grab_list_item([], _Refs, Acc, Wrap) ->
    {[], lists:reverse(Acc), Wrap};

%% Codeblocks in list items (with double indent check)
grab_list_item([{{codeblock, P}, S} | T] = List, Refs, Acc, Wrap) ->
    case is_double_indent(S) of
        false ->
            % Not double-indented, stop grabbing
            {List, lists:reverse(Acc), false};
        {true, _Content} ->
            % Double-indented codeblock, include it
            % Note: The codeblock tokens are already processed
            ContentStr = make_plain_str(snip(P)),
            CodeBlock = #code_block{content = ContentStr, language = undefined},
            grab_list_item(T, Refs, [CodeBlock | Acc], Wrap)
    end;

%% Linefeed when in tight mode - might transition to loose
grab_list_item([{linefeed, _} | T], Refs, Acc, false) ->
    % Call grab_list_item2 to check for following content
    grab_list_item2(T, Refs, Acc, T, Acc, true);

%% Linefeed when already in loose mode - include blank line
grab_list_item([{linefeed, _} | T], Refs, Acc, true) ->
    grab_list_item(T, Refs, [#blank_line{} | Acc], true);

%% Blank when in tight mode - might transition to loose
grab_list_item([{blank, _} | T], Refs, Acc, false) ->
    grab_list_item2(T, Refs, Acc, T, Acc, true);

%% Blank when already in loose mode - include blank line
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
grab_list_item2([{normal, P} | T], Refs, Acc, OrigList, OrigAcc, Wrap) ->
    % Check if normal line starts with whitespace (continuation)
    case P of
        [{{ws, _}, _} | _] ->
            % Starts with whitespace, it's a continuation
            Content = build_inline(P, Refs),
            Para = #paragraph{content = Content},
            grab_list_item(T, Refs, [Para | Acc], Wrap);
        _ ->
            % Doesn't start with whitespace, stop grabbing
            % Return the original state (before the blanks)
            {OrigList, OrigAcc, false}
    end;

grab_list_item2([{linefeed, _} | T], Refs, Acc, OrigList, OrigAcc, _Wrap) ->
    grab_list_item2(T, Refs, [#blank_line{} | Acc], OrigList, OrigAcc, true);

grab_list_item2([{blank, _} | T], Refs, Acc, OrigList, OrigAcc, _Wrap) ->
    grab_list_item2(T, Refs, [#blank_line{} | Acc], OrigList, OrigAcc, true);

grab_list_item2(_List, _Refs, _Acc, OrigList, OrigAcc, _Wrap) ->
    % Stopped without finding continuation, return original state
    {OrigList, OrigAcc, true}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Inline Parsing
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Parse inline elements recursively
-spec parse_inline_elements([term()], refs(), [inline()]) -> [inline()].
parse_inline_elements([], _Refs, Acc) ->
    lists:reverse(Acc);

%% Hard line breaks (marked by mark_hard_breaks/1)
parse_inline_elements([{{hard_break, true}, _} | T], Refs, Acc) ->
    LB = #line_break{type = hard},
    parse_inline_elements(T, Refs, [LB | Acc]);

%% Images: ![alt](url "title")
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

%% Unmatched close bracket - treat as literal text
%% This handles cases like: \[id] where the open bracket was escaped
parse_inline_elements([{{inline, close}, _} | T], Refs, Acc) ->
    TextNode = #text{content = "]"},
    parse_inline_elements(T, Refs, [TextNode | Acc]);

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
parse_inline_elements([{tags, Tag} | T], Refs, Acc) ->
    HTMLInline = #html_inline{content = Tag},
    parse_inline_elements(T, Refs, [HTMLInline | Acc]);

parse_inline_elements([{{{tag, Type}, Tag}, Content} | T], Refs, Acc) ->
    HTMLStr = format_html_tag(Type, Tag, Content),
    HTMLInline = #html_inline{content = HTMLStr},
    parse_inline_elements(T, Refs, [HTMLInline | Acc]);

%% Regular text and emphasis/strong/code - process as text
parse_inline_elements([{_, Orig} | T], Refs, Acc) ->
    % Accumulate text until we hit a special token
    {Rest, TextStr} = collect_text_tokens([{string, Orig} | T]),
    % Parse the text for inline formatting
    Inlines = parse_text_formatting(TextStr, Refs),
    % Inlines is already in correct order (forward), so just prepend reversed to Acc
    parse_inline_elements(Rest, Refs, lists:reverse(Inlines, Acc)).

%% @doc Parse inline content (simplified delegation to get_inline logic)
parse_inline(Tokens, Refs) ->
    parse_inline_elements(Tokens, Refs, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Helper Functions for Inline Parsing
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Collect text tokens until we hit a special marker
collect_text_tokens([]) ->
    {[], ""};
collect_text_tokens([{string, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
collect_text_tokens([{string, _} | T]) ->
    % Skip {string, none} or other non-list string tokens
    collect_text_tokens(T);
% Stop at hard breaks
collect_text_tokens([{{hard_break, _}, _} | _] = List) ->
    {List, ""};
% Stop at inline markup (links, images)
collect_text_tokens([{{inline, _}, _} | _] = List) ->
    {List, ""};
% Stop at images (bang followed by inline open)
collect_text_tokens([{{punc, bang}, _}, {{inline, open}, _} | _] = List) ->
    {List, ""};
% Stop at escaped inline markers (backslash before bracket/bang)
collect_text_tokens([{{punc, bslash}, _}, {{inline, _}, _} | _] = List) ->
    {List, ""};
collect_text_tokens([{{punc, bslash}, _}, {{punc, bang}, _} | _] = List) ->
    {List, ""};
% Stop at URLs and email addresses
collect_text_tokens([{url, _} | _] = List) ->
    {List, ""};
collect_text_tokens([{email, _} | _] = List) ->
    {List, ""};
% Stop at HTML tags
collect_text_tokens([{tags, _} | _] = List) ->
    {List, ""};
% Handle tag tokens with content - include content for backtick processing
collect_text_tokens([{{{tag, _}, _}, Content} | T]) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, Content ++ More};
% CRITICAL FIX: Handle special whitespace token {{ws, none}, none}
collect_text_tokens([{{ws, none}, none} | T]) ->
    % This is non-space-filling whitespace - skip it
    collect_text_tokens(T);
% Skip tokens with atom content (like 'none')
collect_text_tokens([{_, Content} | T]) when is_atom(Content) ->
    collect_text_tokens(T);
% Handle other token types as text - but check if content is a list
collect_text_tokens([{_, S} | T]) when is_list(S) ->
    {Rest, More} = collect_text_tokens(T),
    {Rest, S ++ More};
% Skip tokens with other non-list content (numbers, etc)
collect_text_tokens([{_, _} | T]) ->
    collect_text_tokens(T).

%% @doc Parse text for inline formatting (emphasis, strong, code, etc.)
parse_text_formatting(Text, _Refs) ->
    parse_emphasis_and_code(Text, []).

%% @doc Parse emphasis, strong, and code spans from text
parse_emphasis_and_code([], Acc) ->
    lists:reverse(merge_adjacent_text(Acc));

%% Escaped emphasis markers
parse_emphasis_and_code([$\\, $*, $*, $* | T], Acc) ->
    TextNode = #text{content = "***"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Super-strong: ***text***
parse_emphasis_and_code([$*, $*, $* | T], Acc) ->
    case collect_until(T, [$*, $*, $*]) of
        {Rest, Content} ->
            EmContent = [#text{content = Content}],
            Em = #emphasis{content = EmContent, delimiter = $*},
            Strong = #strong{content = [Em], delimiter = $*},
            parse_emphasis_and_code(Rest, [Strong | Acc]);
        nomatch ->
            TextNode = #text{content = "***"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

%% Escaped strong
parse_emphasis_and_code([$\\, $*, $* | T], Acc) ->
    TextNode = #text{content = "**"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Strong: **text**
parse_emphasis_and_code([$*, $* | T], Acc) ->
    case collect_until(T, [$*, $*]) of
        {Rest, Content} ->
            Strong = #strong{content = [#text{content = Content}], delimiter = $*},
            parse_emphasis_and_code(Rest, [Strong | Acc]);
        nomatch ->
            TextNode = #text{content = "**"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

%% Escaped emphasis
parse_emphasis_and_code([$\\, $* | T], Acc) ->
    TextNode = #text{content = "*"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Emphasis: *text*
parse_emphasis_and_code([$* | T], Acc) ->
    case collect_until(T, [$*]) of
        {Rest, Content} ->
            Em = #emphasis{content = [#text{content = Content}], delimiter = $*},
            parse_emphasis_and_code(Rest, [Em | Acc]);
        nomatch ->
            TextNode = #text{content = "*"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

%% Underscore variants (same as above but with $_)
parse_emphasis_and_code([$\\, $_, $_, $_ | T], Acc) ->
    TextNode = #text{content = "___"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$_, $_, $_ | T], Acc) ->
    case collect_until(T, [$_, $_, $_]) of
        {Rest, Content} ->
            EmContent = [#text{content = Content}],
            Em = #emphasis{content = EmContent, delimiter = $_},
            Strong = #strong{content = [Em], delimiter = $_},
            parse_emphasis_and_code(Rest, [Strong | Acc]);
        nomatch ->
            TextNode = #text{content = "___"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

parse_emphasis_and_code([$\\, $_, $_ | T], Acc) ->
    TextNode = #text{content = "__"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$_, $_ | T], Acc) ->
    case collect_until(T, [$_, $_]) of
        {Rest, Content} ->
            Strong = #strong{content = [#text{content = Content}], delimiter = $_},
            parse_emphasis_and_code(Rest, [Strong | Acc]);
        nomatch ->
            TextNode = #text{content = "__"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

parse_emphasis_and_code([$\\, $_ | T], Acc) ->
    TextNode = #text{content = "_"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$_ | T], Acc) ->
    case collect_until(T, [$_]) of
        {Rest, Content} ->
            Em = #emphasis{content = [#text{content = Content}], delimiter = $_},
            parse_emphasis_and_code(Rest, [Em | Acc]);
        nomatch ->
            TextNode = #text{content = "_"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

%% Code spans: `code` and ``code``
parse_emphasis_and_code([$\\, $` | T], Acc) ->
    TextNode = #text{content = "`"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% General backslash escape for markdown special characters only
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

parse_emphasis_and_code([$`, $` | T], Acc) ->
    case collect_until(T, [$`, $`]) of
        {Rest, Content} ->
            Code = #code_span{content = Content, delimiter = double},
            parse_emphasis_and_code(Rest, [Code | Acc]);
        nomatch ->
            TextNode = #text{content = "``"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

parse_emphasis_and_code([$` | T], Acc) ->
    case collect_until(T, [$`]) of
        {Rest, Content} ->
            Code = #code_span{content = Content, delimiter = single},
            parse_emphasis_and_code(Rest, [Code | Acc]);
        nomatch ->
            TextNode = #text{content = "`"},
            parse_emphasis_and_code(T, [TextNode | Acc])
    end;

%% Line breaks (hard breaks already handled at token level)
parse_emphasis_and_code([?CR, ?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?LF | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

parse_emphasis_and_code([?CR | T], Acc) ->
    LB = #line_break{type = soft},
    parse_emphasis_and_code(T, [LB | Acc]);

%% HTML entities - keep as entities
parse_emphasis_and_code([$&, $c, $o, $p, $y, $; | T], Acc) ->
    TextNode = #text{content = "&copy;"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$&, $a, $m, $p, $; | T], Acc) ->
    TextNode = #text{content = "&amp;"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Special characters
parse_emphasis_and_code([$& | T], Acc) ->
    TextNode = #text{content = "&"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([$< | T], Acc) ->
    TextNode = #text{content = "<"},
    parse_emphasis_and_code(T, [TextNode | Acc]);

parse_emphasis_and_code([?NBSP | T], Acc) ->
    TextNode = #text{content = [194, 160]},  % UTF-8 for nbsp
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Tab - expand to 4 spaces (hard breaks with tab already handled above)
parse_emphasis_and_code([?TAB | T], Acc) ->
    TextNode = #text{content = "    "},
    parse_emphasis_and_code(T, [TextNode | Acc]);

%% Regular character - accumulate
parse_emphasis_and_code([H | T], Acc) ->
    {Rest, Text} = collect_regular_text([H | T]),
    case Text of
        "" -> parse_emphasis_and_code(Rest, Acc);
        _ ->
            TextNode = #text{content = Text},
            parse_emphasis_and_code(Rest, [TextNode | Acc])
    end.

%% @doc Merge adjacent text nodes (nodes are in reverse order in the accumulator)
merge_adjacent_text([]) -> [];
merge_adjacent_text([#text{content = C1}, #text{content = C2} | T]) ->
    % C1 and C2 are in reverse order, so merge as C2 ++ C1 to maintain correct order
    merge_adjacent_text([#text{content = C2 ++ C1} | T]);
merge_adjacent_text([H | T]) ->
    [H | merge_adjacent_text(T)].

%% @doc Collect characters until delimiter is found
collect_until(Text, Delim) ->
    collect_until(Text, Delim, []).

collect_until([], _Delim, _Acc) ->
    nomatch;
collect_until(Text, [D], Acc) when is_integer(D) ->
    case Text of
        [D | Rest] -> {Rest, lists:reverse(Acc)};
        [H | Rest] -> collect_until(Rest, [D], [H | Acc]);
        [] -> nomatch
    end;
collect_until(Text, Delim, Acc) when is_list(Delim) ->
    case lists:prefix(Delim, Text) of
        true -> {lists:nthtail(length(Delim), Text), lists:reverse(Acc)};
        false ->
            case Text of
                [H | Rest] -> collect_until(Rest, Delim, [H | Acc]);
                [] -> nomatch
            end
    end.

%% @doc Collect regular text characters (not special markdown chars)
collect_regular_text(String) ->
    collect_regular_text(String, []).

collect_regular_text([], Acc) ->
    {[], lists:reverse(Acc)};
collect_regular_text([H | T], Acc) when H =:= $*; H =:= $_; H =:= $`;
                                         H =:= $\\; H =:= ?LF; H =:= ?CR;
                                         H =:= $&; H =:= $< ->
    case Acc of
        [] -> {[H | T], ""};  % No regular text collected, return empty string
        _ -> {[H | T], lists:reverse(Acc)}
    end;
collect_regular_text([H | T], Acc) ->
    collect_regular_text(T, [H | Acc]).

%% @doc Get inline content for links and images
get_inline_content(Tokens, Refs, Type) ->
    % This needs to parse [text](url "title") or reference-style links
    case parse_inline_link_or_image(Tokens, Refs, Type) of
        {Rest, {Url, Title, Text}} -> {Rest, {Url, Title, Text}};
        normal -> {Tokens, ""}
    end.

%% @doc Parse inline link or image (simplified version)
parse_inline_link_or_image([], _Refs, _Type) ->
    normal;
parse_inline_link_or_image(Tokens, Refs, Type) ->
    case get_link_text(Tokens, []) of
        {Rest1, Text, close} ->
            case Rest1 of
                [{bra, _} | Rest2] ->
                    % Direct link/image: [text](url)
                    case get_url_and_title(Rest2, []) of
                        {Rest3, Url, Title} ->
                            {Rest3, {Url, Title, Text}};
                        normal ->
                            normal
                    end;
                % Allow space before parenthesis for images: ![alt] (url)
                [{{ws, _}, _}, {bra, _} | Rest2] when Type =:= img ->
                    % Direct image with space: ![text] (url)
                    case get_url_and_title(Rest2, []) of
                        {Rest3, Url, Title} ->
                            {Rest3, {Url, Title, Text}};
                        normal ->
                            normal
                    end;
                % Allow optional space between brackets for images: ![alt] [ref]
                [{{ws, _}, _}, {{inline, open}, _} | Rest2] when Type =:= img ->
                    % Reference image with space: ![text] [id]
                    case get_link_text(Rest2, []) of
                        {Rest3, RefId, close} ->
                            case lists:keyfind(RefId, 1, Refs) of
                                {RefId, {Url, Title}} ->
                                    {Rest3, {Url, Title, Text}};
                                false ->
                                    normal
                            end;
                        _ ->
                            normal
                    end;
                % Allow optional space between brackets for links too: [text] [ref]
                [{{ws, _}, _}, {{inline, open}, _} | Rest2] when Type =:= url ->
                    % Reference link with space: [text] [id]
                    case get_link_text(Rest2, []) of
                        {Rest3, RefId, close} ->
                            case lists:keyfind(RefId, 1, Refs) of
                                {RefId, {Url, Title}} ->
                                    {Rest3, {Url, Title, Text}};
                                false ->
                                    normal
                            end;
                        _ ->
                            normal
                    end;
                [{{inline, open}, _} | Rest2] ->
                    % Reference link: [text][id]
                    case get_link_text(Rest2, []) of
                        {Rest3, RefId, close} ->
                            case lists:keyfind(RefId, 1, Refs) of
                                {RefId, {Url, Title}} ->
                                    {Rest3, {Url, Title, Text}};
                                false ->
                                    normal
                            end;
                        _ ->
                            normal
                    end;
                _ ->
                    % Implicit reference: [text]
                    case lists:keyfind(Text, 1, Refs) of
                        {Text, {Url, Title}} ->
                            {Rest1, {Url, Title, Text}};
                        false ->
                            normal
                    end
            end;
        _ ->
            normal
    end.

%% @doc Get text between brackets
get_link_text(Tokens, Acc) ->
    get_link_text(Tokens, Acc, 0).

%% @doc Get link text with bracket depth tracking
get_link_text([], Acc, _Depth) ->
    {[], make_plain_str(lists:reverse(Acc)), incomplete};
get_link_text([{{inline, close}, _} | Rest], Acc, 0) ->
    % Only close when depth is 0 (outermost bracket)
    {Rest, make_plain_str(lists:reverse(Acc)), close};
get_link_text([{{inline, close}, _} = H | Rest], Acc, Depth) when Depth > 0 ->
    % Nested closing bracket - include it and decrease depth
    get_link_text(Rest, [H | Acc], Depth - 1);
get_link_text([{{inline, open}, _} = H | Rest], Acc, Depth) ->
    % Opening bracket - include it and increase depth
    get_link_text(Rest, [H | Acc], Depth + 1);
get_link_text([{{punc, bslash}, _}, {{inline, close}, C} | Rest], Acc, Depth) ->
    % Escaped closing bracket
    get_link_text(Rest, [{string, C} | Acc], Depth);
get_link_text([H | Rest], Acc, Depth) ->
    get_link_text(Rest, [H | Acc], Depth).

%% @doc Get URL and optional title from (url "title")
get_url_and_title([], _Acc) ->
    normal;
get_url_and_title([{ket, _} | Rest], Acc) ->
    % End of URL section
    {Url, Title} = parse_url_title(lists:reverse(Acc)),
    {Rest, Url, Title};
get_url_and_title([H | Rest], Acc) ->
    get_url_and_title(Rest, [H | Acc]).

%% @doc Parse accumulated URL and title tokens
parse_url_title(Tokens) ->
    case split_url_title(Tokens, []) of
        {Url, []} ->
            {make_plain_str(Url), ""};
        {Url, Title} ->
            {make_plain_str(Url), make_plain_str(Title)}
    end.

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
    lists:reverse(strip_trailing_ws_rev(lists:reverse(Tokens))).

strip_trailing_ws_rev([{{ws, _}, _} | Rest]) ->
    strip_trailing_ws_rev(Rest);
strip_trailing_ws_rev(List) ->
    List.

collect_until_quote([], Acc) ->
    {lists:reverse(Acc), []};
collect_until_quote([{{punc, doubleq}, _} | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
collect_until_quote([H | Rest], Acc) ->
    collect_until_quote(Rest, [H | Acc]).

collect_until_single_quote([], Acc) ->
    {lists:reverse(Acc), []};
collect_until_single_quote([{{punc, singleq}, _} | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
collect_until_single_quote([H | Rest], Acc) ->
    collect_until_single_quote(Rest, [H | Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Helper Functions (copied/adapted from erlmd.erl)
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    NewP1 = make_br(P1),
    lists:flatten([NewP1, {string, ""} | P2]).

merge_with_br(P1, P2) ->
    NewP1 = make_br(P1),
    lists:flatten([NewP1, {tags, "<br /> "} | P2]).

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
make_plain_str([{_, Str} | T], Acc) when is_list(Str) ->
    make_plain_str(T, [Str | Acc]);
make_plain_str([{_, _} | T], Acc) ->
    make_plain_str(T, Acc).

parse_tag(Tag, _Refs) ->
    % Convert tag tokens to HTML block (unused now - tags wrapped in paragraphs)
    Content = case Tag of
        [_H | _] -> make_plain_str(Tag);
        _ -> ""
    end,
    #html_block{
        tag = "unknown",
        content = Content,
        type = self_closing
    }.

%% @doc Convert a string back to tokens for re-parsing (for nested inline content)
%% This is needed for link text that contains images like [![alt](img)](url)
retokenize_string(String) ->
    % Check if string contains image syntax ![
    case string:str(String, "![") of
        0 ->
            % No image syntax, just return as text
            [{string, String}];
        Pos ->
            % Has image syntax - need to tokenize it properly
            % Extract the parts: text before, image marker, rest
            Before = string:substr(String, 1, Pos - 1),
            After = string:substr(String, Pos),
            % Simple tokenization for image: ![alt](url)
            case parse_simple_image(After) of
                {image, Alt, Url, Rest} ->
                    % Create tokens for: before-text, image, rest
                    BeforeTokens = if Before == "" -> []; true -> [{string, Before}] end,
                    ImageTokens = [{{punc, bang}, "!"}, {{inline, open}, "["}, {string, Alt},
                                   {{inline, close}, "]"}, {bra, "("}, {string, Url}, {ket, ")"}],
                    RestTokens = if Rest == "" -> []; true -> retokenize_string(Rest) end,
                    BeforeTokens ++ ImageTokens ++ RestTokens;
                no_match ->
                    [{string, String}]
            end
    end.

%% @doc Simple parser for ![alt](url) syntax
parse_simple_image("![" ++ Rest) ->
    case extract_bracketed(Rest, $]) of
        {Alt, "](" ++ AfterBracket} ->
            case extract_bracketed(AfterBracket, $)) of
                {Url, ")" ++ Remaining} ->
                    {image, Alt, Url, Remaining};
                _ -> no_match
            end;
        _ -> no_match
    end;
parse_simple_image(_) -> no_match.

extract_bracketed(String, EndChar) ->
    extract_bracketed(String, EndChar, []).

extract_bracketed([], _EndChar, Acc) ->
    {lists:reverse(Acc), ""};
extract_bracketed([EndChar | Rest], EndChar, Acc) ->
    {lists:reverse(Acc), [EndChar | Rest]};
extract_bracketed([C | Rest], EndChar, Acc) ->
    extract_bracketed(Rest, EndChar, [C | Acc]).

%% @doc Extract raw HTML string from tag tokens
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

format_html_tag(Type, Tag, _Content) ->
    case Type of
        open -> "&lt;" ++ Tag ++ "&gt;";
        close -> "&lt;/" ++ Tag ++ "&gt;";
        self_closing -> "&lt;" ++ Tag ++ " /&gt;"
    end.
