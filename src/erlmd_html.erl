%%%-------------------------------------------------------------------
%%% @author    Claude Code
%%% @copyright (C) 2025, erlsci
%%% @doc HTML Renderer for erlmd AST
%%%
%%% This module renders the erlmd Abstract Syntax Tree to HTML output.
%%% It maintains compatibility with the original erlmd HTML output format.
%%%
%%% @end
%%% Created : 2025 by Claude Code
%%%-------------------------------------------------------------------

-module(erlmd_html).

-include("../include/types.hrl").

-export([
    render/1,           % render(#document{}) -> string()
    render_block/1,     % render_block(block()) -> string()
    render_inline/1     % render_inline(inline()) -> string()
]).

-define(NBSP, 160).
-define(SPACE, 32).
-define(LF, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Public API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Render a document to HTML
-spec render(#document{}) -> string().
render(#document{blocks = Blocks}) ->
    HTML = lists:map(fun render_block/1, Blocks),
    string:strip(lists:flatten(HTML), both, $\n).

%% @doc Render a single block element
-spec render_block(block()) -> string().
render_block(#paragraph{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    ContentStr = lists:flatten(InlineHTML),
    % Strip leading and trailing spaces (both sides)
    Stripped = string:strip(string:strip(ContentStr, left, $ ), right, $ ),
    "<p>" ++ Stripped ++ "</p>\n";

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal Functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Render a list item
render_list_item(#list_item{content = Blocks, tight = ItemTight}, _ListTight) ->
    % Use the item's own tight flag, not the list's overall tight flag
    BlockHTML = case ItemTight of
        true ->
            % Tight items don't wrap content in <p> tags
            lists:map(fun strip_para/1, Blocks);
        false ->
            % Loose items keep <p> tags
            lists:map(fun render_block/1, Blocks)
    end,
    ContentStr = string:strip(lists:flatten(BlockHTML), right, $\n),
    "<li>" ++ ContentStr ++ "</li>\n".

%% @doc Strip <p> tags from paragraph for tight lists
strip_para(#paragraph{content = Content}) ->
    InlineHTML = lists:map(fun render_inline/1, Content),
    Ret = lists:flatten(InlineHTML),
    string:strip(Ret, right, $\n) ++ "\n";
strip_para(Block) ->
    % Non-paragraph blocks get rendered normally
    render_block(Block).

%% @doc Encode HTML special characters (for code blocks and code spans)
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
htmlencode([?NBSP | Rest], Acc) ->
    htmlencode(Rest, ["&nbsp;" | Acc]);
htmlencode([Else | Rest], Acc) ->
    htmlencode(Rest, [Else | Acc]).

%% @doc Encode text content (handles special entities and characters)
htmlencode_text(Text) when is_list(Text) ->
    htmlencode_text(Text, []);
htmlencode_text(Text) ->
    Text.

htmlencode_text([], Acc) ->
    lists:flatten(lists:reverse(Acc));
% Preserve HTML entities that are already encoded
htmlencode_text([$&, $c, $o, $p, $y, $; | Rest], Acc) ->
    htmlencode_text(Rest, ["&copy;" | Acc]);
htmlencode_text([$&, $a, $m, $p, $; | Rest], Acc) ->
    htmlencode_text(Rest, ["&amp;" | Acc]);
htmlencode_text([$& | Rest], Acc) ->
    htmlencode_text(Rest, ["&amp;" | Acc]);
htmlencode_text([$< | Rest], Acc) ->
    htmlencode_text(Rest, ["&lt;" | Acc]);
htmlencode_text([?NBSP | Rest], Acc) ->
    htmlencode_text(Rest, ["&nbsp;" | Acc]);
htmlencode_text([H | Rest], Acc) ->
    htmlencode_text(Rest, [H | Acc]).
