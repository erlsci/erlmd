%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie
%%% @copyright (C) 2009, Gordon Guthrie
%%% @doc,
%%%
%%% @end
%%% Created : 10 Sep 2009 by gordonguthrie@backawinner.gg
%%%-------------------------------------------------------------------

-module(erlmd).

-include_lib("erlmd/include/types.hrl").

-export([conv/1,
         conv/2,
         conv_utf8/1,
         conv_file/2,
         conv_ast/1,
         conv_html/1,
         default_opts/0]).

-import(lists, [flatten/1, reverse/1]).

-define(SPACE, 32).
-define(TAB,    9).
-define(LF,    10).
-define(CR,    13).
-define(NBSP, 160).
-define(AMP, $&, $a, $m, $p, $;).
-define(COPY, $&, $c, $o, $p, $y, $;).

%%% the lexer first lexes the input
%%% make_lines does 2 passes:
%%% * it chops the lexed strings into lines which it represents as a
%%%   list of lists
%%% * it then types the lines into the following:
%%% * normal lines
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - SETEXT header lines
%%%   - ATX header lines
%%%   - blockquote
%%%   - unordered lists
%%%   - ordered lists
%%%   - code blocks
%%%   - horizontal rules
%%% the parser then does its magic interpolating the references as appropriate

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Public API
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Default options for conversion
-spec default_opts() -> map().
default_opts() ->
    #{format => html}.

%% @doc Convert Markdown using default options (HTML output)
-spec conv(string()) -> string().
conv(Input) ->
    conv(Input, default_opts()).

%% @doc Convert Markdown with options
%% Options:
%%   #{format => html} - Convert to HTML (default)
%%   #{format => ast}  - Return AST structure
-spec conv(string() | document(), map()) -> string() | document().
conv(Input, #{format := ast}) ->
    conv_ast(Input);
conv(Input, #{format := html}) ->
    conv_html(Input).

%% @doc Convert Markdown string to AST
-spec conv_ast(string()) -> document().
conv_ast(String) when is_list(String) ->
    Lex = lex(String),
    UntypedLines = make_lines(Lex),
    {TypedLines, Refs} = type_lines(UntypedLines),
    erlmd_ast:build(TypedLines, Refs).

%% @doc Convert to HTML (accepts AST or Markdown string)
-spec conv_html(document() | string()) -> string().
conv_html(AST) when is_record(AST, document) ->
    erlmd_html:render(AST);
conv_html(String) when is_list(String) ->
    conv_html(conv_ast(String)).

%% @doc Convert UTF-8 encoded Markdown to HTML
-spec conv_utf8(list()) -> list().
conv_utf8(Utf8) ->
    Str = xmerl_ucs:from_utf8(Utf8),
    Res = conv(Str),  % Uses default HTML output
    xmerl_ucs:to_utf8(Res).

%% @doc Convert Markdown file to HTML file
conv_file(FileIn, FileOut) ->
    case file:open(FileIn, [read]) of
        {ok, Device} -> Input = get_all_lines(Device,[]),
                        Output = conv(Input),  % Uses default HTML output
                        write(FileOut, Output);
        _            -> error
    end.

get_all_lines(Device, Accum) ->
    case io:get_line(Device,"") of
        eof  -> file:close(Device),
                Accum;
        Line ->
            get_all_lines(Device,Accum ++ Line)
    end.

write(File, Text) ->
    _Return=filelib:ensure_dir(File),
    case file:open(File, [write]) of
        {ok, Id} ->
            io:fwrite(Id, "~s~n", [Text]),
            file:close(Id);
        _ ->
            error
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Make the lines from the raw tokens
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_lines(Tokens) -> ml1(Tokens, [], []).

ml1([], [], A2)                -> reverse(A2);
ml1([], A1, A2)                -> ml1([], [], [reverse(A1) | A2]);
ml1([{{lf, _}, _} = H | T], A1, A2) -> ml1(T, [], [ml2(H, A1) | A2]);
ml1([H | T], A1, A2)                -> ml1(T, [H | A1], A2).

ml2(H, List) -> reverse([H | List]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Process the lines and give each line a type. The valid types are:
%%% * normal line
%%% * reference style links
%%% * reference style images
%%% * special line types
%%%   - blank
%%%   - SETEXT header lines
%%%   - ATX header lines
%%%   - unordered lists (including code blocks)
%%%   - ordered lists (including code blocks)
%%%   - blockquotes
%%%   - code blocks
%%%   - horizontal rules
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
type_lines(Lines) ->
    {Refs, TypedLines} = t_l1(Lines, [], []),
    % io:format("TypedLines before stripping ~p~n", [TypedLines]),
    {strip_lines(TypedLines), Refs}.

t_l1([], A1, A2) -> {A1, reverse(A2)};
%% this clause extracts URL and Image refs
%% (it is the only one that uses A1 and A2...
%% inlines can have up to 3 spaces before it
t_l1([[{{ws, sp}, _},
       {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);
t_l1([[{{ws, tab}, _},
       {{inline, open}, _} | _] = H | T2], A1, A2) ->
    t_l1(T2, A1, [type_ws(H) | A2]);
t_l1([[{{ws, comp}, W},
       {{inline, open}, _} | T1] = H | T2], A1, A2) ->
    case gt(W, 4) of
        {true, _R} -> t_l1(T2, A1, [type_ws(H) | A2]);
        false      -> t_inline(H, T1, T2, A1, A2)
    end;
t_l1([[{{inline, open}, _} | T1] = H | T2], A1, A2) ->
    t_inline(H, T1, T2, A1, A2);

%% types setext lines
t_l1([[{{md, eq}, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_setext_h1(H) | A2]);
%% NOTE 1: generates a ul as the default not a normal line
%% NOTE 2: depending on the context this might generate an <h2> header
%%         or an <hr />
%% NOTE 3: space - is typed to a bullet down in <ul> land...
t_l1([[{{md, dash}, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_setext_h2(H) | A2]);

%% types atx lines
t_l1([[{{md, atx}, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_atx(H) | A2]);

%% types blockquotes
%% a blockquote on its own or followed by a linefeed is
%% displayed 'as is' by showdown
t_l1([[{{md, gt}, _} | []] = H | T], A1, A2) ->
    t_l1(T, A1, [{normal, H} | A2]);
t_l1([[{{md, gt}, _}, {{lf, _}, _} | []] = H | T], A1, A2) ->
    t_l1(T, A1, [{normal, H} | A2]);
%% one with anything after it starts a blockquote
t_l1([[{{md, gt}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{blockquote, H} | A2]);

%% types unordered lists lines
%% NOTE 1: the dashed version is generated in type_setext_h2
%% NOTE 2: the asterix version also might generate a horizontal rule
%%         which is why it jumps to type_star2 <-- note the 2!!
t_l1([[{{ws, _}, _}, {{md, star}, _} = ST1,
       {{ws, _}, _} = WS1 | T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{type_star2([ST1, WS1 | T1]), H} | A2]);
t_l1([[{{md, star}, _}, {{ws, _}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{type_star2(H), H} | A2]);
t_l1([[{{ws, _}, _}, {{md, plus}, _},
       {{ws, _}, _} = W | T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{{ul, make_list_str([W | T1])}, H} | A2]);
t_l1([[{{md, plus}, _}, {{ws, _}, _} = W | T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{{ul, make_list_str([W | T1])}, H} | A2]);
%% UL based on dashes
t_l1([[{{ws, _}, _}, {{md, dash}, _},
       {{ws, _}, _} = W | T1] = H | T], A1, A2) ->
    t_l1(T, A1, [{{ul, make_list_str([W | T1])}, H} | A2]);

%% types ordered lists...
t_l1([[{{ws, WS}, _}, {num, _} = N1| T1] | T], A1, A2) when WS /= comp ->
    t_l1(T, A1, [type_ol([N1 | T1]) | A2]);
t_l1([[{num, _} | _T] = H | T], A1, A2) ->
    t_l1(T, A1, [type_ol(H) | A2]);

%% types horizontal rules for stars and underscores
%% dashes and some stars are done elsewhere...
t_l1([[{{md, underscore}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [type_underscore(H) | A2]);
t_l1([[{{md, star}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [type_star(H) | A2]);

%% Block level tags - these are look ahead they must be
%% on a single line (ie directly followed by a lf and nothing else
t_l1([[{{{tag, _Type}, Tag}, _ } = H | T1] = List | T], A1, A2) ->
    case is_blank(T1) of
        false -> t_l1(T, A1, [{normal , List} | A2]);
        true  -> case is_block_tag(Tag) of
                     true  -> t_l1(T, A1, [{blocktag , [H]} | A2]);
                     false -> t_l1(T, A1, [{tag, [H | T1]} | A2])
                 end
    end;

%% types a blank line or a code block
t_l1([[{{lf, _}, _}| []]  = H | T], A1, A2) ->
    t_l1(T, A1, [{linefeed, H} | A2]);
t_l1([[{{ws, _}, _} | _T1] = H | T], A1, A2) ->
    t_l1(T, A1, [type_ws(H) | A2]);

%% Final clause...
t_l1([H | T], A1, A2) ->
    t_l1(T, A1, [{normal , H} | A2]).

t_inline(H, T1, T2, A1, A2) ->
    case snip_ref(T1) of
        {Type, {Id, {Url, Title}}} -> t_l1(T2, flatten([{Id, {Url, Title}} | A1]),
                                           [{Type, H} | A2]);
        normal                     -> t_l1(T2, A1, [{normal, H} | A2])
    end.

%% strips blanks from the beginning and end
strip_lines(List) -> reverse(strip_l1(reverse(strip_l1(List)))).

strip_l1([{linefeed, _} | T]) -> strip_l1(T);
strip_l1([{blank, _} | T])    -> strip_l1(T);
strip_l1(List)                -> List.

%%
%% Loads of type rules...
%%

is_blank([])                  -> true;
is_blank([{{lf, _}, _} | []]) -> true;
is_blank([{{ws, _}, _} | T])  -> is_blank(T);
is_blank(_List)               -> false.

is_block_tag("address")    -> true;
is_block_tag("blockquote") -> true;
is_block_tag("center")     -> true;
is_block_tag("dir")        -> true;
is_block_tag("div")        -> true;
is_block_tag("dl")         -> true;
is_block_tag("fieldset")   -> true;
is_block_tag("form")       -> true;
is_block_tag("h1")         -> true;
is_block_tag("h2")         -> true;
is_block_tag("h3")         -> true;
is_block_tag("h4")         -> true;
is_block_tag("h5")         -> true;
is_block_tag("h6")         -> true;
is_block_tag("hr")         -> true;
is_block_tag("isindex")    -> true;
is_block_tag("menu")       -> true;
is_block_tag("noframes")   -> true;
is_block_tag("noscript")   -> true;
is_block_tag("ol")         -> true;
is_block_tag("p")          -> true;
is_block_tag("pre")        -> true;
is_block_tag("table")      -> true;
is_block_tag("thead")      -> true;
is_block_tag("tbody")      -> true;
is_block_tag("tr")         -> true;
is_block_tag("td")         -> true;
is_block_tag("ul")         -> true;
is_block_tag(_Other)       -> false.

type_underscore(List) ->
    case type_underscore1(trim_right(List)) of
        hr    -> {hr, List};
        maybe1 -> {type_underscore2(List), List}
    end.

type_underscore1([])                          -> hr;
type_underscore1([{{md, underscore}, _} | T]) -> type_underscore1(T);
type_underscore1(_List)                       -> maybe1.

type_underscore2(List) ->
    case trim_right(List) of % be permissive of trailing spaces
        [{{md, underscore}, _}, {{ws, _}, _},
         {{md, underscore}, _}, {{ws, _}, _},
         {{md, underscore}, _}]               -> hr;
        _Other                                -> normal
    end.

type_star(List) ->
    Trim = trim_right(List),
    case type_star1(Trim) of % be permssive of trailing spaces
        hr    -> {hr, trim_right(Trim)};
        maybe1 -> Type = type_star2(List),
                 % if it is a normal line we prepend it with a special
                 % non-space filling white space character
                 case Type of
                     normal -> {normal, [{{ws, none}, none} | List]};
                     _      -> {Type, List}
                 end
    end.

type_star1([])                    -> hr;
type_star1([{{md, star}, _} | T]) -> type_star1(T);
type_star1(_List)                 -> maybe1.

type_star2(List) ->
    case trim_right(List) of
        [{{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}, {{ws, _}, _},
         {{md, star}, _}]                -> hr;
        _Other ->
            case List of
                [{{md, star}, _},
                 {{ws, _}, _}= WS | T] -> {ul, make_list_str([WS | T])};
                _Other2                -> normal
            end
    end.

type_ol(List) ->
    case type_ol1(List, []) of
        normal            -> {normal, List};
        {ol, Str}         -> {{ol, Str}, List};
        {esc_normal, Str} -> {normal, Str}
    end.


%% this line terminates on an escaped fullstop after a number
%% (but you need to drop the bslash...)
type_ol1([{num, _} = N,
          {{punc, bslash}, _},
          {{punc, fullstop}, _} = P | T], Acc) ->
    {esc_normal, flatten([reverse(Acc), N, P | T])};
%% we accumulate the digits in case we need to escape a full stop in a normal line
type_ol1([{num, _} = H | T], Acc)  -> type_ol1(T, [H | Acc]);
type_ol1([{{punc, fullstop}, _},
          {{ws, _}, _} | T], _Acc) -> {ol, T};
type_ol1(_List, _Acc)              -> normal.

%% You need to understand what this function is trying to d...
%% '### blah' is fine
%% '### blah ###' is reduced to '### blah' because trailing #'s are
%% just for show but...
%% '##' is like appling '#' to '#' <-- applying 1 less styling to a single #
%% and '###' is like appling '##' to '#' etc, etc
%% but after you hit 6#'s you just get this for a single hash
%% ie '#############' is like applying '######' to a single '#'
%% but/and '######## blah' is like apply '######' to '## blah'
%% strip trailing #'s as they are decorative only...
type_atx(List) ->
    {Sz, R} = get_atx_size(List),
    A = [{{md, atx}, "#"}],
    Type =
        case is_all_hashes(R) of
            true  ->
                if
                    Sz == 1 ->
                        normal;
                    ((Sz > 1) andalso (Sz < 6)) ->
                        Ns = integer_to_list(Sz - 1),
                        Hn = list_to_atom("h" ++ Ns),
                        {Hn, A};
                    ((Sz == 6) andalso (R == [])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, lf}, "\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R == [{{lf, crlf}, "\r\n"}])) ->
                        {h5, A};
                    ((Sz == 6) andalso (R =/= [])) ->
                        {h6, A}
                end;
            false ->
                Ns = integer_to_list(Sz),
                Hn = list_to_atom("h" ++ Ns),
                {Hn, strip_atx(R)}
        end,
    {Type, List}.

is_all_hashes([])                   -> true;
is_all_hashes([{{md, atx}, _} | T]) -> is_all_hashes(T);
is_all_hashes([{{lf, _}, _} | []])  -> true;
is_all_hashes(_List)                -> false.

get_atx_size(List) -> g_atx_size1(List, 0).

% this function also strips whitespace to the left...
g_atx_size1([{{md, atx}, _} = A | T], N) when N == 6 -> {6, [A | T]};
g_atx_size1([{{md, atx}, _} | T], N)                 -> g_atx_size1(T, N + 1);
g_atx_size1([{{ws, _}, _} | T], N)                   -> g_atx_size1(T, N);
g_atx_size1(List, N)                                 -> {N, List}.

strip_atx(List) -> reverse(s_atx1(reverse(List))).

s_atx1([{{lf, _}, _}, {{md, atx}, _} | T]) -> s_atx1(T);
s_atx1([{{md, atx}, _} | T])               -> s_atx1(T);
s_atx1(List)                               -> List.

type_setext_h1(List) -> type_s_h1_1(List, []).

%% terminates on running out or new line
type_s_h1_1([{{lf, _}, _} = L | []], Acc) -> {setext_h1, reverse([L | Acc])};
type_s_h1_1([], Acc)                      -> {setext_h1, reverse(Acc)};
type_s_h1_1([[] | T], Acc)                -> type_s_h1_1(T, Acc);
type_s_h1_1([{{md, eq}, _} = H | T], Acc) -> type_s_h1_1(T, [H | Acc]);
type_s_h1_1(L, Acc)                       ->  {normal, flatten([Acc | L])}.

type_setext_h2(List) ->
    case type_s_h2_1(List) of
        h2_or_hr -> {h2_or_hr, List};
        not_h2   -> {type_s_h2_2(trim_right(List)), List}
    end.
%% terminates on running out or new line
type_s_h2_1([{{lf, _}, _} | []])   -> h2_or_hr;
type_s_h2_1([])                    -> h2_or_hr;
type_s_h2_1([[] | T])              -> type_s_h2_1(T);
type_s_h2_1([{{md, dash}, _} | T]) -> type_s_h2_1(T);
type_s_h2_1(_L)                    -> not_h2.

type_s_h2_2([{{md, dash}, _}, {{ws,_}, _},
             {{md, dash}, _}, {{ws, _}, _},
             {{md, dash}, _}])              -> hr;
type_s_h2_2([{{md, dash}, _},
             {{ws, _}, _} = WS | T])        -> {ul, make_list_str([WS | T])};
type_s_h2_2(_List)                          -> normal.

type_ws(List) ->
    case type_ws1(List) of
        blank         -> {blank, List};
        try_codeblock ->
            case type_ws2(List) of
                normal           -> {normal, List};
                {codeblock, Ret} -> {{codeblock, Ret}, List}
            end
    end.

type_ws1([])                  -> blank;
type_ws1([{{lf, _}, _} | []]) -> blank;
type_ws1([[] | T])            -> type_ws1(T);
type_ws1([{{ws, _}, _} | T])  -> type_ws1(T);
type_ws1(_L)                  -> try_codeblock.

%% 4 or more spaces takes you over the limit
%% (a tab is 4...)
type_ws2([{{ws, tab}, _} | T])  -> {codeblock, T};
type_ws2([{{ws, comp}, W} | T]) -> case gt(W, 4) of
                                           {true, R} -> {codeblock, [R| T]};
                                           false     -> normal
                                       end;
type_ws2([{{ws, sp}, _} | _T])  -> normal.

gt(String, Len) ->
    ExpString = re:replace(String, "\t", "    ", [{return, list}, unicode]),
    ExpStringLen = length(ExpString),
    if
        ExpStringLen >= Len -> WS = string:substr(ExpString, Len + 1,
                                                  ExpStringLen),
                               {true, {{ws, sp}, WS}};
        ExpStringLen <  Len -> false
    end.


%% if it is a list we need to discard the initial white space...
make_list_str([{{ws, _}, _} | T] = List) ->
    case is_double_indent(List) of
        false     -> T;
        {true, R} -> flatten([{tags, "<pre><code>"} ,R ,
                              {tags, "</code></pre>\n\n"} | []])
    end.

is_double_indent(List) -> is_double_indent1(List, 0).

%% double indent is any combination of tabs and spaces that add
%% up to 8
is_double_indent1([], _N)                  -> false;
is_double_indent1(Rest, N) when N > 7      -> {true, Rest};
is_double_indent1([{{ws, sp}, _} | T], N)  -> is_double_indent1(T, N + 1);
is_double_indent1([{{ws, tab}, _} | T], N) -> is_double_indent1(T, N + 4);
is_double_indent1(_List, _N)               -> false.

%% All ref processing can ignore the original values 'cos those
%% have already been captured at a higher level
snip_ref(List) ->
    case get_id(List) of
        {[{_, Id}], Rest} -> {_Rest2, Ref, Title} = parse_inline(Rest),
                             Ref2 = trim(Ref),
                             Rs = htmlencode(make_plain_str(Ref2)),
                             Ts = make_plain_str(Title),
                             {inlineref, {Id, {Rs, Ts}}};
        normal            -> normal
    end.

get_id(List) -> g_id1(List, []).

g_id1([], _Acc)                         -> normal;
g_id1([{{inline, close}, _},
       {{punc, colon}, _}, {{ws, _}, _}
       | T], Acc)                       -> {reverse(Acc), T};
g_id1([H | T], Acc)                     -> g_id1(T, [H | Acc]).

parse_inline(List) -> p_in1(List, []).

%% snip off the terminal linefeed (if there is one...)
p_in1([{{lf, _}, _} | []], A)            -> {[], reverse(A), []};
p_in1([], A)                             -> {[], reverse(A), []};
%% brackets can be escaped
p_in1([{{punc, bslash}, _},
       {bra, _} = B | T], A)             -> p_in1(T, [B | A]);
p_in1([{{punc, bslash}, _},
       {ket, _} = B | T], A)             -> p_in1(T, [B | A]);
p_in1([{{punc, bslash}, _},
       {{punc, doubleq}, _} = Q | T], A) -> p_in1(T, [Q | A]);
p_in1([{{punc, bslash}, _},
       {{punc, singleq}, _} = Q | T], A) -> p_in1(T, [Q | A]);
%% these clauses capture the start of the title...
p_in1([{{punc, doubleq}, _} | T], A)     -> p_in2(T, reverse(A), doubleq, []);
p_in1([{{punc, singleq}, _} | T], A)     -> p_in2(T, reverse(A), singleq, []);
p_in1([{bra, _} | T], A)            -> p_in2(T, reverse(A), brackets, []);
p_in1([{ket, _} | T], A)                 -> {T, reverse(A), []};
p_in1([H | T], A)                        -> p_in1(T, [H | A]).

%% this gets titles in single and double quotes
%% the delimiter type is passed in as 'D'
p_in2([], Url, _D, A)                              -> {[], Url, flatten(reverse(A))};
%% brackets can be escaped
p_in2([{{punc, bslash}, _},
       {bra, _} = B | T], Url, D, A)               -> p_in2(T, Url, D, [B | A]);
p_in2([{{punc, bslash}, _},
       {ket, _} = B | T], Url, D, A)               -> p_in2(T, Url, D, [B | A]);
%% quotes can be escaped
p_in2([{{punc, bslash}, _},
       {{punc, doubleq}, _}= Q | T], Url, D, A)    -> p_in2(T, Url, D, [Q | A]);
p_in2([{{punc, bslash}, _},
       {{punc, singleq}, _} = Q | T], Url, D, A)   -> p_in2(T, Url, D, [Q | A]);
%% these clauses capture the end of the title and drop the delimiter...
p_in2([{{punc, doubleq}, _} | T], Url, doubleq, A) -> p_in2(T, Url, none, A);
p_in2([{{punc, singleq}, _} | T], Url, singleq, A) -> p_in2(T, Url, none, A);
p_in2([{ket, _} | T], Url, brackets, A)            -> p_in2(T, Url, none, A);
%% terminator clause
p_in2([{ket, _} | T], Url, none, A)                -> {T, Url, flatten(reverse(A))};
%% this clause silently discards stuff after the delimiter...
p_in2([_H | T], Url, none, A)                      -> p_in2(T, Url, none, [A]);
p_in2([H | T], Url, D, A)                          -> p_in2(T, Url, D, [H | A]).

trim(String) -> trim_left(trim_right(String)).

trim_right(String) -> reverse(trim_left(reverse(String))).

trim_left([{{ws, _}, _} | T]) -> trim_left(T);
trim_left([[] | T])           -> trim_left(T);
trim_left(List)               -> List.


%% end of ref processing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Build the Lexed Token List
%%% This is a two part lexer, first it chunks the input and then on the second
%%% pass it gathers it into lines and types the lines
%%%
%%% NOTE that there are two different styles of processing lines:
%%% * markdown transformed
%%% * block
%%% inside block processing the whole text is dumped and just url encoded
%%% and the original text is always maintained during the lexing/parsing
%%% so that it can be recreated if the context requires it...
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(String) -> merge_ws(l1(String, [], [])).

merge_ws(List) -> m_ws1(List, []).

m_ws1([], Acc) -> reverse(Acc);
m_ws1([{{ws, _}, W1}, {{ws, _}, W2} | T], Acc) ->
    m_ws1([{{ws, comp}, W1 ++ W2} | T], Acc);
m_ws1([H | T], Acc) -> m_ws1(T, [H | Acc]).

%% this is the terminal head which ends the parsing...
l1([], [], A2)             -> flatten(reverse(A2));
l1([], A1, A2)             -> l1([], [], [l2(A1) | A2]);
%% these two heads capture opening and closing tags
l1([$<, $/|T], A1, A2)     -> {Tag, NewT} = closingdiv(T, []),
                              l1(NewT, [], [Tag, l2(A1) | A2]);
l1([$< | T], A1, A2)       -> {Tag, NewT} = openingdiv(T),
                              l1(NewT, [], [Tag , l2(A1) | A2]);
%% these clauses are the normal lexer clauses
l1([$= | T], A1, A2)       -> l1(T, [], [{{md, eq}, "="},   l2(A1) | A2]);
l1([$- | T], A1, A2)       -> l1(T, [], [{{md, dash}, "-"}, l2(A1) | A2]);
l1([$# | T], A1, A2)       -> l1(T, [], [{{md, atx}, "#"},  l2(A1) | A2]);
l1([$> | T], A1, A2)       -> l1(T, [], [{{md, gt}, ">"},   l2(A1) | A2]);
l1([$+ | T], A1, A2)       -> l1(T, [], [{{md, plus}, "+"}, l2(A1) | A2]);
l1([$* | T], A1, A2)       -> l1(T, [], [{{md, star}, "*"}, l2(A1) | A2]);
l1([$_ | T], A1, A2)       -> l1(T, [], [{{md, underscore}, "_"}, l2(A1) | A2]);
l1([$1 | T], A1, A2)       -> l1(T, [], [{num, "1"}, l2(A1) | A2]);
l1([$2 | T], A1, A2)       -> l1(T, [], [{num, "2"}, l2(A1) | A2]);
l1([$3 | T], A1, A2)       -> l1(T, [], [{num, "3"}, l2(A1) | A2]);
l1([$4 | T], A1, A2)       -> l1(T, [], [{num, "4"}, l2(A1) | A2]);
l1([$5 | T], A1, A2)       -> l1(T, [], [{num, "5"}, l2(A1) | A2]);
l1([$6 | T], A1, A2)       -> l1(T, [], [{num, "6"}, l2(A1) | A2]);
l1([$7 | T], A1, A2)       -> l1(T, [], [{num, "7"}, l2(A1) | A2]);
l1([$8 | T], A1, A2)       -> l1(T, [], [{num, "8"}, l2(A1) | A2]);
l1([$9 | T], A1, A2)       -> l1(T, [], [{num, "9"}, l2(A1) | A2]);
l1([$0 | T], A1, A2)       -> l1(T, [], [{num, "0"}, l2(A1) | A2]);
l1([$. | T], A1, A2)       -> l1(T, [], [{{punc, fullstop}, "."}, l2(A1) | A2]);
l1([$: | T], A1, A2)       -> l1(T, [], [{{punc, colon}, ":"}, l2(A1) | A2]);
l1([$' | T], A1, A2)       -> l1(T, [], [{{punc, singleq}, "'"}, l2(A1) | A2]); %'
l1([$" | T], A1, A2)       -> l1(T, [], [{{punc, doubleq}, "\""}, l2(A1) | A2]); %"
l1([$` | T], A1, A2)       -> l1(T, [], [{{punc, backtick}, "`"}, l2(A1) | A2]); %"
l1([$! | T], A1, A2)       -> l1(T, [], [{{punc, bang}, "!"}, l2(A1) | A2]); %"
l1([$\\ | T], A1, A2)      -> l1(T, [], [{{punc, bslash}, "\\"}, l2(A1) | A2]); %"
l1([$/ | T], A1, A2)       -> l1(T, [], [{{punc, fslash}, "/"}, l2(A1) | A2]); %"
l1([$( | T], A1, A2)       -> l1(T, [], [{bra, "("}, l2(A1) | A2]);
l1([$) | T], A1, A2)       -> l1(T, [], [{ket, ")"}, l2(A1) | A2]);
l1([$[ | T], A1, A2)       -> l1(T, [], [{{inline, open}, "["}, l2(A1) | A2]);
l1([$] | T], A1, A2)       -> l1(T, [], [{{inline, close}, "]"}, l2(A1) | A2]);
%% note there is a special 'whitespace' {{ws, none}, ""} which is used to generate non-space
%% filling whitespace for cases like '*bob* is great' which needs a non-space filling
%% whitespace prepended to trigger emphasis so it renders as "<em>bob</em> is great...
%% that 'character' doesn't exist so isn't in the lexer but appears in the parser
l1([?SPACE | T], A1, A2)   -> l1(T, [], [{{ws, sp}, " "}, l2(A1) | A2]);
l1([?TAB | T], A1, A2)     -> l1(T, [], [{{ws, tab}, "\t"}, l2(A1) | A2]);
l1([?NBSP | T], A1, A2)    -> l1(T, [], [{{ws, sp}, "&nbsp"}, l2(A1) | A2]);
l1([?CR, ?LF | T], A1, A2) -> l1(T, [], [{{lf, crlf}, [?CR , ?LF]}, l2(A1) | A2]);
l1([?LF | T], A1, A2)      -> l1(T, [], [{{lf, lf}, [?LF]}, l2(A1) | A2]);
%% l1([?CR | T], A1, A2)      -> l1(T, [], [{{lf, cr}, [?CR]}, l2(A1) | A2]);
%% this final clause accumulates line fragments
l1([H|T], A1, A2)          -> l1(T, [H |A1] , A2).

l2([])   -> [];
l2(List) -> {string, flatten(reverse(List))}.

%% need to put in regexes for urls and e-mail addies
openingdiv(String) ->
    case get_url(String) of
        {{url, URL}, R1} -> {{url, URL}, R1};
        not_url          ->
            case get_email_addie(String) of
                {{email, EM}, R2} -> {{email, EM}, R2};
                not_email         -> openingdiv1(String, [])
            end
    end.

% dumps out a list if it is not an opening div
openingdiv1([], Acc)         -> {flatten([{{punc, bra}, "<"}
                                          | lex(reverse(Acc))]), []};
openingdiv1([$/,$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                                Acc3 = string:to_lower(Acc2),
                                [Tag | _T] = string:tokens(Acc3, " "),
                                {{{{tag, self_closing}, Tag}, "<"
                                  ++ Acc2 ++ "/>"}, T};
%% special for non-tags
openingdiv1([$>| T], [])     -> {[{{punc, bra}, "<"},
                                          {{punc, ket}, ">"}], T};
openingdiv1([$>| T], Acc)    -> Acc2 = flatten(reverse(Acc)),
                                Acc3 = string:to_lower(Acc2),
                                [Tag | _T] = string:tokens(Acc3, " "),
                                {{{{tag, open}, Tag}, "<"
                                  ++ Acc2 ++ ">"}, T};
openingdiv1([H|T], Acc)      -> openingdiv1(T, [H | Acc]).

% dumps out a list if it is not an closing div
closingdiv([], Acc)     -> {flatten([{{punc, bra}, "<"},
                                     {{punc, fslash}, "/"}
                                     | lex(reverse(Acc))]), []};
closingdiv([$>| T], Acc) -> Acc2 = flatten(reverse(Acc)),
                            Acc3 = string:to_lower(Acc2),
                            [Tag | _T] = string:tokens(Acc3, " "),
                            {{{{tag, close}, Tag}, "</"
                              ++ Acc2 ++ ">"}, T};
closingdiv([H|T], Acc)   -> closingdiv(T, [H | Acc]).

get_url(String) -> HTTP_regex = "^(H|h)(T|t)(T|t)(P|p)(S|s)*://",
                   case re:run(String, HTTP_regex, [unicode]) of
                       nomatch    -> not_url;
                       {match, _} -> get_url1(String, [])
                   end.

get_url1([], Acc)            -> URL = flatten(reverse(Acc)),
                                {{url, URL}, []};
% allow escaped kets
get_url1([$\\, $> | T], Acc) -> get_url1(T, [$>, $\\ | Acc]);
get_url1([$> | T], Acc)      -> URL = flatten(reverse(Acc)),
                                {{url, URL}, T};
get_url1([H | T], Acc)       -> get_url1(T, [H | Acc]).

get_email_addie(String) ->
    Snip_regex = ">",
    case re:run(String, Snip_regex, [unicode]) of
        nomatch                -> not_email;
        {match, [{N, _} | _T]} ->
            {Possible, [$> | T]} = lists:split(N, String),
            EMail_regex = "[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+"
                ++ "(?:\.[a-zA-Z0-9!#$%&'*+/=?^_`{|}~-]+)*"
                ++ "@(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]*[a-zA-Z0-9])?\.)+"
                ++ "(?:[a-zA-Z]{2}|com|org|net|gov|mil"
                ++ "|biz|info|mobi|name|aero|jobs|museum)",
            case re:run(Possible, EMail_regex, [unicode]) of
                nomatch    -> not_email;
                {match, _} -> {{email, Possible}, T}
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Internal functions
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
make_plain_str(List) -> m_plain(List, []).

m_plain([], Acc)                           -> flatten(reverse(Acc));
m_plain([{{ws, none}, none} | T], Acc)     -> m_plain(T, [" " | Acc]);
m_plain([{_, Str} | T], Acc)               -> m_plain(T, [Str | Acc]).


%% convert ascii into html characters
htmlencode(List) ->
    htmlencode(List, []).

htmlencode([], Acc) ->
    lists:flatten(lists:reverse(Acc));

htmlencode([$&   | Rest], Acc) -> htmlencode(Rest, ["&amp;" | Acc]);
htmlencode([$<   | Rest], Acc) -> htmlencode(Rest, ["&lt;" | Acc]);
htmlencode([$>   | Rest], Acc) -> htmlencode(Rest, ["&gt;" | Acc]);
htmlencode([160  | Rest], Acc) -> htmlencode(Rest, ["&nbsp;" | Acc]);
htmlencode([Else | Rest], Acc) -> htmlencode(Rest, [Else | Acc]).

