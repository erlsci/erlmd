%%%-----------------------------------------------------------------------------
%%% @doc Partial title construct - parses optional titles in resources.
%%%
%%% Three forms:
%%% 1. Double quoted: "title"
%%% 2. Single quoted: 'title'
%%% 3. Parenthesized: (title)
%%%
%%% Titles can contain line endings, which are converted to spaces.
%%% They can contain escape sequences.
%%%
%%% Examples:
%%%   "my title"           - Double quoted
%%%   'my title'           - Single quoted
%%%   (my title)           - Parenthesized
%%%   "multi\nline"        - Title with line ending
%%%
%%% Reference: markdown-rs src/construct/partial_title.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_prtl_title).

-export([start/1, title_begin/1, title_at_break/1, title_inside/1,
         title_escape/1, title_after_eol/1, title_nok/1]).

-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Entry point for title parsing.
start(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $"; C =:= $'; C =:= $( ->
            %% Store the marker (convert ( to ))
            Marker = if C =:= $( -> $); true -> C end,
            T1 = erlmd_tokeniser:set_marker(T, Marker),

            Token1 = erlmd_tokeniser:get_token_1(T),
            Token2 = erlmd_tokeniser:get_token_2(T),

            T2 = erlmd_tokeniser:enter(T1, Token1),
            T3 = erlmd_tokeniser:enter(T2, Token2),
            T4 = erlmd_tokeniser:consume(T3),
            T5 = erlmd_tokeniser:exit(T4, Token2),
            {{next, prtl_title_begin}, T5};
        _ ->
            {nok, T}
    end.

%% @doc Begin title content.
title_begin(T) ->
    Marker = erlmd_tokeniser:get_marker(T),

    case erlmd_tokeniser:current(T) of
        M when M =:= Marker ->
            %% Empty title is ok
            Token2 = erlmd_tokeniser:get_token_2(T),
            Token1 = erlmd_tokeniser:get_token_1(T),

            T1 = erlmd_tokeniser:enter(T, Token2),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, Token2),
            T4 = erlmd_tokeniser:exit(T3, Token1),
            T5 = erlmd_tokeniser:set_marker(T4, 0),
            {ok, T5};
        _ ->
            Token3 = erlmd_tokeniser:get_token_3(T),
            T1 = erlmd_tokeniser:enter(T, Token3),
            {{retry, prtl_title_at_break}, T1}
    end.

%% @doc At a break point in title.
title_at_break(T) ->
    Marker = erlmd_tokeniser:get_marker(T),

    case erlmd_tokeniser:current(T) of
        M when M =:= Marker ->
            %% Found closing marker
            Token3 = erlmd_tokeniser:get_token_3(T),
            Token2 = erlmd_tokeniser:get_token_2(T),
            Token1 = erlmd_tokeniser:get_token_1(T),

            T1 = erlmd_tokeniser:exit(T, Token3),
            T2 = erlmd_tokeniser:enter(T1, Token2),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, Token2),
            T5 = erlmd_tokeniser:exit(T4, Token1),
            T6 = erlmd_tokeniser:set_marker(T5, 0),
            {ok, T6};
        $\n ->
            %% Line ending - attempt to consume whitespace
            T1 = erlmd_tokeniser:attempt(T, {next, prtl_title_after_eol},
                                            {next, prtl_title_after_eol}),
            {{retry, space_or_tab_eol}, T1};
        eof ->
            %% Unexpected EOF
            {{retry, prtl_title_nok}, T};
        _ ->
            %% Continue with content
            {{retry, prtl_title_inside}, T}
    end.

%% @doc Parse title content.
title_inside(T) ->
    Marker = erlmd_tokeniser:get_marker(T),

    case erlmd_tokeniser:current(T) of
        M when M =:= Marker ->
            {{retry, prtl_title_at_break}, T};
        $\n ->
            {{retry, prtl_title_at_break}, T};
        $\\ ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, prtl_title_escape}, T1};
        eof ->
            {{retry, prtl_title_at_break}, T};
        _ ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, prtl_title_inside}, T1}
    end.

%% @doc Handle escape sequence in title.
title_escape(T) ->
    Marker = erlmd_tokeniser:get_marker(T),

    case erlmd_tokeniser:current(T) of
        M when M =:= Marker; M =:= $\\ ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, prtl_title_inside}, T1};
        _ ->
            {{retry, prtl_title_inside}, T}
    end.

%% @doc After consuming EOL whitespace in title.
title_after_eol(T) ->
    {{retry, prtl_title_at_break}, T}.

%% @doc Title parsing failed.
title_nok(T) ->
    T1 = erlmd_tokeniser:set_marker(T, 0),
    {nok, T1}.
