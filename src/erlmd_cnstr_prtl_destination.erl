%%%-----------------------------------------------------------------------------
%%% @doc Partial destination construct - parses URL destinations.
%%%
%%% Two forms:
%%% 1. Enclosed: <https://example.com>
%%% 2. Raw: https://example.com (with balanced parentheses)
%%%
%%% The enclosed form is simple - anything until >.
%%% The raw form must track parenthesis nesting (max 32 levels).
%%%
%%% Examples:
%%%   <http://example.com>           - Enclosed
%%%   http://example.com             - Raw
%%%   http://example.com/path(1)     - Raw with balanced parens
%%%
%%% Reference: markdown-rs src/construct/partial_destination.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_prtl_destination).

-export([start/1, enclosed_before/1, enclosed/1, enclosed_escape/1,
         raw/1, raw_escape/1]).

-include("types.hrl").
-include("tokeniser.hrl").

-define(MAX_PAREN_DEPTH, 32).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Entry point for destination parsing.
start(T) ->
    case erlmd_tokeniser:current(T) of
        $< ->
            %% Enclosed destination
            Token1 = erlmd_tokeniser:get_token_1(T),
            Token2 = erlmd_tokeniser:get_token_2(T),
            Token3 = erlmd_tokeniser:get_token_3(T),

            T1 = erlmd_tokeniser:enter(T, Token1),
            T2 = erlmd_tokeniser:enter(T1, Token2),
            T3 = erlmd_tokeniser:enter(T2, Token3),
            T4 = erlmd_tokeniser:consume(T3),
            T5 = erlmd_tokeniser:exit(T4, Token3),
            {{next, prtl_destination_enclosed_before}, T5};

        C when C >= 16#01 andalso C =< 16#1F;
               C =:= $\s; C =:= $); C =:= 16#7F ->
            %% ASCII control, space, or closing paren - not allowed
            {nok, T};

        eof ->
            {nok, T};

        _ ->
            %% Raw destination
            Token1 = erlmd_tokeniser:get_token_1(T),
            Token4 = erlmd_tokeniser:get_token_4(T),
            Token5 = erlmd_tokeniser:get_token_5(T),

            T1 = erlmd_tokeniser:enter(T, Token1),
            T2 = erlmd_tokeniser:enter(T1, Token4),
            T3 = erlmd_tokeniser:enter(T2, Token5),
            T4 = erlmd_tokeniser:set_paren_depth(T3, 0),
            {{retry, prtl_destination_raw}, T4}
    end.

%% @doc Before enclosed destination content.
enclosed_before(T) ->
    case erlmd_tokeniser:current(T) of
        $< ->
            %% Nested < not allowed
            {nok, T};
        _ ->
            Token5 = erlmd_tokeniser:get_token_5(T),
            T1 = erlmd_tokeniser:enter(T, Token5),
            {{retry, prtl_destination_enclosed}, T1}
    end.

%% @doc Inside enclosed destination.
enclosed(T) ->
    case erlmd_tokeniser:current(T) of
        $> ->
            %% End of enclosed destination
            Token5 = erlmd_tokeniser:get_token_5(T),
            Token3 = erlmd_tokeniser:get_token_3(T),
            Token2 = erlmd_tokeniser:get_token_2(T),
            Token1 = erlmd_tokeniser:get_token_1(T),

            T1 = erlmd_tokeniser:exit(T, Token5),
            T2 = erlmd_tokeniser:enter(T1, Token3),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, Token3),
            T5 = erlmd_tokeniser:exit(T4, Token2),
            T6 = erlmd_tokeniser:exit(T5, Token1),
            {ok, T6};
        $< ->
            %% Nested < not allowed
            {nok, T};
        $\n ->
            %% Line ending not allowed in enclosed
            {nok, T};
        $\\ ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, prtl_destination_enclosed_escape}, T1};
        eof ->
            {nok, T};
        _ ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, prtl_destination_enclosed}, T1}
    end.

%% @doc Handle escape in enclosed destination.
enclosed_escape(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $<; C =:= $>; C =:= $\\ ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, prtl_destination_enclosed}, T1};
        _ ->
            {{retry, prtl_destination_enclosed}, T}
    end.

%% @doc Parse raw destination with balanced parentheses.
raw(T) ->
    Depth = erlmd_tokeniser:get_paren_depth(T),
    Curr = erlmd_tokeniser:current(T),
    io:format("      [dest raw] depth=~p curr=~p~n", [Depth, Curr]),

    if
        Depth > ?MAX_PAREN_DEPTH ->
            %% Too deep!
            io:format("      [dest raw] Too deep!~n"),
            {nok, T};
        Depth =:= 0 ->
            %% At depth 0, check for end conditions
            case Curr of
                C when C =:= $\t; C =:= $\n; C =:= $\s; C =:= $); C =:= eof ->
                    %% End of destination
                    io:format("      [dest raw] Found terminator ~p, returning OK~n", [C]),
                    Token5 = erlmd_tokeniser:get_token_5(T),
                    Token4 = erlmd_tokeniser:get_token_4(T),
                    Token1 = erlmd_tokeniser:get_token_1(T),

                    T1 = erlmd_tokeniser:exit(T, Token5),
                    T2 = erlmd_tokeniser:exit(T1, Token4),
                    T3 = erlmd_tokeniser:exit(T2, Token1),
                    T4 = erlmd_tokeniser:set_paren_depth(T3, 0),
                    {ok, T4};
                $( ->
                    %% Opening paren - increase depth
                    T1 = erlmd_tokeniser:consume(T),
                    T2 = erlmd_tokeniser:inc_paren_depth(T1),
                    {{next, prtl_destination_raw}, T2};
                $\\ ->
                    T1 = erlmd_tokeniser:consume(T),
                    {{next, prtl_destination_raw_escape}, T1};
                C when C >= 16#01 andalso C =< 16#1F; C =:= 16#7F ->
                    %% ASCII control not allowed
                    {nok, T};
                _ ->
                    T1 = erlmd_tokeniser:consume(T),
                    {{next, prtl_destination_raw}, T1}
            end;
        true ->
            %% Have open parens, continue
            case erlmd_tokeniser:current(T) of
                $( ->
                    T1 = erlmd_tokeniser:consume(T),
                    T2 = erlmd_tokeniser:inc_paren_depth(T1),
                    {{next, prtl_destination_raw}, T2};
                $) ->
                    T1 = erlmd_tokeniser:consume(T),
                    T2 = erlmd_tokeniser:dec_paren_depth(T1),
                    {{next, prtl_destination_raw}, T2};
                $\\ ->
                    T1 = erlmd_tokeniser:consume(T),
                    {{next, prtl_destination_raw_escape}, T1};
                C when C >= 16#01 andalso C =< 16#1F; C =:= 16#7F ->
                    {nok, T};
                C when C =:= $\t; C =:= $\n; C =:= $\s; C =:= eof ->
                    %% Unexpected end while parens are open
                    {nok, T};
                _ ->
                    T1 = erlmd_tokeniser:consume(T),
                    {{next, prtl_destination_raw}, T1}
            end
    end.

%% @doc Handle escape in raw destination.
raw_escape(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $(; C =:= $); C =:= $\\ ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, prtl_destination_raw}, T1};
        _ ->
            {{retry, prtl_destination_raw}, T}
    end.
