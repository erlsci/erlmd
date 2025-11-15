%%%-----------------------------------------------------------------------------
%%% @doc Hard break escape construct.
%%%
%%% Handles backslash followed by line ending to create hard line breaks.
%%% Implements CommonMark 6.7 "Hard line breaks" (escape method).
%%%
%%% A backslash before a line ending creates a hard break, which forces
%%% a line break in the rendered output.
%%%
%%% Note: This is different from character_escape, which handles backslash
%%% before ASCII punctuation.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_cnstr_hard_break_escape).

-export([start/1, after_escape/1]).

-include("types.hrl").

%%%=============================================================================
%%% State Functions
%%%=============================================================================

%% @doc Entry point - checks for backslash
start(T) ->
    case erlmd_tokeniser:current(T) of
        $\\ ->
            T1 = erlmd_tokeniser:enter(T, hard_break_escape),
            T2 = erlmd_tokeniser:consume(T1),
            {{next, hard_break_escape_after}, T2};
        _ ->
            {nok, T}
    end.

%% @doc After backslash - checks for line ending
after_escape(T) ->
    case erlmd_tokeniser:current(T) of
        $\n ->
            %% Line ending after backslash - this is a hard break
            T1 = erlmd_tokeniser:exit(T, hard_break_escape),
            {ok, T1};
        eof ->
            %% Backslash at EOF - not a hard break
            {nok, T};
        _ ->
            %% Any other character - not a hard break
            {nok, T}
    end.
