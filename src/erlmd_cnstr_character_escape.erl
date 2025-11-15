%%%-----------------------------------------------------------------------------
%%% @doc Character escape construct.
%%%
%%% Handles backslash-escaped ASCII punctuation characters.
%%% Implements CommonMark 2.4 "Backslash escapes".
%%%
%%% A backslash before any ASCII punctuation character causes that character
%%% to be treated as a literal character rather than a Markdown construct.
%%%
%%% ASCII punctuation: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_cnstr_character_escape).

-export([start/1, inside/1]).

-include("types.hrl").

%%%=============================================================================
%%% State Functions
%%%=============================================================================

%% @doc Entry point - checks for backslash
start(T) ->
    case erlmd_tokeniser:current(T) of
        $\\ ->
            T1 = erlmd_tokeniser:enter(T, character_escape),
            T2 = erlmd_tokeniser:enter(T1, character_escape_marker),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, character_escape_marker),
            {{next, character_escape_inside}, T4};
        _ ->
            {nok, T}
    end.

%% @doc After backslash - checks for ASCII punctuation
inside(T) ->
    case erlmd_tokeniser:current(T) of
        Byte when is_integer(Byte) ->
            case is_ascii_punctuation(Byte) of
                true ->
                    T1 = erlmd_tokeniser:enter(T, character_escape_value),
                    T2 = erlmd_tokeniser:consume(T1),
                    T3 = erlmd_tokeniser:exit(T2, character_escape_value),
                    T4 = erlmd_tokeniser:exit(T3, character_escape),
                    {ok, T4};
                false ->
                    {nok, T}
            end;
        eof ->
            {nok, T}
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% @doc Check if byte is ASCII punctuation character
%%
%% ASCII punctuation characters:
%% ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
%%
%% These fall into 4 ranges:
%% - 33-47:  ! " # $ % & ' ( ) * + , - . /
%% - 58-64:  : ; < = > ? @
%% - 91-96:  [ \ ] ^ _ `
%% - 123-126: { | } ~
is_ascii_punctuation(Byte) ->
    (Byte >= $! andalso Byte =< $/) orelse    % 33-47
    (Byte >= $: andalso Byte =< $@) orelse    % 58-64
    (Byte >= $[ andalso Byte =< $`) orelse    % 91-96
    (Byte >= ${ andalso Byte =< $~).          % 123-126
