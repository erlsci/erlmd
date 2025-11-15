%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for character escape construct.
%%%
%%% Tests backslash-escaped ASCII punctuation characters.
%%% Implements CommonMark 2.4 "Backslash escapes" test coverage.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_character_escape_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_escape(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(character_escape, T).

%%%=============================================================================
%%% Basic Functionality Tests
%%%=============================================================================

%% Test 1: Escape asterisk
escape_asterisk_test() ->
    {ok, T1} = parse_escape(<<"\\*">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should have character_escape events
    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assertEqual(2, length(EscapeEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(EscapeEvents))#event.kind),
    ?assertEqual(exit, (lists:last(EscapeEvents))#event.kind),

    %% Should have marker and value
    MarkerEvents = [E || E <- Events, E#event.name =:= character_escape_marker],
    ?assertEqual(2, length(MarkerEvents)),
    ValueEvents = [E || E <- Events, E#event.name =:= character_escape_value],
    ?assertEqual(2, length(ValueEvents)).

%% Test 2: Escape backslash
escape_backslash_test() ->
    {ok, T1} = parse_escape(<<"\\\\">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assertEqual(2, length(EscapeEvents)).

%% Test 3: Escape brackets
escape_brackets_test() ->
    {ok, T1} = parse_escape(<<"\\[">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assertEqual(2, length(EscapeEvents)).

%% Test 4: Escape parentheses
escape_parentheses_test() ->
    {ok, T1} = parse_escape(<<"\\(">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assertEqual(2, length(EscapeEvents)).

%%%=============================================================================
%%% All ASCII Punctuation Tests
%%%=============================================================================

%% Test 5: All ASCII punctuation characters can be escaped
all_punctuation_test() ->
    %% ASCII punctuation: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
    Punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~",
    lists:foreach(fun(Char) ->
        Input = list_to_binary([$\\, Char]),
        {ok, T} = parse_escape(Input),
        Events = lists:reverse(erlmd_tokenizer:get_events(T)),
        EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
        ?assertEqual(2, length(EscapeEvents))
    end, Punctuation).

%%%=============================================================================
%%% Negative Tests
%%%=============================================================================

%% Test 6: Backslash followed by letter - not an escape
not_escape_letter_test() ->
    {nok, _T1} = parse_escape(<<"\\a">>).

%% Test 7: Backslash followed by digit - not an escape
not_escape_digit_test() ->
    {nok, _T1} = parse_escape(<<"\\1">>).

%% Test 8: Backslash followed by space - not an escape
not_escape_space_test() ->
    {nok, _T1} = parse_escape(<<"\\ ">>).

%% Test 9: Backslash at EOF - not an escape
not_escape_eof_test() ->
    {nok, _T1} = parse_escape(<<"\\">>).

%% Test 10: No backslash - not an escape
no_backslash_test() ->
    {nok, _T1} = parse_escape(<<"*">>).

%%%=============================================================================
%%% CommonMark Examples (Section 2.4)
%%%=============================================================================

%% Example 298: Backslash escapes
commonmark_298_test() ->
    %% \!\"\#\$\%\&\'\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~
    %% Each of these should be escapable
    Punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~",
    lists:foreach(fun(Char) ->
        Input = list_to_binary([$\\, Char]),
        {ok, _T} = parse_escape(Input)
    end, Punctuation).

%% Example 299: Backslashes before other characters are literal
commonmark_299_test() ->
    %% \→ \A \a \ \3 \φ \«
    %% These should fail (return nok)
    Inputs = [
        <<"\\a">>,  % Letter
        <<"\\A">>,  % Capital letter
        <<"\\3">>,  % Digit
        <<"\\ ">>   % Space
    ],
    lists:foreach(fun(Input) ->
        {nok, _T} = parse_escape(Input)
    end, Inputs).

%% Example 300: Escaped characters are treated as regular characters
%% (This would be tested in integration, just verify escape works)
commonmark_300_test() ->
    {ok, T1} = parse_escape(<<"\\*">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assertEqual(2, length(EscapeEvents)).

%% Example 301: Backslash at end of line
commonmark_301_test() ->
    %% This is actually a hard break escape, not character escape
    %% So character_escape should return nok
    {nok, _T1} = parse_escape(<<"\\\n">>).

%% Example 302: Backslash before newline in code blocks
%% (Not relevant to character_escape directly - integration test)

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

%% Test: Only backslash
only_backslash_test() ->
    {nok, _T} = parse_escape(<<"\\">>).

%% Test: Backslash with newline (should fail - this is hard_break_escape)
backslash_newline_test() ->
    {nok, _T} = parse_escape(<<"\\\n">>).

%% Test: Backslash with tab (not punctuation)
backslash_tab_test() ->
    {nok, _T} = parse_escape(<<"\\\t">>).

%% Test: Multiple escapes in sequence (only test first one)
first_escape_only_test() ->
    %% \\* should parse as one escape (\\), leaving * unconsumed
    {ok, T1} = parse_escape(<<"\\\\">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    EscapeEvents = [E || E <- Events, E#event.name =:= character_escape],
    ?assertEqual(2, length(EscapeEvents)).
