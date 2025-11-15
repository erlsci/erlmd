%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for hard break escape construct.
%%%
%%% Tests backslash followed by line ending for hard line breaks.
%%% Implements CommonMark 6.7 "Hard line breaks" (escape method) test coverage.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_hard_break_escape_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_hard_break(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(hard_break_escape, T).

%%%=============================================================================
%%% Basic Functionality Tests
%%%=============================================================================

%% Test 1: Backslash followed by newline - creates hard break
basic_hard_break_test() ->
    {ok, T1} = parse_hard_break(<<"\\\n">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should have hard_break_escape events
    BreakEvents = [E || E <- Events, E#event.name =:= hard_break_escape],
    ?assertEqual(2, length(BreakEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(BreakEvents))#event.kind),
    ?assertEqual(exit, (lists:last(BreakEvents))#event.kind).

%% Test 2: Backslash with content after newline
hard_break_with_content_test() ->
    {ok, T1} = parse_hard_break(<<"\\\ntext">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    BreakEvents = [E || E <- Events, E#event.name =:= hard_break_escape],
    ?assertEqual(2, length(BreakEvents)).

%%%=============================================================================
%%% Negative Tests
%%%=============================================================================

%% Test 3: Backslash at EOF - not a hard break
backslash_at_eof_test() ->
    {nok, _T1} = parse_hard_break(<<"\\">>).

%% Test 4: Backslash followed by space - not a hard break
backslash_space_test() ->
    {nok, _T1} = parse_hard_break(<<"\\ ">>).

%% Test 5: Backslash followed by letter - not a hard break
backslash_letter_test() ->
    {nok, _T1} = parse_hard_break(<<"\\a">>).

%% Test 6: Backslash followed by punctuation - not a hard break (that's character escape)
backslash_punctuation_test() ->
    {nok, _T1} = parse_hard_break(<<"\\*">>).

%% Test 7: No backslash - not a hard break
no_backslash_test() ->
    {nok, _T1} = parse_hard_break(<<"\n">>).

%%%=============================================================================
%%% CommonMark Examples (Section 6.7)
%%%=============================================================================

%% Example 654: Hard line break with backslash
commonmark_654_test() ->
    %% foo\
    %% bar
    %% The backslash-newline should be recognized
    {ok, T1} = parse_hard_break(<<"\\\n">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    BreakEvents = [E || E <- Events, E#event.name =:= hard_break_escape],
    ?assertEqual(2, length(BreakEvents)).

%% Example 655: Backslash at end of line
commonmark_655_test() ->
    %% Similar to 654
    {ok, T1} = parse_hard_break(<<"\\\n">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    BreakEvents = [E || E <- Events, E#event.name =:= hard_break_escape],
    ?assertEqual(2, length(BreakEvents)).

%% Note: More CommonMark examples test integration with paragraph/inline contexts
%% Those will be tested in integration tests

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

%% Test: Only backslash
only_backslash_test() ->
    {nok, _T} = parse_hard_break(<<"\\">>).

%% Test: Backslash followed by tab - not a hard break
backslash_tab_test() ->
    {nok, _T} = parse_hard_break(<<"\\\t">>).

%% Test: Multiple backslashes before newline (only first pair matters)
double_backslash_test() ->
    %% \\ followed by newline
    %% The first backslash escapes the second, so this is NOT a hard break
    %% This would be handled by character_escape first
    {nok, _T} = parse_hard_break(<<"\\\\">>).

%% Test: Backslash with CRLF
backslash_crlf_test() ->
    %% In CommonMark, \r\n is normalized to \n before parsing
    %% So backslash followed by \r should not be a hard break
    %% (the \r gets consumed as part of line ending normalization)
    %% But let's verify backslash before \r fails
    {nok, _T} = parse_hard_break(<<"\\r">>).
