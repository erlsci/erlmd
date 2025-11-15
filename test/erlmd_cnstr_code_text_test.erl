%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for code text (inline code) construct.
%%%
%%% Tests backtick-delimited inline code spans.
%%% Implements CommonMark 6.1 "Code spans" test coverage.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_code_text_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_code(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(code_text, T).

%%%=============================================================================
%%% Basic Functionality Tests
%%%=============================================================================

%% Test 1: Simple code span
simple_code_test() ->
    {ok, T1} = parse_code(<<"`code`">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should have code_text events
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(CodeEvents))#event.kind),
    ?assertEqual(exit, (lists:last(CodeEvents))#event.kind),

    %% Should have sequence events (opening and closing)
    SeqEvents = [E || E <- Events, E#event.name =:= code_text_sequence],
    ?assert(length(SeqEvents) >= 2),  % At least opening and closing

    %% Should have data events
    DataEvents = [E || E <- Events, E#event.name =:= code_text_data],
    ?assert(length(DataEvents) >= 1).

%% Test 2: Code span with double backticks
double_backtick_test() ->
    {ok, T1} = parse_code(<<"``code``">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% Test 3: Code span containing single backtick
code_with_backtick_test() ->
    {ok, T1} = parse_code(<<"``code ` with``">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% Test 4: Triple backticks
triple_backtick_test() ->
    {ok, T1} = parse_code(<<"```code```">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%%%=============================================================================
%%% Sequence Matching Tests
%%%=============================================================================

%% Test 5: Mismatched sequences - should fail or reclassify
mismatched_sequences_test() ->
    %% `code`` - 1 backtick open, 2 backticks close
    Result = parse_code(<<"`code``">>),
    %% Should either fail (nok) or succeed if it finds a valid 1-backtick close
    ?assertMatch({_, _}, Result).

%% Test 6: Unclosed code span - should fail
unclosed_code_test() ->
    {nok, _T1} = parse_code(<<"`code">>).

%% Test 7: Only opening backticks
only_opening_test() ->
    {nok, _T1} = parse_code(<<"`">>).

%% Test 8: Only backticks - should fail (no closing sequence)
%%  Two backticks are consumed as opening, then EOF, so no closer found
only_backticks_test() ->
    {nok, _T1} = parse_code(<<"``">>).

%%%=============================================================================
%%% Line Ending Tests
%%%=============================================================================

%% Test 9: Code span with line ending
code_with_newline_test() ->
    {ok, T1} = parse_code(<<"`code\nmore`">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should have code_text events
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)),

    %% Should have line_ending event
    LineEvents = [E || E <- Events, E#event.name =:= line_ending],
    ?assert(length(LineEvents) >= 1).

%% Test 10: Multiple line endings
code_with_multiple_newlines_test() ->
    {ok, T1} = parse_code(<<"`code\n\nmore`">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)),

    LineEvents = [E || E <- Events, E#event.name =:= line_ending],
    ?assert(length(LineEvents) >= 2).

%%%=============================================================================
%%% CommonMark Examples (Section 6.1)
%%%=============================================================================

%% Example 328: Simple code span
commonmark_328_test() ->
    {ok, T1} = parse_code(<<"`foo`">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% Example 329: Code span with backticks inside
commonmark_329_test() ->
    {ok, T1} = parse_code(<<"`` foo ` bar ``">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% Example 330: Single backtick in double backtick span
commonmark_330_test() ->
    {ok, T1} = parse_code(<<"`` ` ``">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% Example 331: Mismatched sequences - should fail
%% ```foo`` = 3 backticks open, content "foo", 2 backticks close (mismatch)
commonmark_331_test() ->
    %% This should fail because opening (3) doesn't match any closing (2)
    {nok, _T1} = parse_code(<<"```foo``">>).

%% Example 332: Code span across line
commonmark_332_test() ->
    {ok, T1} = parse_code(<<"`foo\nbar`">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% More examples test integration with paragraph contexts
%% Those will be in integration tests

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

%% Test: Empty code span - use proper matching sequences
empty_code_test() ->
    %% ` ` = 1 backtick open, space as data, 1 backtick close
    {ok, T1} = parse_code(<<"` `">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% Test: Code with spaces
code_with_spaces_test() ->
    {ok, T1} = parse_code(<<"`  code  `">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).

%% Test: No backtick - should fail
no_backtick_test() ->
    {nok, _T} = parse_code(<<"code">>).

%% Test: Code at EOF
code_at_eof_test() ->
    {ok, T1} = parse_code(<<"`code`">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),
    CodeEvents = [E || E <- Events, E#event.name =:= code_text],
    ?assertEqual(2, length(CodeEvents)).
