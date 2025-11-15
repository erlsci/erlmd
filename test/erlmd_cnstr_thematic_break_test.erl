%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for thematic break construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_thematic_break_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_thematic_break(Input) ->
    T = test_helper:make_tokenizer(Input),
    %% Call start directly, then run through state machine if it returns {next/retry, ...}
    case erlmd_cnstr_thematic_break:start(T) of
        {{retry, StateName}, T1} ->
            test_helper:run_construct(StateName, T1);
        {{next, StateName}, T1} ->
            test_helper:run_construct(StateName, T1);
        Result ->
            Result
    end.

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% Test 1: Three asterisks
three_asterisks_test() ->
    {Result, T1} = parse_thematic_break(<<"***">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should succeed
    ?assertEqual(ok, Result),

    %% Should have thematic_break events
    BreakEvents = [E || E <- Events, E#event.name =:= thematic_break],
    ?assertEqual(2, length(BreakEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(BreakEvents))#event.kind).

%% Test 2: Three hyphens
three_hyphens_test() ->
    {Result, _T1} = parse_thematic_break(<<"---">>),
    ?assertEqual(ok, Result).

%% Test 3: Three underscores
three_underscores_test() ->
    {Result, _T1} = parse_thematic_break(<<"___">>),
    ?assertEqual(ok, Result).

%% Test 4: More than three markers
many_markers_test() ->
    {Result, _T1} = parse_thematic_break(<<"*****">>),
    ?assertEqual(ok, Result).

%% Test 5: With spaces between markers
with_spaces_test() ->
    {Result, _T1} = parse_thematic_break(<<"* * *">>),
    ?assertEqual(ok, Result).

%% Test 6: With tabs between markers
with_tabs_test() ->
    {Result, _T1} = parse_thematic_break(<<"*\t*\t*">>),
    ?assertEqual(ok, Result).

%% Test 7: Only two markers (should fail)
two_markers_test() ->
    {Result, _T1} = parse_thematic_break(<<"**">>),
    ?assertEqual(nok, Result).

%% Test 8: Mixed markers (should fail)
mixed_markers_test() ->
    {Result, _T1} = parse_thematic_break(<<"*-*">>),
    ?assertEqual(nok, Result).

%% Test 9: Wrong character (should fail)
wrong_character_test() ->
    {Result, _T1} = parse_thematic_break(<<"###">>),
    ?assertEqual(nok, Result).

%% Test 10: Text after markers (should fail)
text_after_test() ->
    {Result, _T1} = parse_thematic_break(<<"*** text">>),
    ?assertEqual(nok, Result).

%% Test 11: Spaces before and after
spaces_around_test() ->
    {Result, _T1} = parse_thematic_break(<<" - - - ">>),
    ?assertEqual(ok, Result).

%% Test 12: At EOF (no newline)
at_eof_test() ->
    {Result, _T1} = parse_thematic_break(<<"***">>),
    ?assertEqual(ok, Result).

%% Test 13: With newline
with_newline_test() ->
    {Result, T1} = parse_thematic_break(<<"***\n">>),
    ?assertEqual(ok, Result),

    %% Should have consumed up to (but not including) the newline
    ?assertEqual($\n, erlmd_tokenizer:current(T1)).
