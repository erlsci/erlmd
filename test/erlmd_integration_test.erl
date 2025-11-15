%%%-----------------------------------------------------------------------------
%%% @doc Integration tests for Phase 3 simple constructs.
%%%
%%% Tests the constructs through the full tokenizer feed loop.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_integration_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

parse_simple_data_test() ->
    T0 = test_helper:make_tokenizer(<<"Hello">>),
    T1 = erlmd_tokeniser:set_markers(T0, []),
    {ok, T2} = test_helper:run_construct(data_start, T1),

    Events = erlmd_tokeniser:get_events(T2),

    %% Should have data enter and exit events
    DataCount = test_helper:count_events(Events, data),
    ?assertEqual(2, DataCount).

parse_data_with_newline_test() ->
    T0 = test_helper:make_tokenizer(<<"Hello\nWorld">>),
    T1 = erlmd_tokeniser:set_markers(T0, []),
    {ok, T2} = test_helper:run_construct(data_start, T1),

    Events = erlmd_tokeniser:get_events(T2),

    %% Should have 2 data chunks and 2 line endings
    DataCount = test_helper:count_events(Events, data),
    LineCount = test_helper:count_events(Events, line_ending),
    ?assertEqual(4, DataCount),  % 2 chunks * 2 events each
    ?assertEqual(2, LineCount).  % 1 line ending * 2 events

parse_empty_input_test() ->
    T0 = test_helper:make_tokenizer(<<>>),
    T1 = erlmd_tokeniser:set_markers(T0, []),
    {ok, T2} = test_helper:run_construct(data_start, T1),

    Events = erlmd_tokeniser:get_events(T2),

    %% Should have no events
    DataCount = test_helper:count_events(Events, data),
    ?assertEqual(0, DataCount).

parse_whitespace_test() ->
    T0 = test_helper:make_tokenizer(<<"   ">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T0),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    %% Should have consumed the whitespace
    ?assertEqual(eof, erlmd_tokeniser:current(T2)).

blank_line_detection_test() ->
    T0 = test_helper:make_tokenizer(<<"   \n">>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T0),

    %% Should be at the newline
    ?assertEqual($\n, erlmd_tokeniser:current(T1)).
