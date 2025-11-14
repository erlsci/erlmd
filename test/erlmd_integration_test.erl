%%%-----------------------------------------------------------------------------
%%% @doc Integration tests for Phase 3 simple constructs.
%%%
%%% Tests the constructs through the full tokenizer feed loop.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_integration_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

parse_simple_data_test() ->
    Input = <<"Hello">>,
    T0 = test_helper:make_tokenizer(Input),
    T1 = erlmd_tokenizer:set_markers(T0, []),

    %% Feed through data_start state
    {ok, T2} = erlmd_tokenizer:feed(T1, data_start, Input),

    %% Check events
    {ok, Events} = erlmd_tokenizer:finalize(T2),

    %% Should have data enter and exit events
    DataCount = test_helper:count_events(Events, data),
    ?assertEqual(2, DataCount).

parse_data_with_newline_test() ->
    Input = <<"Hello\nWorld">>,
    T0 = test_helper:make_tokenizer(Input),
    T1 = erlmd_tokenizer:set_markers(T0, []),

    %% Feed through data_start state
    {ok, T2} = erlmd_tokenizer:feed(T1, data_start, Input),

    %% Check events
    {ok, Events} = erlmd_tokenizer:finalize(T2),

    %% Should have 2 data chunks and 2 line endings
    DataCount = test_helper:count_events(Events, data),
    LineCount = test_helper:count_events(Events, line_ending),
    ?assertEqual(4, DataCount),  % 2 chunks * 2 events each
    ?assertEqual(2, LineCount).  % 1 line ending * 2 events

parse_empty_input_test() ->
    Input = <<>>,
    T0 = test_helper:make_tokenizer(Input),
    T1 = erlmd_tokenizer:set_markers(T0, []),

    %% Feed through data_start state
    {ok, T2} = erlmd_tokenizer:feed(T1, data_start, Input),

    %% Check events
    {ok, Events} = erlmd_tokenizer:finalize(T2),

    %% Should have no events
    ?assertEqual([], Events).

parse_whitespace_test() ->
    Input = <<"   ">>,
    T0 = test_helper:make_tokenizer(Input),
    T1 = erlmd_tokenizer:prepare_byte(T0),

    %% Feed through space_or_tab_start using space_or_tab helper
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1),

    %% Should have consumed the whitespace
    ?assertEqual(eof, erlmd_tokenizer:current(T2)).

blank_line_detection_test() ->
    Input = <<"   \n">>,
    T0 = test_helper:make_tokenizer(Input),
    T1 = erlmd_tokenizer:prepare_byte(T0),

    %% Test blank line detection
    {ok, T2} = erlmd_construct_blank_line:start(T1),

    %% Should be at the newline
    ?assertEqual($\n, erlmd_tokenizer:current(T2)).
