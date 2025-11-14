%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for partial_data construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_partial_data_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").
-include("../src/tokenizer_internal.hrl").

%%%=============================================================================
%%% Tests
%%%=============================================================================

simple_data_test() ->
    T = test_helper:make_tokenizer(<<"Hello">>),
    T1 = erlmd_tokenizer:set_markers(T, []),
    {ok, T2} = erlmd_construct_partial_data:start(T1),
    Events = erlmd_tokenizer:get_events(T2),

    %% Should have data enter and exit
    ?assertEqual(2, length(Events)),
    [Exit, Enter] = Events,  % Reversed order
    ?assertEqual(enter, Enter#event.kind),
    ?assertEqual(data, Enter#event.name),
    ?assertEqual(exit, Exit#event.kind),
    ?assertEqual(data, Exit#event.name).

data_with_newline_test() ->
    T = test_helper:make_tokenizer(<<"Hello\n">>),
    T1 = erlmd_tokenizer:set_markers(T, []),
    {ok, T2} = erlmd_construct_partial_data:start(T1),
    Events = erlmd_tokenizer:get_events(T2),

    %% Should have data enter/exit and line_ending enter/exit
    DataCount = test_helper:count_events(Events, data),
    LineEndingCount = test_helper:count_events(Events, line_ending),
    ?assertEqual(2, DataCount),      % enter + exit
    ?assertEqual(2, LineEndingCount). % enter + exit

empty_data_test() ->
    T = test_helper:make_tokenizer(<<>>),
    T1 = erlmd_tokenizer:set_markers(T, []),
    {ok, T2} = erlmd_construct_partial_data:start(T1),
    Events = erlmd_tokenizer:get_events(T2),
    ?assertEqual([], Events).

data_with_marker_test() ->
    T = test_helper:make_tokenizer(<<"Hello*World">>),
    T1 = erlmd_tokenizer:set_markers(T, [$*]),
    {ok, T2} = erlmd_construct_partial_data:start(T1),

    %% Should stop at the *
    Events = erlmd_tokenizer:get_events(T2),
    DataCount = test_helper:count_events(Events, data),
    ?assertEqual(2, DataCount),  % Only "Hello" (enter + exit)

    %% Current position should be at *
    ?assertEqual($*, erlmd_tokenizer:current(T2)).

multiple_words_test() ->
    T = test_helper:make_tokenizer(<<"Hello World">>),
    T1 = erlmd_tokenizer:set_markers(T, []),
    {ok, T2} = erlmd_construct_partial_data:start(T1),
    Events = erlmd_tokenizer:get_events(T2),

    %% Entire thing should be one data chunk
    DataCount = test_helper:count_events(Events, data),
    ?assertEqual(2, DataCount).

data_stops_at_eof_test() ->
    T = test_helper:make_tokenizer(<<"Hello">>),
    T1 = erlmd_tokenizer:set_markers(T, []),
    {ok, T2} = erlmd_construct_partial_data:start(T1),

    %% Should be at EOF now
    ?assertEqual(eof, erlmd_tokenizer:current(T2)).

multiline_data_test() ->
    T = test_helper:make_tokenizer(<<"Hello\nWorld\n">>),
    T1 = erlmd_tokenizer:set_markers(T, []),
    {ok, T2} = erlmd_construct_partial_data:start(T1),
    Events = erlmd_tokenizer:get_events(T2),

    %% Should have 2 data chunks and 2 line endings
    DataCount = test_helper:count_events(Events, data),
    LineEndingCount = test_helper:count_events(Events, line_ending),
    ?assertEqual(4, DataCount),       % 2 data chunks (enter + exit each)
    ?assertEqual(4, LineEndingCount). % 2 line endings (enter + exit each)

marker_at_start_test() ->
    T = test_helper:make_tokenizer(<<"*Hello">>),
    T1 = erlmd_tokenizer:set_markers(T, [$*]),
    {ok, T2} = erlmd_construct_partial_data:start(T1),
    Events = erlmd_tokenizer:get_events(T2),

    %% Marker at start should stop immediately
    ?assertEqual([], Events),
    ?assertEqual($*, erlmd_tokenizer:current(T2)).
