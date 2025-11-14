%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for partial_space_or_tab construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_partial_space_or_tab_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").
-include("../src/tokenizer_internal.hrl").

%%%=============================================================================
%%% Tests
%%%=============================================================================

single_space_test() ->
    T = test_helper:make_tokenizer(<<" abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

multiple_spaces_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

single_tab_test() ->
    T = test_helper:make_tokenizer(<<"\tabc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

multiple_tabs_test() ->
    T = test_helper:make_tokenizer(<<"\t\tabc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

mixed_whitespace_test() ->
    T = test_helper:make_tokenizer(<<" \t abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

no_whitespace_test() ->
    T = test_helper:make_tokenizer(<<"abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {nok, _T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1).

min_max_exact_test() ->
    T = test_helper:make_tokenizer(<<"  abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T1, 2, 2),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

min_max_too_few_test() ->
    T = test_helper:make_tokenizer(<<" abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {nok, _T2} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T1, 2, 4).

min_max_within_range_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T1, 1, 4),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

min_max_at_max_test() ->
    T = test_helper:make_tokenizer(<<"    abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T1, 1, 4),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

min_zero_optional_test() ->
    %% Min of 0 means whitespace is optional
    T = test_helper:make_tokenizer(<<"abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab_min_max(T1, 0, 4),
    ?assertEqual($a, erlmd_tokenizer:current(T2)).

events_emitted_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_partial_space_or_tab:space_or_tab(T1),
    Events = erlmd_tokenizer:get_events(T2),

    %% Should have space_or_tab enter and exit
    SpaceCount = test_helper:count_events(Events, space_or_tab),
    ?assertEqual(2, SpaceCount).  % enter + exit
