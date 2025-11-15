%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for partial_space_or_tab construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_prtl_space_or_tab_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% Tests
%%%=============================================================================

single_space_test() ->
    T = test_helper:make_tokenizer(<<" abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    %% Should have consumed space and be at 'a'
    ?assertEqual($a, erlmd_tokeniser:current(T2)),

    %% Should have space_or_tab events
    Events = erlmd_tokeniser:get_events(T2),
    SpaceCount = test_helper:count_events(Events, space_or_tab),
    ?assertEqual(2, SpaceCount).  % enter + exit

multiple_spaces_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

single_tab_test() ->
    T = test_helper:make_tokenizer(<<"\tabc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

multiple_tabs_test() ->
    T = test_helper:make_tokenizer(<<"\t\tabc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

mixed_whitespace_test() ->
    T = test_helper:make_tokenizer(<<" \t abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

no_whitespace_test() ->
    T = test_helper:make_tokenizer(<<"abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T),
    {nok, _T2} = test_helper:run_construct(space_or_tab_start, T1).

min_max_exact_test() ->
    T = test_helper:make_tokenizer(<<"  abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab_min_max(T, 2, 2),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

min_max_too_few_test() ->
    T = test_helper:make_tokenizer(<<" abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab_min_max(T, 2, 4),
    {nok, _T2} = test_helper:run_construct(space_or_tab_start, T1).

min_max_within_range_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab_min_max(T, 1, 4),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

min_max_at_max_test() ->
    T = test_helper:make_tokenizer(<<"    abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab_min_max(T, 1, 4),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

min_zero_optional_test() ->
    %% Min of 0 means whitespace is optional
    T = test_helper:make_tokenizer(<<"abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab_min_max(T, 0, 4),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),

    ?assertEqual($a, erlmd_tokeniser:current(T2)).

events_emitted_test() ->
    T = test_helper:make_tokenizer(<<"   abc">>),
    {{retry, space_or_tab_start}, T1} = erlmd_cnstr_prtl_space_or_tab:space_or_tab(T),
    {ok, T2} = test_helper:run_construct(space_or_tab_start, T1),
    Events = erlmd_tokeniser:get_events(T2),

    %% Should have space_or_tab enter and exit
    SpaceCount = test_helper:count_events(Events, space_or_tab),
    ?assertEqual(2, SpaceCount).  % enter + exit
