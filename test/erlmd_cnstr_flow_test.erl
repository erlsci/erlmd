%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for flow (block-level) content dispatcher.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_flow_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_flow(Input) ->
    T = erlmd_tokenizer:new(Input, #{}),
    erlmd_cnstr_flow:start(T).

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% Test 1: Blank line is recognized
blank_line_test() ->
    {Result, _T1} = parse_flow(<<"  \n">>),
    %% Blank line construct will be tried first
    %% Since blank_line IS implemented, it should succeed
    ?assertEqual(ok, Result).

%% Test 2: Non-blank content should try paragraph (fallback)
paragraph_fallback_test() ->
    {Result, T1} = parse_flow(<<"hello">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should try all constructs and reach paragraph last
    %% Paragraph is now implemented in Phase 5, so should succeed
    ?assertEqual(ok, Result),

    %% Should have paragraph events
    ParaEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParaEvents)).

%% Test 3: Empty input - blank_line handles it
empty_flow_test() ->
    {Result, _T1} = parse_flow(<<>>),
    %% Blank line construct can handle empty input
    ?assertEqual(ok, Result).
