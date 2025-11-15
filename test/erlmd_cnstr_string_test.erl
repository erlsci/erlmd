%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for string content dispatcher.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_string_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_string(Input) ->
    T = erlmd_tokeniser:new(Input, #{}),
    erlmd_cnstr_string:start(T).

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% Test 1: Plain text becomes data (fallback)
plain_text_test() ->
    {Result, T1} = parse_string(<<"hello">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should succeed with data events
    ?assertEqual(ok, Result),
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assert(length(DataEvents) >= 2),  % At least Enter + Exit
    ?assertEqual(enter, (hd(DataEvents))#event.kind).

%% Test 2: Empty string
empty_string_test() ->
    {Result, T1} = parse_string(<<>>),
    Events = erlmd_tokeniser:get_events(T1),

    %% Should succeed with no events or minimal data
    ?assertEqual(ok, Result),
    ?assert(length(Events) =< 2).

%% Test 3: String dispatcher succeeds
dispatcher_succeeds_test() ->
    {Result, _T1} = parse_string(<<"test">>),
    ?assertEqual(ok, Result).
