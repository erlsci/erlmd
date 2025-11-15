%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for text (inline) content dispatcher.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_text_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_text(Input) ->
    T = erlmd_tokeniser:new(Input, #{}),
    erlmd_cnstr_text:start(T).

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% Test 1: Plain text becomes data (fallback)
plain_text_test() ->
    {Result, T1} = parse_text(<<"hello world">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should succeed with data events
    ?assertEqual(ok, Result),
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assert(length(DataEvents) >= 2).  % At least Enter + Exit

%% Test 2: Empty input
empty_text_test() ->
    {Result, T1} = parse_text(<<>>),
    Events = erlmd_tokeniser:get_events(T1),

    %% Should succeed
    ?assertEqual(ok, Result),
    ?assert(length(Events) =< 2).

%% Test 3: Text dispatcher succeeds
dispatcher_succeeds_test() ->
    {Result, _T1} = parse_text(<<"plain text">>),
    ?assertEqual(ok, Result).
