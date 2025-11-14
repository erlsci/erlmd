%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for erlmd_state dispatcher.
%%%
%%% Tests state dispatching and error handling.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_state_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").
-include("../src/tokenizer_internal.hrl").

%%%=============================================================================
%%% Dispatch Tests
%%%=============================================================================

dispatch_document_start_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    {ok, _T1} = erlmd_state:call(document_start, T).

dispatch_unknown_state_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    ?assertError({unknown_state, _}, erlmd_state:call(bogus_state, T)).

dispatch_returns_tokenizer_test() ->
    T = erlmd_tokenizer:new(<<"test">>, #{}),
    {ok, T1} = erlmd_state:call(document_start, T),
    ?assert(is_tuple(T1)),
    ?assertEqual(tokenizer, element(1, T1)).
