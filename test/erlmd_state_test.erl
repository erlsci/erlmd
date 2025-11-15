%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for erlmd_state dispatcher.
%%%
%%% Tests state dispatching and error handling.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_state_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% Dispatch Tests
%%%=============================================================================

dispatch_document_test() ->
    T = erlmd_tokeniser:new(<<>>, #{}),
    {ok, _T1} = erlmd_state:call(document, T).

dispatch_unknown_state_test() ->
    T = erlmd_tokeniser:new(<<"test">>, #{}),
    ?assertError({unknown_state, _}, erlmd_state:call(bogus_state, T)).

dispatch_returns_tokenizer_test() ->
    T = erlmd_tokeniser:new(<<>>, #{}),
    {ok, T1} = erlmd_state:call(document, T),
    ?assert(is_tuple(T1)),
    ?assertEqual(tokenizer, element(1, T1)).
