%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for document (top-level) content dispatcher.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_document_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_document(Input) ->
    T = erlmd_tokenizer:new(Input, #{}),
    erlmd_cnstr_document:start(T).

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% Test 1: Empty document
empty_document_test() ->
    {Result, T1} = parse_document(<<>>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should succeed with document enter/exit
    ?assertEqual(ok, Result),
    DocEvents = [E || E <- Events, E#event.name =:= document],
    ?assertEqual(2, length(DocEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(DocEvents))#event.kind),
    ?assertEqual(exit, (lists:last(DocEvents))#event.kind).

%% Test 2: Document structure exists
document_structure_test() ->
    {Result, T1} = parse_document(<<>>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Document should be outermost token
    ?assertEqual(ok, Result),
    ?assertEqual(document, (hd(Events))#event.name),
    ?assertEqual(enter, (hd(Events))#event.kind),
    ?assertEqual(document, (lists:last(Events))#event.name),
    ?assertEqual(exit, (lists:last(Events))#event.kind).

%% Test 3: Document with content (will fail since paragraph not implemented)
%%         This test shows the expected behavior when paragraph IS implemented
document_with_content_will_fail_test() ->
    Result = parse_document(<<"Hello">>),
    %% Since paragraph is not implemented, this will return error
    %% When paragraph is implemented in Phase 5, this should succeed
    case Result of
        {error, _, _} -> ok;  % Expected for now
        {ok, _} -> ?assert(false)  % Should not succeed yet
    end.
