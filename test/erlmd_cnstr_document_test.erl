%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for document (top-level) content dispatcher.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_document_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_document(Input) ->
    T = erlmd_tokeniser:new(Input, #{}),
    erlmd_cnstr_document:start(T).

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% Test 1: Empty document
empty_document_test() ->
    {Result, T1} = parse_document(<<>>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should succeed with document enter/exit
    ?assertEqual(ok, Result),
    DocEvents = [E || E <- Events, E#event.name =:= document],
    ?assertEqual(2, length(DocEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(DocEvents))#event.kind),
    ?assertEqual(exit, (lists:last(DocEvents))#event.kind).

%% Test 2: Document structure exists
document_structure_test() ->
    {Result, T1} = parse_document(<<>>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Document should be outermost token
    ?assertEqual(ok, Result),
    ?assertEqual(document, (hd(Events))#event.name),
    ?assertEqual(enter, (hd(Events))#event.kind),
    ?assertEqual(document, (lists:last(Events))#event.name),
    ?assertEqual(exit, (lists:last(Events))#event.kind).

%% Test 3: Document with content (paragraph now implemented in Phase 5)
document_with_content_test() ->
    {ok, T1} = parse_document(<<"Hello">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should have document events
    DocEvents = [E || E <- Events, E#event.name =:= document],
    ?assertEqual(2, length(DocEvents)),

    %% Should have paragraph events
    ParaEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParaEvents)).
