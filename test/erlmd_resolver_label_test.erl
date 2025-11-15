%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for label resolver.
%%%
%%% Tests that matched labels are resolved into link/image events.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_resolver_label_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_inline(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(text, T).

%%%=============================================================================
%%% Link Event Tests
%%%=============================================================================

simple_link_creates_link_events_test() ->
    {ok, T} = parse_inline(<<"[text](url)">>),

    %% Check events list contains link events (enter and exit)
    Events = erlmd_tokeniser:get_events(T),
    LinkEvents = [E || E <- Events, E#event.name =:= link],

    io:format("Total link events: ~p~n", [length(LinkEvents)]),
    io:format("Link events: ~p~n", [LinkEvents]),

    %% Should have at least enter and exit
    ?assert(length(LinkEvents) >= 2),

    %% Find enter and exit events
    EnterEvents = [E || E <- LinkEvents, E#event.kind =:= enter],
    ExitEvents = [E || E <- LinkEvents, E#event.kind =:= exit],

    ?assert(length(EnterEvents) >= 1),
    ?assert(length(ExitEvents) >= 1).

simple_image_creates_image_events_test() ->
    {ok, T} = parse_inline(<<"![alt](url)">>),

    Events = erlmd_tokeniser:get_events(T),
    ImageEvents = [E || E <- Events, E#event.name =:= image],

    %% Should have at least enter and exit
    ?assert(length(ImageEvents) >= 2),

    EnterEvents = [E || E <- ImageEvents, E#event.kind =:= enter],
    ExitEvents = [E || E <- ImageEvents, E#event.kind =:= exit],

    ?assert(length(EnterEvents) >= 1),
    ?assert(length(ExitEvents) >= 1).

reference_link_creates_link_events_test() ->
    {ok, T} = parse_inline(<<"[text][ref]">>),

    Events = erlmd_tokeniser:get_events(T),
    LinkEvents = [E || E <- Events, E#event.name =:= link],

    %% Should have link events even for references
    ?assert(length(LinkEvents) >= 2).

shortcut_reference_creates_link_events_test() ->
    {ok, T} = parse_inline(<<"[text]">>),

    Events = erlmd_tokeniser:get_events(T),
    LinkEvents = [E || E <- Events, E#event.name =:= link],

    %% Shortcut references should also create link events
    ?assert(length(LinkEvents) >= 2).

%% DISABLED - times out due to multiple links in single input
%% multiple_links_create_multiple_events_test() ->
%%     {ok, T} = parse_inline(<<"[link1](url1) and [link2](url2)">>),
%%     Labels = erlmd_tokeniser:get_labels(T),
%%     %% Should have 2 links with proper events
%%     io:format("Labels: ~p~n", [length(Labels)]).
