%%%-----------------------------------------------------------------------------
%%% @doc Tests for subtokenisation module.
%%%
%%% Tests the recursive processing of linked events.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_subtokenise_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Link Helper Function Tests
%%%=============================================================================

link_simple_test() ->
    %% Create simple event chain with void event to link
    Events = [
        #event{kind = enter, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0},
               link = #link{content = text}},
        #event{kind = exit, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 2, offset = 1},
               link = #link{content = text}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 6, offset = 5}}
    ],

    %% Link event at index 2 to previous void event at index 0
    Events2 = erlmd_subtokenise:link(Events, 2),

    %% Check links were set correctly
    E0 = lists:nth(1, Events2),
    E2 = lists:nth(3, Events2),

    ?assertEqual(enter, E0#event.kind),
    ?assertEqual(paragraph, E0#event.name),
    ?assertEqual(2, E0#event.link#link.next),

    ?assertEqual(enter, E2#event.kind),
    ?assertEqual(data, E2#event.name),
    ?assertEqual(0, E2#event.link#link.previous).

link_to_explicit_test() ->
    %% Test explicit link_to function
    Events = [
        #event{kind = enter, name = heading_atx,
               point = #point{line = 1, column = 1, offset = 0},
               link = #link{content = text}},
        #event{kind = exit, name = heading_atx,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 3, offset = 2}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 8, offset = 7}}
    ],

    %% Link indices 0 and 2
    Events2 = erlmd_subtokenise:link_to(Events, 0, 2),

    %% Verify bidirectional link
    E0 = lists:nth(1, Events2),
    E2 = lists:nth(3, Events2),

    ?assertEqual(2, E0#event.link#link.next),
    ?assertEqual(text, E0#event.link#link.content),

    ?assertEqual(0, E2#event.link#link.previous),
    ?assertEqual(text, E2#event.link#link.content).

link_chain_test() ->
    %% Test linking multiple events in a chain
    Events = [
        #event{kind = enter, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0},
               link = #link{content = text}},
        #event{kind = exit, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 2, offset = 1},
               link = #link{content = text}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 7, offset = 6}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 8, offset = 7},
               link = #link{content = text}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 12, offset = 11}}
    ],

    %% Link first data to paragraph
    Events2 = erlmd_subtokenise:link(Events, 2),

    %% Link second data to first data
    Events3 = erlmd_subtokenise:link_to(Events2, 2, 4),

    %% Verify chain: paragraph -> data[0] -> data[1]
    E0 = lists:nth(1, Events3),
    E2 = lists:nth(3, Events3),
    E4 = lists:nth(5, Events3),

    ?assertEqual(2, E0#event.link#link.next),
    ?assertEqual(undefined, E0#event.link#link.previous),

    ?assertEqual(4, E2#event.link#link.next),
    ?assertEqual(0, E2#event.link#link.previous),

    ?assertEqual(undefined, E4#event.link#link.next),
    ?assertEqual(2, E4#event.link#link.previous).

link_content_type_mismatch_test() ->
    %% Test that linking events with different content types fails
    Events = [
        #event{kind = enter, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0},
               link = #link{content = text}},
        #event{kind = exit, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 2, offset = 1},
               link = #link{content = string}}  % Different content type!
    ],

    %% Should crash with assertion failure
    ?assertError({badmatch, false}, erlmd_subtokenise:link(Events, 2)).

%%%=============================================================================
%%% Subtokenise Tests (Basic - will expand as implementation progresses)
%%%=============================================================================

subtokenise_no_links_test() ->
    %% Events with no links should return done=true
    Events = [
        #event{kind = enter, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 5, offset = 4}},
        #event{kind = exit, name = paragraph,
               point = #point{line = 1, column = 5, offset = 4}}
    ],

    ParseState = #{bytes => <<"test">>},

    {ok, Result, Events2} = erlmd_subtokenise:subtokenise(Events, ParseState, undefined),

    ?assertEqual(true, Result#subresult.done),
    ?assertEqual([], Result#subresult.definitions),
    ?assertEqual([], Result#subresult.gfm_footnote_definitions),
    ?assertEqual(Events, Events2).  % No changes when no links

subtokenise_with_link_test() ->
    %% Events with a link should return done=false (needs another pass)
    Events = [
        #event{kind = enter, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0},
               link = #link{content = text, next = 2}},
        #event{kind = exit, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 1, offset = 0},
               link = #link{content = text, previous = 0}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 5, offset = 4}}
    ],

    ParseState = #{bytes => <<"test">>},

    {ok, Result, _Events2} = erlmd_subtokenise:subtokenise(Events, ParseState, undefined),

    %% When links are found, done should be false (need another subtokenise pass)
    ?assertEqual(false, Result#subresult.done).

%%%=============================================================================
%%% Slice Extraction Tests (Week 1 Days 3-4)
%%%=============================================================================

extract_slice_simple_test() ->
    %% Test extracting a simple content slice
    %% "Hello World" is 11 bytes (indices 0-10)
    %% Exit offset should be 11 (after the last character)
    Events = [
        #event{kind = enter, name = paragraph,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = exit, name = paragraph,
               point = #point{line = 1, column = 12, offset = 11}}
    ],

    Bytes = <<"Hello World">>,
    Slice = erlmd_subtokenise:extract_slice(Events, Bytes, 0),

    ?assertEqual(<<"Hello World">>, Slice).

extract_slice_void_test() ->
    %% Test extracting from a void event (no content)
    Events = [
        #event{kind = enter, name = paragraph,
               point = #point{line = 1, column = 1, offset = 5}},
        #event{kind = exit, name = paragraph,
               point = #point{line = 1, column = 1, offset = 5}}
    ],

    Bytes = <<"test test">>,
    Slice = erlmd_subtokenise:extract_slice(Events, Bytes, 0),

    ?assertEqual(<<>>, Slice).

extract_slice_nested_test() ->
    %% Test extracting with nested events of same name
    Events = [
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 1, offset = 0}},
        #event{kind = enter, name = data,
               point = #point{line = 1, column = 2, offset = 1}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 4, offset = 3}},
        #event{kind = exit, name = data,
               point = #point{line = 1, column = 5, offset = 4}}
    ],

    Bytes = <<"test">>,
    Slice = erlmd_subtokenise:extract_slice(Events, Bytes, 0),

    %% Should extract from first enter to last exit (entire content)
    ?assertEqual(<<"test">>, Slice).
