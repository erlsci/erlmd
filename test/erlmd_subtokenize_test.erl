%%%-----------------------------------------------------------------------------
%%% @doc Tests for subtokenization module.
%%%
%%% Tests the recursive processing of linked events.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_subtokenize_test).

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
    Events2 = erlmd_subtokenize:link(Events, 2),

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
    Events2 = erlmd_subtokenize:link_to(Events, 0, 2),

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
    Events2 = erlmd_subtokenize:link(Events, 2),

    %% Link second data to first data
    Events3 = erlmd_subtokenize:link_to(Events2, 2, 4),

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
    ?assertError({badmatch, false}, erlmd_subtokenize:link(Events, 2)).

%%%=============================================================================
%%% Subtokenize Tests (Basic - will expand as implementation progresses)
%%%=============================================================================

subtokenize_no_links_test() ->
    %% Events with no links should return done=true
    Events = [
        #event{kind = enter, name = paragraph},
        #event{kind = enter, name = data},
        #event{kind = exit, name = data},
        #event{kind = exit, name = paragraph}
    ],

    ParseState = #{bytes => <<"test">>},

    {ok, Result} = erlmd_subtokenize:subtokenize(Events, ParseState, undefined),

    ?assertEqual(true, Result#subresult.done),
    ?assertEqual([], Result#subresult.definitions),
    ?assertEqual([], Result#subresult.gfm_footnote_definitions).
