%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for core type definitions.
%%%
%%% Tests creation and field access for all core record types.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_types_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").

%%%=============================================================================
%%% Point Tests
%%%=============================================================================

point_creation_test() ->
    P = #point{line = 5, column = 10, offset = 42},
    ?assertEqual(5, P#point.line),
    ?assertEqual(10, P#point.column),
    ?assertEqual(42, P#point.offset).

point_default_values_test() ->
    P = #point{},
    ?assertEqual(1, P#point.line),
    ?assertEqual(1, P#point.column),
    ?assertEqual(0, P#point.offset).

point_partial_creation_test() ->
    P = #point{line = 3},
    ?assertEqual(3, P#point.line),
    ?assertEqual(1, P#point.column),
    ?assertEqual(0, P#point.offset).

point_update_test() ->
    P1 = #point{line = 1, column = 1, offset = 0},
    P2 = P1#point{column = 5, offset = 4},
    ?assertEqual(1, P2#point.line),
    ?assertEqual(5, P2#point.column),
    ?assertEqual(4, P2#point.offset).

%%%=============================================================================
%%% Position Tests
%%%=============================================================================

position_creation_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 1, column = 5, offset = 4},
    Pos = #position{start = Start, 'end' = End},
    ?assertEqual(Start, Pos#position.start),
    ?assertEqual(End, Pos#position.'end').

position_span_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 5, column = 10, offset = 100},
    Pos = #position{start = Start, 'end' = End},
    ?assertEqual(1, (Pos#position.start)#point.line),
    ?assertEqual(5, (Pos#position.'end')#point.line).

position_multiline_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 10, column = 1, offset = 200},
    Pos = #position{start = Start, 'end' = End},
    ?assert((Pos#position.'end')#point.line > (Pos#position.start)#point.line).

%%%=============================================================================
%%% Event Tests
%%%=============================================================================

event_enter_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    E = #event{kind = enter, name = paragraph, point = P},
    ?assertEqual(enter, E#event.kind),
    ?assertEqual(paragraph, E#event.name),
    ?assertEqual(P, E#event.point),
    ?assertEqual(undefined, E#event.link),
    ?assertEqual(undefined, E#event.content).

event_exit_test() ->
    P = #point{line = 5, column = 20, offset = 100},
    E = #event{kind = exit, name = paragraph, point = P},
    ?assertEqual(exit, E#event.kind),
    ?assertEqual(paragraph, E#event.name).

event_with_link_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    L = #link{content = text},
    E = #event{kind = enter, name = paragraph, point = P, link = L},
    ?assertEqual(L, E#event.link).

event_with_content_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    E = #event{kind = enter, name = paragraph, point = P, content = flow},
    ?assertEqual(flow, E#event.content).

event_heading_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    E = #event{kind = enter, name = heading_atx, point = P},
    ?assertEqual(heading_atx, E#event.name).

event_code_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    E = #event{kind = enter, name = code_fenced, point = P},
    ?assertEqual(code_fenced, E#event.name).

event_emphasis_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    E = #event{kind = enter, name = emphasis, point = P},
    ?assertEqual(emphasis, E#event.name).

%%%=============================================================================
%%% Link Tests
%%%=============================================================================

link_creation_test() ->
    L = #link{previous = 10, next = 20, content = text},
    ?assertEqual(10, L#link.previous),
    ?assertEqual(20, L#link.next),
    ?assertEqual(text, L#link.content).

link_default_values_test() ->
    L = #link{content = flow},
    ?assertEqual(undefined, L#link.previous),
    ?assertEqual(undefined, L#link.next),
    ?assertEqual(flow, L#link.content).

link_content_types_test() ->
    L1 = #link{content = flow},
    L2 = #link{content = content},
    L3 = #link{content = string},
    L4 = #link{content = text},
    ?assertEqual(flow, L1#link.content),
    ?assertEqual(content, L2#link.content),
    ?assertEqual(string, L3#link.content),
    ?assertEqual(text, L4#link.content).

%%%=============================================================================
%%% Message Tests
%%%=============================================================================

message_creation_test() ->
    P = #point{line = 5, column = 10, offset = 50},
    Place = {point, P},
    M = #message{
        place = Place,
        reason = <<"Unexpected character">>,
        rule_id = <<"MD001">>,
        source = <<"erlmd">>
    },
    ?assertEqual(Place, M#message.place),
    ?assertEqual(<<"Unexpected character">>, M#message.reason),
    ?assertEqual(<<"MD001">>, M#message.rule_id),
    ?assertEqual(<<"erlmd">>, M#message.source).

message_with_position_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 1, column = 5, offset = 4},
    Pos = #position{start = Start, 'end' = End},
    Place = {position, Pos},
    M = #message{
        place = Place,
        reason = <<"Error">>,
        rule_id = test,
        source = erlmd
    },
    ?assertEqual(Place, M#message.place).

message_default_place_test() ->
    M = #message{
        reason = <<"Error">>,
        rule_id = test,
        source = erlmd
    },
    ?assertEqual(undefined, M#message.place).

message_string_types_test() ->
    M1 = #message{reason = <<"Binary">>, rule_id = <<"R1">>, source = <<"S1">>},
    M2 = #message{reason = "String", rule_id = atom, source = atom},
    ?assertEqual(<<"Binary">>, M1#message.reason),
    ?assertEqual("String", M2#message.reason).

%%%=============================================================================
%%% Type Guard Tests
%%%=============================================================================

point_guard_test() ->
    P = #point{},
    ?assert(?is_point(P)),
    ?assertNot(?is_point(not_a_point)),
    ?assertNot(?is_point(#position{})).

position_guard_test() ->
    Start = #point{},
    End = #point{},
    Pos = #position{start = Start, 'end' = End},
    ?assert(?is_position(Pos)),
    ?assertNot(?is_position(not_a_position)),
    ?assertNot(?is_position(#point{})).

event_guard_test() ->
    E = #event{kind = enter, name = paragraph, point = #point{}},
    ?assert(?is_event(E)),
    ?assertNot(?is_event(not_an_event)),
    ?assertNot(?is_event(#point{})).

link_guard_test() ->
    L = #link{content = text},
    ?assert(?is_link(L)),
    ?assertNot(?is_link(not_a_link)),
    ?assertNot(?is_link(#point{})).

message_guard_test() ->
    M = #message{reason = <<"test">>, rule_id = test, source = test},
    ?assert(?is_message(M)),
    ?assertNot(?is_message(not_a_message)),
    ?assertNot(?is_message(#point{})).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

smoke_test() ->
    %% Test that we can create all types
    P = #point{line = 1, column = 1, offset = 0},
    ?assert(is_record(P, point)),

    Pos = #position{start = P, 'end' = P},
    ?assert(is_record(Pos, position)),

    E = #event{kind = enter, name = paragraph, point = P},
    ?assert(is_record(E, event)),

    L = #link{content = text},
    ?assert(is_record(L, link)),

    M = #message{reason = <<"test">>, rule_id = test, source = test},
    ?assert(is_record(M, message)),

    ok.

complex_event_structure_test() ->
    %% Create a complex nested structure
    P1 = #point{line = 1, column = 1, offset = 0},
    P2 = #point{line = 1, column = 10, offset = 9},

    L = #link{previous = 0, next = 2, content = text},

    E1 = #event{kind = enter, name = paragraph, point = P1, link = L, content = flow},
    E2 = #event{kind = exit, name = paragraph, point = P2},

    ?assertEqual(enter, E1#event.kind),
    ?assertEqual(exit, E2#event.kind),
    ?assertEqual(text, (E1#event.link)#link.content),
    ?assertEqual(flow, E1#event.content).
