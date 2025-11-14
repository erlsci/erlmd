%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for position tracking utilities.
%%%
%%% Tests point and position creation, advancement, and formatting.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_util_position_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").

%%%=============================================================================
%%% Point Creation Tests
%%%=============================================================================

new_point_test() ->
    P = erlmd_util_position:new_point(5, 10, 42),
    ?assertEqual(5, P#point.line),
    ?assertEqual(10, P#point.column),
    ?assertEqual(42, P#point.offset).

new_point_start_test() ->
    P = erlmd_util_position:new_point(1, 1, 0),
    ?assertEqual(1, P#point.line),
    ?assertEqual(1, P#point.column),
    ?assertEqual(0, P#point.offset).

%%%=============================================================================
%%% Position Creation Tests
%%%=============================================================================

new_position_from_points_test() ->
    Start = erlmd_util_position:new_point(1, 1, 0),
    End = erlmd_util_position:new_point(5, 10, 42),
    Pos = erlmd_util_position:new_position(Start, End),
    ?assertEqual(Start, Pos#position.start),
    ?assertEqual(End, Pos#position.'end').

new_position_from_values_test() ->
    Pos = erlmd_util_position:new_position(1, 1, 0, 5, 10, 42),
    ?assertEqual(1, (Pos#position.start)#point.line),
    ?assertEqual(1, (Pos#position.start)#point.column),
    ?assertEqual(0, (Pos#position.start)#point.offset),
    ?assertEqual(5, (Pos#position.'end')#point.line),
    ?assertEqual(10, (Pos#position.'end')#point.column),
    ?assertEqual(42, (Pos#position.'end')#point.offset).

new_position_same_line_test() ->
    Pos = erlmd_util_position:new_position(3, 5, 20, 3, 10, 25),
    ?assertEqual(3, (Pos#position.start)#point.line),
    ?assertEqual(3, (Pos#position.'end')#point.line).

%%%=============================================================================
%%% Point Advancement - Column Tests
%%%=============================================================================

advance_column_test() ->
    P = #point{line = 1, column = 5, offset = 10},
    P2 = erlmd_util_position:advance_column(P),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(6, P2#point.column),
    ?assertEqual(11, P2#point.offset).

advance_column_multiple_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_column(P),
    P3 = erlmd_util_position:advance_column(P2),
    P4 = erlmd_util_position:advance_column(P3),
    ?assertEqual(1, P4#point.line),
    ?assertEqual(4, P4#point.column),
    ?assertEqual(3, P4#point.offset).

%%%=============================================================================
%%% Point Advancement - Line Tests
%%%=============================================================================

advance_line_test() ->
    P = #point{line = 5, column = 20, offset = 100},
    P2 = erlmd_util_position:advance_line(P),
    ?assertEqual(6, P2#point.line),
    ?assertEqual(1, P2#point.column),
    ?assertEqual(101, P2#point.offset).

advance_line_from_start_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_line(P),
    ?assertEqual(2, P2#point.line),
    ?assertEqual(1, P2#point.column),
    ?assertEqual(1, P2#point.offset).

advance_line_resets_column_test() ->
    P = #point{line = 1, column = 50, offset = 100},
    P2 = erlmd_util_position:advance_line(P),
    ?assertEqual(1, P2#point.column), % Column resets to 1
    ?assertEqual(2, P2#point.line).

%%%=============================================================================
%%% Point Advancement - By N Tests
%%%=============================================================================

advance_by_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_by(P, 3),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(4, P2#point.column),
    ?assertEqual(3, P2#point.offset).

advance_by_large_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_by(P, 100),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(101, P2#point.column),
    ?assertEqual(100, P2#point.offset).

advance_by_one_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_by(P, 1),
    P3 = erlmd_util_position:advance_column(P),
    ?assertEqual(P3#point.line, P2#point.line),
    ?assertEqual(P3#point.column, P2#point.column),
    ?assertEqual(P3#point.offset, P2#point.offset).

%%%=============================================================================
%%% Point Advancement - Tab Tests
%%%=============================================================================

advance_by_tab_from_column_1_test() ->
    %% From column 1, tab advances by 4 to column 5
    P = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_by_tab(P),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(5, P2#point.column),
    ?assertEqual(1, P2#point.offset). % Offset advances by 1 (the tab byte)

advance_by_tab_from_column_3_test() ->
    %% From column 3, tab advances by 2 to column 5 (next tab stop)
    P = #point{line = 1, column = 3, offset = 2},
    P2 = erlmd_util_position:advance_by_tab(P),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(5, P2#point.column),
    ?assertEqual(3, P2#point.offset).

advance_by_tab_from_column_5_test() ->
    %% From column 5, tab advances by 4 to column 9 (already at tab stop)
    P = #point{line = 1, column = 5, offset = 4},
    P2 = erlmd_util_position:advance_by_tab(P),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(9, P2#point.column),
    ?assertEqual(5, P2#point.offset).

advance_by_tab_from_column_6_test() ->
    %% From column 6, tab advances by 3 to column 9
    P = #point{line = 1, column = 6, offset = 5},
    P2 = erlmd_util_position:advance_by_tab(P),
    ?assertEqual(1, P2#point.line),
    ?assertEqual(9, P2#point.column),
    ?assertEqual(6, P2#point.offset).

advance_by_tab_sequence_test() ->
    %% Multiple tabs
    P1 = #point{line = 1, column = 1, offset = 0},
    P2 = erlmd_util_position:advance_by_tab(P1), % -> column 5
    P3 = erlmd_util_position:advance_by_tab(P2), % -> column 9
    P4 = erlmd_util_position:advance_by_tab(P3), % -> column 13
    ?assertEqual(5, P2#point.column),
    ?assertEqual(9, P3#point.column),
    ?assertEqual(13, P4#point.column).

%%%=============================================================================
%%% Unist Conversion Tests
%%%=============================================================================

to_unist_point_test() ->
    P = #point{line = 5, column = 10, offset = 42},
    Map = erlmd_util_position:to_unist_point(P),
    ?assertEqual(5, maps:get(line, Map)),
    ?assertEqual(10, maps:get(column, Map)),
    ?assertEqual(42, maps:get(offset, Map)).

to_unist_position_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 5, column = 10, offset = 42},
    Pos = #position{start = Start, 'end' = End},
    Map = erlmd_util_position:to_unist_position(Pos),
    StartMap = maps:get(start, Map),
    EndMap = maps:get('end', Map),
    ?assertEqual(1, maps:get(line, StartMap)),
    ?assertEqual(1, maps:get(column, StartMap)),
    ?assertEqual(5, maps:get(line, EndMap)),
    ?assertEqual(10, maps:get(column, EndMap)).

%%%=============================================================================
%%% Formatting Tests
%%%=============================================================================

format_point_test() ->
    P = #point{line = 10, column = 5, offset = 42},
    Formatted = erlmd_util_position:format_point(P),
    ?assertEqual(<<"10:5 (42)">>, Formatted).

format_point_start_test() ->
    P = #point{line = 1, column = 1, offset = 0},
    Formatted = erlmd_util_position:format_point(P),
    ?assertEqual(<<"1:1 (0)">>, Formatted).

format_position_test() ->
    Start = #point{line = 1, column = 1, offset = 0},
    End = #point{line = 5, column = 10, offset = 42},
    Pos = #position{start = Start, 'end' = End},
    Formatted = erlmd_util_position:format_position(Pos),
    ?assertEqual(<<"1:1 (0)-5:10 (42)">>, Formatted).

format_position_same_line_test() ->
    Start = #point{line = 3, column = 5, offset = 20},
    End = #point{line = 3, column = 10, offset = 25},
    Pos = #position{start = Start, 'end' = End},
    Formatted = erlmd_util_position:format_position(Pos),
    ?assertEqual(<<"3:5 (20)-3:10 (25)">>, Formatted).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

tracking_simple_string_test() ->
    %% Track position through "Hello"
    P1 = erlmd_util_position:new_point(1, 1, 0),
    P2 = erlmd_util_position:advance_column(P1), % H -> column 2
    P3 = erlmd_util_position:advance_column(P2), % e -> column 3
    P4 = erlmd_util_position:advance_column(P3), % l -> column 4
    P5 = erlmd_util_position:advance_column(P4), % l -> column 5
    P6 = erlmd_util_position:advance_column(P5), % o -> column 6

    ?assertEqual(1, P6#point.line),
    ?assertEqual(6, P6#point.column),
    ?assertEqual(5, P6#point.offset).

tracking_multiline_test() ->
    %% Track through "Hello\nWorld"
    P1 = erlmd_util_position:new_point(1, 1, 0),
    P2 = erlmd_util_position:advance_by(P1, 5),  % "Hello" -> column 6, offset 5
    P3 = erlmd_util_position:advance_line(P2),   % \n -> line 2, column 1, offset 6
    P4 = erlmd_util_position:advance_by(P3, 5),  % "World" -> column 6, offset 11

    ?assertEqual(2, P4#point.line),
    ?assertEqual(6, P4#point.column),
    ?assertEqual(11, P4#point.offset).

tracking_with_tabs_test() ->
    %% Track through "\tHello"
    P1 = erlmd_util_position:new_point(1, 1, 0),
    P2 = erlmd_util_position:advance_by_tab(P1), % \t -> column 5, offset 1
    P3 = erlmd_util_position:advance_by(P2, 5),  % "Hello" -> column 10, offset 6

    ?assertEqual(1, P3#point.line),
    ?assertEqual(10, P3#point.column),
    ?assertEqual(6, P3#point.offset).

complex_position_tracking_test() ->
    %% Track a complex markdown construct: "# Heading\nParagraph"
    Start = erlmd_util_position:new_point(1, 1, 0),

    % Track "# Heading\n"
    AfterHash = erlmd_util_position:advance_column(Start),
    AfterSpace = erlmd_util_position:advance_column(AfterHash),
    AfterHeading = erlmd_util_position:advance_by(AfterSpace, 7), % "Heading"
    AfterNewline = erlmd_util_position:advance_line(AfterHeading),

    % Track "Paragraph"
    AfterPara = erlmd_util_position:advance_by(AfterNewline, 9),

    %% Create position for the entire thing
    Pos = erlmd_util_position:new_position(Start, AfterPara),

    ?assertEqual(1, (Pos#position.start)#point.line),
    ?assertEqual(1, (Pos#position.start)#point.column),
    ?assertEqual(2, (Pos#position.'end')#point.line),
    ?assertEqual(10, (Pos#position.'end')#point.column).
