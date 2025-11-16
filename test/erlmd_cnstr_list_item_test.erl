%%%-----------------------------------------------------------------------------
%%% @doc Tests for list item construct.
%%%
%%% Based on CommonMark Section 5.2 - List items.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_list_item_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_list_item(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(list_item, T).

has_event(T, EventName) ->
    Events = erlmd_tokeniser:get_events(T),
    lists:any(fun(E) -> E#event.name =:= EventName end, Events).

count_event_type(T, EventName, Kind) ->
    Events = erlmd_tokeniser:get_events(T),
    length([E || E <- Events, E#event.name =:= EventName, E#event.kind =:= Kind]).

%%%=============================================================================
%%% Unordered List Marker Tests
%%%=============================================================================

unordered_asterisk_test() ->
    %% * item
    {ok, T} = parse_list_item(<<"* item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)).

unordered_dash_test() ->
    %% - item
    {ok, T} = parse_list_item(<<"- item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)).

unordered_plus_test() ->
    %% + item
    {ok, T} = parse_list_item(<<"+ item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)).

%%%=============================================================================
%%% Ordered List Marker Tests
%%%=============================================================================

ordered_single_digit_test() ->
    %% 1. item
    {ok, T} = parse_list_item(<<"1. item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)),
    ?assert(has_event(T, list_item_value)).

ordered_multiple_digits_test() ->
    %% 123. item
    {ok, T} = parse_list_item(<<"123. item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)),
    ?assert(has_event(T, list_item_value)).

ordered_paren_marker_test() ->
    %% 1) item
    {ok, T} = parse_list_item(<<"1) item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)),
    ?assert(has_event(T, list_item_value)).

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

not_list_wrong_char_test() ->
    %% @ item (wrong character)
    {nok, _T} = parse_list_item(<<"@ item">>).

ordered_too_many_digits_test() ->
    %% 1234567890. item (10 digits - too many)
    {nok, _T} = parse_list_item(<<"1234567890. item">>).

%%%=============================================================================
%%% Indentation Tests
%%%=============================================================================

unordered_with_one_space_indent_test() ->
    %%  * item (1 space before *)
    {ok, T} = parse_list_item(<<" * item">>),

    ?assert(has_event(T, list_item)).

unordered_with_three_spaces_indent_test() ->
    %%    * item (3 spaces before *)
    {ok, T} = parse_list_item(<<"   * item">>),

    ?assert(has_event(T, list_item)).

%%%=============================================================================
%%% Whitespace After Marker Tests (Day 2)
%%%=============================================================================

no_space_after_marker_test() ->
    %% *item (no space after *)
    {ok, T} = parse_list_item(<<"*item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)).

multiple_spaces_after_marker_test() ->
    %% *    item (4 spaces after *)
    {ok, T} = parse_list_item(<<"*    item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, space_or_tab)).

tab_after_marker_test() ->
    %% *\titem (tab after *)
    {ok, T} = parse_list_item(<<"*\titem">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, space_or_tab)).

empty_list_item_test() ->
    %% * (just marker, no content)
    {ok, T} = parse_list_item(<<"*">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)).

ordered_no_space_after_marker_test() ->
    %% 1.item (no space after marker)
    {ok, T} = parse_list_item(<<"1.item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_value)).

five_spaces_after_marker_test() ->
    %% *     item (5 spaces after marker)
    %% Should consume only 4 spaces (max allowed for prefix)
    {ok, T} = parse_list_item(<<"*     item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, space_or_tab)).

six_spaces_after_marker_test() ->
    %% *      item (6 spaces after marker)
    %% Should still consume only 4 spaces
    {ok, T} = parse_list_item(<<"*      item">>),

    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, space_or_tab)).

%%%=============================================================================
%%% Continuation Tests (Day 3 - Basic)
%%%=============================================================================

continuation_with_indent_test() ->
    %% Test cont_start with indented line (should succeed)
    T = test_helper:make_tokenizer(<<"  continued">>),
    {ok, T1} = test_helper:run_construct(list_item_cont_start, T),

    ?assert(has_event(T1, space_or_tab)).

continuation_no_indent_test() ->
    %% Test cont_start with no indent (should fail)
    T = test_helper:make_tokenizer(<<"continued">>),
    {nok, _T1} = test_helper:run_construct(list_item_cont_start, T).

continuation_with_tab_test() ->
    %% Test cont_start with tab indent (should succeed)
    T = test_helper:make_tokenizer(<<"\tcontinued">>),
    {ok, T1} = test_helper:run_construct(list_item_cont_start, T),

    ?assert(has_event(T1, space_or_tab)).

%%%=============================================================================
%%% CommonMark Spec Examples (Day 4 - Integration)
%%%=============================================================================

%% Based on Example 261 - No space between marker and content
%% Note: We actually SUPPORT this (it's valid in our implementation)
commonmark_261_no_space_test() ->
    %% -one (no space - we allow this)
    {ok, T} = parse_list_item(<<"-one">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)).

commonmark_261_ordered_no_space_test() ->
    %% 2.two (no space - we allow this)
    {ok, T} = parse_list_item(<<"2.two">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_value)).

%% Based on Example 267 - Leading zeros
commonmark_267_leading_zero_test() ->
    %% 0. ok
    {ok, T} = parse_list_item(<<"0. ok">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_value)),
    ?assert(has_event(T, list_item_marker)).

%% Based on Example 281 - Empty list items
commonmark_281_empty_between_test() ->
    %% - (just marker, empty)
    {ok, T} = parse_list_item(<<"-">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_marker)).

%% All three unordered markers
all_unordered_markers_test() ->
    %% Test all three markers work
    {ok, T1} = parse_list_item(<<"* asterisk">>),
    ?assert(has_event(T1, list_item)),

    {ok, T2} = parse_list_item(<<"- dash">>),
    ?assert(has_event(T2, list_item)),

    {ok, T3} = parse_list_item(<<"+ plus">>),
    ?assert(has_event(T3, list_item)).

%% Ordered markers with both terminators
ordered_both_terminators_test() ->
    %% 1. and 1) both work
    {ok, T1} = parse_list_item(<<"1. period">>),
    ?assert(has_event(T1, list_item)),

    {ok, T2} = parse_list_item(<<"1) paren">>),
    ?assert(has_event(T2, list_item)).

%% Large ordered list number
large_ordered_number_test() ->
    %% 999999999. (max 9 digits)
    {ok, T} = parse_list_item(<<"999999999. item">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_value)).

%%%=============================================================================
%%% Thematic Break Interaction Tests (Day 4)
%%%=============================================================================

%% Single * is not a thematic break (needs 3+)
not_thematic_break_single_asterisk_test() ->
    %% * item (single asterisk - list item, not thematic break)
    {ok, T} = parse_list_item(<<"* item">>),
    ?assert(has_event(T, list_item)),
    %% Should NOT have thematic_break event
    Events = erlmd_tokeniser:get_events(T),
    ThematicBreaks = [E || E <- Events, E#event.name =:= thematic_break],
    ?assertEqual(0, length(ThematicBreaks)).

%% Single - is not a thematic break (needs 3+)
not_thematic_break_single_dash_test() ->
    %% - item (single dash - list item, not thematic break)
    {ok, T} = parse_list_item(<<"- item">>),
    ?assert(has_event(T, list_item)),
    %% Should NOT have thematic_break event
    Events = erlmd_tokeniser:get_events(T),
    ThematicBreaks = [E || E <- Events, E#event.name =:= thematic_break],
    ?assertEqual(0, length(ThematicBreaks)).

%% Two asterisks is not a thematic break (needs 3+)
not_thematic_break_two_asterisks_test() ->
    %% * * (two asterisks with space - still list item)
    {ok, T} = parse_list_item(<<"* *">>),
    ?assert(has_event(T, list_item)),
    Events = erlmd_tokeniser:get_events(T),
    ThematicBreaks = [E || E <- Events, E#event.name =:= thematic_break],
    ?assertEqual(0, length(ThematicBreaks)).

%%%=============================================================================
%%% Boundary Condition Tests (Day 4)
%%%=============================================================================

%% Exactly max indentation before marker (3 spaces)
max_indent_before_marker_test() ->
    %%    * item (3 spaces)
    {ok, T} = parse_list_item(<<"   * item">>),
    ?assert(has_event(T, list_item)).

%% Exactly 4 spaces after marker (max for prefix)
exactly_four_spaces_after_test() ->
    %% *    item (4 spaces after *)
    {ok, T} = parse_list_item(<<"*    item">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, space_or_tab)).

%% Exactly max digit count (9 digits)
max_digit_count_test() ->
    %% 123456789. item
    {ok, T} = parse_list_item(<<"123456789. item">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_value)).

%% Mixed marker types in sequence (not in same list, just testing each works)
mixed_markers_test() ->
    {ok, T1} = parse_list_item(<<"* first">>),
    ?assert(has_event(T1, list_item)),

    {ok, T2} = parse_list_item(<<"1. second">>),
    ?assert(has_event(T2, list_item)),

    {ok, T3} = parse_list_item(<<"- third">>),
    ?assert(has_event(T3, list_item)).

%% Single digit with paren
single_digit_paren_test() ->
    %% 5) item
    {ok, T} = parse_list_item(<<"5) item">>),
    ?assert(has_event(T, list_item)),
    ?assert(has_event(T, list_item_value)),
    ?assert(has_event(T, list_item_marker)).
