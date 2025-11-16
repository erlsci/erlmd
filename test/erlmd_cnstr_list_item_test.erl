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
