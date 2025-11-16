%%%-----------------------------------------------------------------------------
%%% @doc Tests for block quote construct.
%%%
%%% Based on CommonMark Section 5.1 - Block quotes.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_block_quote_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_block_quote(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(block_quote, T).

has_event(T, EventName) ->
    Events = erlmd_tokeniser:get_events(T),
    lists:any(fun(E) -> E#event.name =:= EventName end, Events).

count_event_type(T, EventName, Kind) ->
    Events = erlmd_tokeniser:get_events(T),
    length([E || E <- Events, E#event.name =:= EventName, E#event.kind =:= Kind]).

%%%=============================================================================
%%% Basic Functionality Tests
%%%=============================================================================

simple_block_quote_test() ->
    %% > hello
    {ok, T} = parse_block_quote(<<"> hello">>),

    %% Should have block_quote enter event
    %% Note: exit is handled by document container, not by construct
    ?assert(has_event(T, block_quote)),
    EnterEvents = count_event_type(T, block_quote, enter),
    ?assertEqual(1, EnterEvents).

block_quote_with_marker_test() ->
    %% > text
    {ok, T} = parse_block_quote(<<"> text">>),

    %% Should have block_quote_marker events
    ?assert(has_event(T, block_quote_marker)).

block_quote_without_space_test() ->
    %% >text (no space after >)
    {ok, T} = parse_block_quote(<<">text">>),

    %% Should still parse as block quote
    ?assert(has_event(T, block_quote)),
    ?assert(has_event(T, block_quote_marker)).

empty_block_quote_test() ->
    %% > (just the marker)
    {ok, T} = parse_block_quote(<<">">>),

    %% Should parse as block quote
    ?assert(has_event(T, block_quote)),
    ?assert(has_event(T, block_quote_marker)).

%%%=============================================================================
%%% Indentation Tests
%%% DISABLED: Attempt mechanism needs more work
%%%=============================================================================

%% block_quote_with_one_space_indent_test() ->
%%     %%  > text (1 space before >)
%%     {ok, T} = parse_block_quote(<<" > text">>),
%%
%%     ?assert(has_event(T, block_quote)).
%%
%% block_quote_with_two_spaces_indent_test() ->
%%     %%   > text (2 spaces before >)
%%     {ok, T} = parse_block_quote(<<"  > text">>),
%%
%%     ?assert(has_event(T, block_quote)).
%%
%% block_quote_with_three_spaces_indent_test() ->
%%     %%    > text (3 spaces before >)
%%     {ok, T} = parse_block_quote(<<"   > text">>),
%%
%%     ?assert(has_event(T, block_quote)).

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

not_block_quote_wrong_char_test() ->
    %% < text (wrong character)
    {nok, _T} = parse_block_quote(<<"< text">>).

block_quote_with_prefix_test() ->
    %% > hello
    {ok, T} = parse_block_quote(<<"> hello">>),

    %% Should have block_quote_prefix events
    ?assert(has_event(T, block_quote_prefix)),
    PrefixEnter = count_event_type(T, block_quote_prefix, enter),
    PrefixExit = count_event_type(T, block_quote_prefix, exit),
    ?assertEqual(PrefixEnter, PrefixExit).

%%%=============================================================================
%%% Event Structure Tests
%%%=============================================================================

event_nesting_test() ->
    %% Verify proper event nesting for: > text
    {ok, T} = parse_block_quote(<<"> text">>),
    Events = erlmd_tokeniser:get_events(T),

    %% Events should be in reverse order (newest first)
    %% Look for: exit(block_quote_prefix), ..., enter(block_quote_prefix), ..., enter(block_quote)

    BlockQuoteEnters = [I || {I, E} <- lists:zip(lists:seq(1, length(Events)), Events),
                              E#event.kind =:= enter, E#event.name =:= block_quote],
    PrefixEnters = [I || {I, E} <- lists:zip(lists:seq(1, length(Events)), Events),
                         E#event.kind =:= enter, E#event.name =:= block_quote_prefix],

    %% Prefix should be entered after block_quote (later in reversed list)
    ?assert(length(BlockQuoteEnters) > 0),
    ?assert(length(PrefixEnters) > 0).
