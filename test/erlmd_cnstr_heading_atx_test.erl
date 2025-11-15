%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for ATX heading construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_heading_atx_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_heading(Input) ->
    T = test_helper:make_tokenizer(Input),
    case erlmd_cnstr_heading_atx:start(T) of
        {{retry, StateName}, T1} ->
            test_helper:run_construct(StateName, T1);
        {{next, StateName}, T1} ->
            test_helper:run_construct(StateName, T1);
        Result ->
            Result
    end.

%%%=============================================================================
%%% Tests
%%%=============================================================================

%% Test 1: Simple h1 heading
simple_h1_test() ->
    {Result, T1} = parse_heading(<<"# Heading">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should succeed
    ?assertEqual(ok, Result),

    %% Should have heading_atx events
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(HeadingEvents))#event.kind).

%% Test 2: Simple h2 heading
simple_h2_test() ->
    {Result, T1} = parse_heading(<<"## Heading">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 3: All heading levels (1-6)
all_levels_test() ->
    Inputs = [
        <<"# H1">>,
        <<"## H2">>,
        <<"### H3">>,
        <<"#### H4">>,
        <<"##### H5">>,
        <<"###### H6">>
    ],
    lists:foreach(fun(Input) ->
        {Result, T1} = parse_heading(Input),
        Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
        ?assertEqual(ok, Result),
        HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
        ?assertEqual(2, length(HeadingEvents))
    end, Inputs).

%% Test 4: Seven hashes - not a heading
seven_hashes_test() ->
    {Result, _T1} = parse_heading(<<"####### Not a heading">>),
    ?assertEqual(nok, Result).

%% Test 5: No space after # - not a heading
no_space_test() ->
    {Result, _T1} = parse_heading(<<"#No heading">>),
    ?assertEqual(nok, Result).

%% Test 6: Heading with closing sequence
with_closing_test() ->
    {Result, T1} = parse_heading(<<"# Heading #">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)),

    %% Should have closing sequence
    SeqEvents = [E || E <- Events, E#event.name =:= heading_atx_sequence],
    ?assert(length(SeqEvents) >= 2).  % At least opening, maybe closing

%% Test 7: Heading with multiple closing hashes
multiple_closing_test() ->
    {Result, T1} = parse_heading(<<"## Heading ####">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 8: Empty heading
empty_heading_test() ->
    {Result, T1} = parse_heading(<<"#">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 9: Empty heading with space
empty_with_space_test() ->
    {Result, T1} = parse_heading(<<"# ">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 10: Heading at EOF (no newline)
at_eof_test() ->
    {Result, T1} = parse_heading(<<"### Heading text">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 11: Heading with leading space
with_leading_space_test() ->
    {Result, T1} = parse_heading(<<" # Heading">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 12: Heading with leading spaces (3 max)
with_three_spaces_test() ->
    {Result, T1} = parse_heading(<<"   ## Heading">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 13: Heading with newline
with_newline_test() ->
    {Result, T1} = parse_heading(<<"# Heading\n">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 14: Heading with mixed content
mixed_content_test() ->
    {Result, T1} = parse_heading(<<"# Hello world foo bar">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)),

    %% Should have data event for content
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assert(length(DataEvents) >= 1).

%% Test 15: Closing sequence requires space before it
closing_no_space_test() ->
    {Result, T1} = parse_heading(<<"# Heading#">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    %% The # at the end is part of content, not a closing sequence
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 16: Heading with tabs
with_tabs_test() ->
    {Result, T1} = parse_heading(<<"#\tHeading">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 17: Just hashes (empty level 6)
just_six_hashes_test() ->
    {Result, T1} = parse_heading(<<"######">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ?assertEqual(ok, Result),
    HeadingEvents = [E || E <- Events, E#event.name =:= heading_atx],
    ?assertEqual(2, length(HeadingEvents)).

%% Test 18: Invalid - empty input
empty_input_test() ->
    {Result, _T1} = parse_heading(<<>>),
    ?assertEqual(nok, Result).

%% Test 19: Invalid - just whitespace
just_whitespace_test() ->
    {Result, _T1} = parse_heading(<<"   ">>),
    ?assertEqual(nok, Result).

%% Test 20: Invalid - just newline
just_newline_test() ->
    {Result, _T1} = parse_heading(<<"\n">>),
    ?assertEqual(nok, Result).
