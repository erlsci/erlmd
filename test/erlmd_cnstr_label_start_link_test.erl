%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for label_start_link construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_label_start_link_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_label_start_link(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(label_start_link, T).

%%%=============================================================================
%%% Basic Functionality Tests
%%%=============================================================================

%% Test 1: Simple opening bracket
simple_bracket_test() ->
    {ok, T1} = parse_label_start_link(<<"[">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should have label_link and label_marker events
    LinkEvents = [E || E <- Events, E#event.name =:= label_link],
    ?assertEqual(2, length(LinkEvents)),  % Enter + Exit

    MarkerEvents = [E || E <- Events, E#event.name =:= label_marker],
    ?assertEqual(2, length(MarkerEvents)),  % Enter + Exit

    %% Should have added a label start
    ?assert(erlmd_tokeniser:has_label_starts(T1)),

    %% Check the label start details
    LabelStart = erlmd_tokeniser:peek_label_start(T1),
    ?assertEqual(link, LabelStart#label_start.kind),
    ?assertEqual(false, LabelStart#label_start.inactive).

%% Test 2: Bracket with content after
bracket_with_content_test() ->
    {ok, T1} = parse_label_start_link(<<"[text">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should still have the events
    LinkEvents = [E || E <- Events, E#event.name =:= label_link],
    ?assertEqual(2, length(LinkEvents)),

    %% Should have label start
    ?assert(erlmd_tokeniser:has_label_starts(T1)).

%%%=============================================================================
%%% Negative Tests
%%%=============================================================================

%% Test 3: Not a bracket - should fail
not_bracket_test() ->
    {nok, _T1} = parse_label_start_link(<<"text">>).

%% Test 4: Other characters
other_char_test() ->
    {nok, _T1} = parse_label_start_link(<<"!">>).

%% Test 5: Closing bracket - should fail (only opening brackets)
closing_bracket_test() ->
    {nok, _T1} = parse_label_start_link(<<"]">>).

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

%% Test: Bracket at EOF
bracket_at_eof_test() ->
    {ok, T1} = parse_label_start_link(<<"[">>),
    ?assert(erlmd_tokeniser:has_label_starts(T1)).

%% Test: Empty input
empty_input_test() ->
    {nok, _T} = parse_label_start_link(<<>>).

%% Test: Multiple brackets (only tests first one)
multiple_brackets_test() ->
    {ok, T1} = parse_label_start_link(<<"[[">>),
    %% Should have consumed only the first bracket
    ?assert(erlmd_tokeniser:has_label_starts(T1)).
