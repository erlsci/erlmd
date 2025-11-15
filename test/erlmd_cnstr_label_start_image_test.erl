%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for label_start_image construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_label_start_image_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_label_start_image(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(label_start_image, T).

%%%=============================================================================
%%% Basic Functionality Tests
%%%=============================================================================

%% Test 1: Simple image start ![
simple_image_start_test() ->
    {ok, T1} = parse_label_start_image(<<"![">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should have label_image events
    ImageEvents = [E || E <- Events, E#event.name =:= label_image],
    ?assertEqual(2, length(ImageEvents)),  % Enter + Exit

    %% Should have label_image_marker for !
    ImageMarkerEvents = [E || E <- Events, E#event.name =:= label_image_marker],
    ?assertEqual(2, length(ImageMarkerEvents)),

    %% Should have label_marker for [
    MarkerEvents = [E || E <- Events, E#event.name =:= label_marker],
    ?assertEqual(2, length(MarkerEvents)),

    %% Should have added a label start
    ?assert(erlmd_tokeniser:has_label_starts(T1)),

    %% Check the label start details
    LabelStart = erlmd_tokeniser:peek_label_start(T1),
    ?assertEqual(image, LabelStart#label_start.kind),
    ?assertEqual(false, LabelStart#label_start.inactive).

%% Test 2: Image start with content
image_with_content_test() ->
    {ok, T1} = parse_label_start_image(<<"![alt">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    ImageEvents = [E || E <- Events, E#event.name =:= label_image],
    ?assertEqual(2, length(ImageEvents)),

    ?assert(erlmd_tokeniser:has_label_starts(T1)).

%%%=============================================================================
%%% Negative Tests
%%%=============================================================================

%% Test 3: Just ! without [ - should fail
just_exclamation_test() ->
    {nok, _T1} = parse_label_start_image(<<"!">>).

%% Test 4: ! with something other than [ - should fail
exclamation_other_test() ->
    {nok, _T1} = parse_label_start_image(<<"!a">>).

%% Test 5: No ! at start - should fail
no_exclamation_test() ->
    {nok, _T1} = parse_label_start_image(<<"[">>).

%% Test 6: ![^ pattern - should fail (GFM footnote conflict)
gfm_footnote_conflict_test() ->
    {nok, _T1} = parse_label_start_image(<<"![^">>).

%% Test 7: Other characters
other_char_test() ->
    {nok, _T1} = parse_label_start_image(<<"text">>).

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

%% Test: ![ at EOF
image_at_eof_test() ->
    {ok, T1} = parse_label_start_image(<<"![">>),
    ?assert(erlmd_tokeniser:has_label_starts(T1)).

%% Test: Empty input
empty_input_test() ->
    {nok, _T} = parse_label_start_image(<<>>).

%% Test: Just ! at EOF
just_exclamation_eof_test() ->
    {nok, _T} = parse_label_start_image(<<"!">>).

%% Test: ![ followed by normal character (not ^)
normal_after_bracket_test() ->
    {ok, T1} = parse_label_start_image(<<"![a">>),
    ?assert(erlmd_tokeniser:has_label_starts(T1)),
    LabelStart = erlmd_tokeniser:peek_label_start(T1),
    ?assertEqual(image, LabelStart#label_start.kind).
