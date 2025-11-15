%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for attention construct (emphasis/strong marking).
%%%
%%% Tests:
%%% - Single marker sequences: *, _
%%% - Multiple marker sequences: **, ***, ****, __, ___, ____
%%% - Mixed content
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_attention_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_inline(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(text, T).

count_attention_sequences(T) ->
    Events = erlmd_tokeniser:get_events(T),
    %% Count only enter events (each sequence has enter + exit, we want the count of sequences)
    length([E || E <- Events, E#event.name =:= attention_sequence, E#event.kind =:= enter]).

%%%=============================================================================
%%% Single Marker Tests
%%%=============================================================================

single_asterisk_test() ->
    {ok, T} = parse_inline(<<"*">>),
    %% Should create an attention_sequence event
    ?assertEqual(1, count_attention_sequences(T)).

single_underscore_test() ->
    {ok, T} = parse_inline(<<"_">>),
    ?assertEqual(1, count_attention_sequences(T)).

%%%=============================================================================
%%% Multiple Marker Tests
%%%=============================================================================

double_asterisk_test() ->
    {ok, T} = parse_inline(<<"**">>),
    %% Should create one attention_sequence event for both markers
    ?assertEqual(1, count_attention_sequences(T)).

triple_asterisk_test() ->
    {ok, T} = parse_inline(<<"***">>),
    ?assertEqual(1, count_attention_sequences(T)).

quad_asterisk_test() ->
    {ok, T} = parse_inline(<<"****">>),
    ?assertEqual(1, count_attention_sequences(T)).

double_underscore_test() ->
    {ok, T} = parse_inline(<<"__">>),
    ?assertEqual(1, count_attention_sequences(T)).

triple_underscore_test() ->
    {ok, T} = parse_inline(<<"___">>),
    ?assertEqual(1, count_attention_sequences(T)).

%%%=============================================================================
%%% Emphasis/Strong Pattern Tests
%%%=============================================================================

emphasis_pattern_test() ->
    {ok, T} = parse_inline(<<"*text*">>),
    %% Should have 2 attention sequences (open and close)
    ?assertEqual(2, count_attention_sequences(T)).

strong_pattern_test() ->
    {ok, T} = parse_inline(<<"**text**">>),
    ?assertEqual(2, count_attention_sequences(T)).

emphasis_underscore_test() ->
    {ok, T} = parse_inline(<<"_text_">>),
    ?assertEqual(2, count_attention_sequences(T)).

strong_underscore_test() ->
    {ok, T} = parse_inline(<<"__text__">>),
    ?assertEqual(2, count_attention_sequences(T)).

%%%=============================================================================
%%% Mixed Content Tests
%%%=============================================================================

multiple_emphasis_test() ->
    {ok, T} = parse_inline(<<"*one* and *two*">>),
    %% Should have 4 attention sequences (2 pairs)
    ?assertEqual(4, count_attention_sequences(T)).

nested_pattern_test() ->
    {ok, T} = parse_inline(<<"***both***">>),
    %% Should have 2 attention sequences (3 markers on each side)
    ?assertEqual(2, count_attention_sequences(T)).

mixed_markers_test() ->
    {ok, T} = parse_inline(<<"*asterisk* and _underscore_">>),
    %% Should have 4 attention sequences (2 different pairs)
    ?assertEqual(4, count_attention_sequences(T)).

%%%=============================================================================
%%% Marker Separation Tests
%%%=============================================================================

separated_markers_test() ->
    {ok, T} = parse_inline(<<"* * *">>),
    %% Each asterisk is separate, so 3 attention sequences
    ?assertEqual(3, count_attention_sequences(T)).

mixed_consecutive_test() ->
    {ok, T} = parse_inline(<<"** and ***">>),
    %% Two separate sequences
    ?assertEqual(2, count_attention_sequences(T)).
