%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for paragraph construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_paragraph_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_paragraph(Input) ->
    T = test_helper:make_tokenizer(Input),
    %% Call start directly, then run through state machine if needed
    case erlmd_cnstr_paragraph:start(T) of
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

%% Test 1: Simple single-line paragraph
simple_paragraph_test() ->
    {Result, T1} = parse_paragraph(<<"Hello world">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should succeed
    ?assertEqual(ok, Result),

    %% Should have paragraph events
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParagraphEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(ParagraphEvents))#event.kind).

%% Test 2: Multi-line paragraph (no blank line)
multiline_paragraph_test() ->
    {Result, T1} = parse_paragraph(<<"Line 1\nLine 2">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should succeed
    ?assertEqual(ok, Result),

    %% Should have one paragraph
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParagraphEvents)),  % 1 enter + 1 exit

    %% Should have data events for both lines
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assert(length(DataEvents) >= 2).  % At least enter and exit for each line

%% Test 3: Paragraph at EOF (no trailing newline)
paragraph_at_eof_test() ->
    {Result, T1} = parse_paragraph(<<"Single line">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    ?assertEqual(ok, Result),
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParagraphEvents)).

%% Test 4: Empty input (no paragraph)
empty_input_test() ->
    {Result, _T1} = parse_paragraph(<<>>),
    ?assertEqual(nok, Result).

%% Test 5: Just newline (no paragraph)
just_newline_test() ->
    {Result, _T1} = parse_paragraph(<<"\n">>),
    ?assertEqual(nok, Result).

%% Test 6: Three-line paragraph
three_lines_test() ->
    {Result, T1} = parse_paragraph(<<"Line 1\nLine 2\nLine 3">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    ?assertEqual(ok, Result),

    %% One paragraph
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParagraphEvents)),

    %% Three data sections
    DataEvents = [E || E <- Events, E#event.name =:= data],
    ?assert(length(DataEvents) >= 3).

%% Test 7: Paragraph ending with blank line
with_trailing_blank_test() ->
    {Result, T1} = parse_paragraph(<<"Text\n\n">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    %% Should stop at first newline (before blank line)
    ?assertEqual(ok, Result),

    %% Current position should be at second newline
    ?assertEqual($\n, erlmd_tokenizer:current(T1)).

%% Test 8: Single character paragraph
single_char_test() ->
    {Result, T1} = parse_paragraph(<<"a">>),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    ?assertEqual(ok, Result),
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParagraphEvents)).

%% Test 9: Paragraph with spaces
with_spaces_test() ->
    {Result, _T1} = parse_paragraph(<<"Hello   world">>),
    ?assertEqual(ok, Result).

%% Test 10: Long paragraph
long_paragraph_test() ->
    LongText = binary:copy(<<"word ">>, 100),
    {Result, T1} = parse_paragraph(LongText),
    Events = lists:reverse(erlmd_tokenizer:get_events(T1)),

    ?assertEqual(ok, Result),
    ParagraphEvents = [E || E <- Events, E#event.name =:= paragraph],
    ?assertEqual(2, length(ParagraphEvents)).
