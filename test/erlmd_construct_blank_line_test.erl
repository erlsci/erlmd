%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for blank_line construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_blank_line_test).

-include_lib("eunit/include/eunit.hrl").
-include("../src/types.hrl").
-include("../src/tokenizer_internal.hrl").

%%%=============================================================================
%%% Tests
%%%=============================================================================

empty_line_test() ->
    T = test_helper:make_tokenizer(<<"\n">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_blank_line:start(T1),
    ?assertEqual($\n, erlmd_tokenizer:current(T2)).

just_newline_test() ->
    T = test_helper:make_tokenizer(<<"\n">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, _T2} = erlmd_construct_blank_line:start(T1).

spaces_line_test() ->
    T = test_helper:make_tokenizer(<<"   \n">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_blank_line:start(T1),
    %% Should be at newline after consuming spaces
    ?assertEqual($\n, erlmd_tokenizer:current(T2)).

tabs_line_test() ->
    T = test_helper:make_tokenizer(<<"\t\t\n">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_blank_line:start(T1),
    ?assertEqual($\n, erlmd_tokenizer:current(T2)).

mixed_whitespace_line_test() ->
    T = test_helper:make_tokenizer(<<" \t \n">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_blank_line:start(T1),
    ?assertEqual($\n, erlmd_tokenizer:current(T2)).

not_blank_with_text_test() ->
    T = test_helper:make_tokenizer(<<"  a\n">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {nok, _T2} = erlmd_construct_blank_line:start(T1).

not_blank_no_spaces_test() ->
    T = test_helper:make_tokenizer(<<"abc\n">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {nok, _T2} = erlmd_construct_blank_line:start(T1).

eof_after_whitespace_test() ->
    T = test_helper:make_tokenizer(<<"  ">>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_blank_line:start(T1),
    ?assertEqual(eof, erlmd_tokenizer:current(T2)).

eof_only_test() ->
    T = test_helper:make_tokenizer(<<>>),
    T1 = erlmd_tokenizer:prepare_byte(T),
    {ok, T2} = erlmd_construct_blank_line:start(T1),
    ?assertEqual(eof, erlmd_tokenizer:current(T2)).
