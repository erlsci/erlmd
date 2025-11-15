%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for blank_line construct.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_blank_line_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% Tests
%%%=============================================================================

empty_line_test() ->
    T = test_helper:make_tokenizer(<<"\n">>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T),

    %% Should succeed and be at the newline
    ?assertEqual($\n, erlmd_tokeniser:current(T1)).

just_newline_test() ->
    T = test_helper:make_tokenizer(<<"\n">>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T),

    %% Should succeed
    ?assertEqual($\n, erlmd_tokeniser:current(T1)).

spaces_line_test() ->
    T = test_helper:make_tokenizer(<<"   \n">>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T),

    %% Should succeed and be at newline
    %% (blank_line doesn't emit events, it just validates structure)
    ?assertEqual($\n, erlmd_tokeniser:current(T1)).

tabs_line_test() ->
    T = test_helper:make_tokenizer(<<"\t\t\n">>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T),

    %% Should succeed and be at newline
    ?assertEqual($\n, erlmd_tokeniser:current(T1)).

mixed_whitespace_line_test() ->
    T = test_helper:make_tokenizer(<<" \t \t\n">>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T),

    %% Should succeed
    ?assertEqual($\n, erlmd_tokeniser:current(T1)).

not_blank_with_text_test() ->
    T = test_helper:make_tokenizer(<<"  a\n">>),
    {nok, _T1} = test_helper:run_construct(blank_line_start, T).

not_blank_no_spaces_test() ->
    T = test_helper:make_tokenizer(<<"abc\n">>),
    {nok, _T1} = test_helper:run_construct(blank_line_start, T).

eof_after_whitespace_test() ->
    T = test_helper:make_tokenizer(<<"  ">>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T),

    %% Should succeed
    ?assertEqual(eof, erlmd_tokeniser:current(T1)).

eof_only_test() ->
    T = test_helper:make_tokenizer(<<>>),
    {ok, T1} = test_helper:run_construct(blank_line_start, T),

    %% Empty file is a blank line
    ?assertEqual(eof, erlmd_tokeniser:current(T1)).
