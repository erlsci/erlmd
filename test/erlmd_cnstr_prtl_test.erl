%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for partial constructs (label, destination, title).
%%%
%%% These are helper constructs used by label_end.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_prtl_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Partial Label Tests
%%%=============================================================================

parse_label(Input) ->
    T = test_helper:make_tokenizer(Input),
    %% Set token names
    T1 = erlmd_tokeniser:set_token_names(T, reference, reference_marker,
                                          reference_string, undefined, undefined),
    test_helper:run_construct(prtl_label_start, T1).

simple_label_test() ->
    {ok, _T1} = parse_label(<<"[ref]">>).

label_with_spaces_test() ->
    {ok, _T1} = parse_label(<<"[my ref]">>).

label_with_escape_test() ->
    {ok, _T1} = parse_label(<<"[foo\\]bar]">>).

empty_label_test() ->
    {nok, _T} = parse_label(<<"[]">>).

nested_bracket_test() ->
    {nok, _T} = parse_label(<<"[foo[bar]]">>).

%%%=============================================================================
%%% Partial Destination Tests
%%%=============================================================================

parse_destination(Input) ->
    T = test_helper:make_tokenizer(Input),
    T1 = erlmd_tokeniser:set_token_names(T, resource_destination,
                                          resource_destination_literal,
                                          resource_destination_literal_marker,
                                          resource_destination_raw,
                                          resource_destination_string),
    test_helper:run_construct(prtl_destination_start, T1).

enclosed_destination_test() ->
    {ok, _T1} = parse_destination(<<"<http://example.com>">>).

raw_destination_test() ->
    {ok, _T1} = parse_destination(<<"http://example.com">>).

raw_with_parens_test() ->
    {ok, _T1} = parse_destination(<<"http://example.com/path(1)">>).

raw_balanced_parens_test() ->
    {ok, _T1} = parse_destination(<<"url((nested))">>).

destination_no_start_test() ->
    {nok, _T} = parse_destination(<<" http://example.com">>).

%%%=============================================================================
%%% Partial Title Tests
%%%=============================================================================

parse_title(Input) ->
    T = test_helper:make_tokenizer(Input),
    T1 = erlmd_tokeniser:set_token_names(T, resource_title,
                                          resource_title_marker,
                                          resource_title_string,
                                          undefined, undefined),
    test_helper:run_construct(prtl_title_start, T1).

double_quoted_title_test() ->
    {ok, _T1} = parse_title(<<"\"my title\"">>).

single_quoted_title_test() ->
    {ok, _T1} = parse_title(<<"'my title'">>).

paren_title_test() ->
    {ok, _T1} = parse_title(<<"(my title)">>).

empty_title_test() ->
    {ok, _T1} = parse_title(<<"\"\"">>).

title_with_escape_test() ->
    {ok, _T1} = parse_title(<<"\"foo\\\"bar\"">>).

unclosed_title_test() ->
    {nok, _T} = parse_title(<<"\"unclosed">>).

no_quote_test() ->
    {nok, _T} = parse_title(<<"no quote">>).
