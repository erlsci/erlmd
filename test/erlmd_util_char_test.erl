%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for character classification utilities.
%%%
%%% Tests ASCII and Unicode character classification functions.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_util_char_test).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% ASCII Whitespace Tests
%%%=============================================================================

ascii_whitespace_space_test() ->
    ?assert(erlmd_util_char:is_ascii_whitespace($ )).

ascii_whitespace_tab_test() ->
    ?assert(erlmd_util_char:is_ascii_whitespace($\t)).

ascii_whitespace_newline_test() ->
    ?assert(erlmd_util_char:is_ascii_whitespace($\n)).

ascii_whitespace_carriage_return_test() ->
    ?assert(erlmd_util_char:is_ascii_whitespace($\r)).

ascii_whitespace_negative_test() ->
    ?assertNot(erlmd_util_char:is_ascii_whitespace($a)),
    ?assertNot(erlmd_util_char:is_ascii_whitespace($1)),
    ?assertNot(erlmd_util_char:is_ascii_whitespace($*)),
    ?assertNot(erlmd_util_char:is_ascii_whitespace($\0)).

%%%=============================================================================
%%% ASCII Punctuation Tests
%%%=============================================================================

ascii_punctuation_exclamation_to_slash_test() ->
    %% Test range ! to / (0x21-0x2F)
    ?assert(erlmd_util_char:is_ascii_punctuation($!)),
    ?assert(erlmd_util_char:is_ascii_punctuation($")),
    ?assert(erlmd_util_char:is_ascii_punctuation($#)),
    ?assert(erlmd_util_char:is_ascii_punctuation($$)),
    ?assert(erlmd_util_char:is_ascii_punctuation($%)),
    ?assert(erlmd_util_char:is_ascii_punctuation($&)),
    ?assert(erlmd_util_char:is_ascii_punctuation($')),
    ?assert(erlmd_util_char:is_ascii_punctuation($()),
    ?assert(erlmd_util_char:is_ascii_punctuation($))),
    ?assert(erlmd_util_char:is_ascii_punctuation($*)),
    ?assert(erlmd_util_char:is_ascii_punctuation($+)),
    ?assert(erlmd_util_char:is_ascii_punctuation($,)),
    ?assert(erlmd_util_char:is_ascii_punctuation($-)),
    ?assert(erlmd_util_char:is_ascii_punctuation($.)),
    ?assert(erlmd_util_char:is_ascii_punctuation($/)).

ascii_punctuation_colon_to_at_test() ->
    %% Test range : to @ (0x3A-0x40)
    ?assert(erlmd_util_char:is_ascii_punctuation($:)),
    ?assert(erlmd_util_char:is_ascii_punctuation($;)),
    ?assert(erlmd_util_char:is_ascii_punctuation($<)),
    ?assert(erlmd_util_char:is_ascii_punctuation($=)),
    ?assert(erlmd_util_char:is_ascii_punctuation($>)),
    ?assert(erlmd_util_char:is_ascii_punctuation($?)),
    ?assert(erlmd_util_char:is_ascii_punctuation($@)).

ascii_punctuation_bracket_to_backtick_test() ->
    %% Test range [ to ` (0x5B-0x60)
    ?assert(erlmd_util_char:is_ascii_punctuation($[)),
    ?assert(erlmd_util_char:is_ascii_punctuation($\\)),
    ?assert(erlmd_util_char:is_ascii_punctuation($])),
    ?assert(erlmd_util_char:is_ascii_punctuation($^)),
    ?assert(erlmd_util_char:is_ascii_punctuation($_)),
    ?assert(erlmd_util_char:is_ascii_punctuation($`)).

ascii_punctuation_brace_to_tilde_test() ->
    %% Test range { to ~ (0x7B-0x7E)
    ?assert(erlmd_util_char:is_ascii_punctuation(${)),
    ?assert(erlmd_util_char:is_ascii_punctuation($|)),
    ?assert(erlmd_util_char:is_ascii_punctuation($})),
    ?assert(erlmd_util_char:is_ascii_punctuation($~)).

ascii_punctuation_negative_test() ->
    ?assertNot(erlmd_util_char:is_ascii_punctuation($a)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($Z)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($0)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($9)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($ )),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($\n)).

%%%=============================================================================
%%% ASCII Alphanumeric Tests
%%%=============================================================================

ascii_alphanumeric_digits_test() ->
    ?assert(erlmd_util_char:is_ascii_alphanumeric($0)),
    ?assert(erlmd_util_char:is_ascii_alphanumeric($1)),
    ?assert(erlmd_util_char:is_ascii_alphanumeric($5)),
    ?assert(erlmd_util_char:is_ascii_alphanumeric($9)).

ascii_alphanumeric_uppercase_test() ->
    ?assert(erlmd_util_char:is_ascii_alphanumeric($A)),
    ?assert(erlmd_util_char:is_ascii_alphanumeric($M)),
    ?assert(erlmd_util_char:is_ascii_alphanumeric($Z)).

ascii_alphanumeric_lowercase_test() ->
    ?assert(erlmd_util_char:is_ascii_alphanumeric($a)),
    ?assert(erlmd_util_char:is_ascii_alphanumeric($m)),
    ?assert(erlmd_util_char:is_ascii_alphanumeric($z)).

ascii_alphanumeric_negative_test() ->
    ?assertNot(erlmd_util_char:is_ascii_alphanumeric($!)),
    ?assertNot(erlmd_util_char:is_ascii_alphanumeric($*)),
    ?assertNot(erlmd_util_char:is_ascii_alphanumeric($ )),
    ?assertNot(erlmd_util_char:is_ascii_alphanumeric($\n)).

%%%=============================================================================
%%% Character Classification Tests
%%%=============================================================================

classify_whitespace_test() ->
    ?assertEqual(whitespace, erlmd_util_char:classify($ )),
    ?assertEqual(whitespace, erlmd_util_char:classify($\t)),
    ?assertEqual(whitespace, erlmd_util_char:classify($\n)),
    ?assertEqual(whitespace, erlmd_util_char:classify($\r)).

classify_punctuation_test() ->
    ?assertEqual(punctuation, erlmd_util_char:classify($!)),
    ?assertEqual(punctuation, erlmd_util_char:classify($*)),
    ?assertEqual(punctuation, erlmd_util_char:classify($_)),
    ?assertEqual(punctuation, erlmd_util_char:classify($`)),
    ?assertEqual(punctuation, erlmd_util_char:classify(${)).

classify_other_test() ->
    ?assertEqual(other, erlmd_util_char:classify($a)),
    ?assertEqual(other, erlmd_util_char:classify($Z)),
    ?assertEqual(other, erlmd_util_char:classify($0)),
    ?assertEqual(other, erlmd_util_char:classify($9)).

classify_byte_whitespace_test() ->
    ?assertEqual(whitespace, erlmd_util_char:classify_byte($ )),
    ?assertEqual(whitespace, erlmd_util_char:classify_byte($\t)),
    ?assertEqual(whitespace, erlmd_util_char:classify_byte($\n)),
    ?assertEqual(whitespace, erlmd_util_char:classify_byte($\r)).

classify_byte_punctuation_test() ->
    ?assertEqual(punctuation, erlmd_util_char:classify_byte($!)),
    ?assertEqual(punctuation, erlmd_util_char:classify_byte($*)),
    ?assertEqual(punctuation, erlmd_util_char:classify_byte($_)).

classify_byte_other_test() ->
    ?assertEqual(other, erlmd_util_char:classify_byte($a)),
    ?assertEqual(other, erlmd_util_char:classify_byte($5)).

classify_byte_non_ascii_test() ->
    %% Non-ASCII bytes (>= 128) should be classified as 'other' in Phase 1
    ?assertEqual(other, erlmd_util_char:classify_byte(200)).

%%%=============================================================================
%%% kind_after_index Tests
%%%=============================================================================

kind_after_index_test() ->
    Bytes = <<"Hello *world*">>,
    ?assertEqual(other, erlmd_util_char:kind_after_index(Bytes, 0)),      % H
    ?assertEqual(other, erlmd_util_char:kind_after_index(Bytes, 1)),      % e
    ?assertEqual(other, erlmd_util_char:kind_after_index(Bytes, 4)),      % o
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 5)), % space
    ?assertEqual(punctuation, erlmd_util_char:kind_after_index(Bytes, 6)), % *
    ?assertEqual(other, erlmd_util_char:kind_after_index(Bytes, 7)),      % w
    ?assertEqual(punctuation, erlmd_util_char:kind_after_index(Bytes, 12)). % *

kind_after_index_eof_test() ->
    Bytes = <<"abc">>,
    %% Beyond end of binary should return whitespace (EOF treated as whitespace)
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 3)),
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 99)),
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 1000)).

kind_after_index_empty_test() ->
    Bytes = <<>>,
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 0)),
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 1)).

kind_after_index_complex_test() ->
    Bytes = <<"# Heading\n  \t  *emphasis*">>,
    ?assertEqual(punctuation, erlmd_util_char:kind_after_index(Bytes, 0)), % #
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 1)),  % space
    ?assertEqual(other, erlmd_util_char:kind_after_index(Bytes, 2)),       % H
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 9)),  % \n
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 10)), % space
    ?assertEqual(whitespace, erlmd_util_char:kind_after_index(Bytes, 12)), % \t
    ?assertEqual(punctuation, erlmd_util_char:kind_after_index(Bytes, 15)). % *

%%%=============================================================================
%%% Unicode Tests (Basic for Phase 1)
%%%=============================================================================

unicode_whitespace_test() ->
    %% ASCII whitespace should work
    ?assert(erlmd_util_char:is_unicode_whitespace($ )),
    ?assert(erlmd_util_char:is_unicode_whitespace($\t)),
    ?assert(erlmd_util_char:is_unicode_whitespace($\n)),
    ?assert(erlmd_util_char:is_unicode_whitespace($\r)).

unicode_whitespace_negative_test() ->
    ?assertNot(erlmd_util_char:is_unicode_whitespace($a)),
    ?assertNot(erlmd_util_char:is_unicode_whitespace($*)).

unicode_punctuation_ascii_test() ->
    %% ASCII punctuation should work
    ?assert(erlmd_util_char:is_unicode_punctuation($!)),
    ?assert(erlmd_util_char:is_unicode_punctuation($*)).

unicode_punctuation_negative_test() ->
    ?assertNot(erlmd_util_char:is_unicode_punctuation($a)),
    ?assertNot(erlmd_util_char:is_unicode_punctuation($ )).

%%%=============================================================================
%%% Edge Cases and Boundary Tests
%%%=============================================================================

edge_case_null_byte_test() ->
    ?assertNot(erlmd_util_char:is_ascii_whitespace($\0)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation($\0)),
    ?assertNot(erlmd_util_char:is_ascii_alphanumeric($\0)),
    ?assertEqual(other, erlmd_util_char:classify_byte($\0)).

edge_case_del_byte_test() ->
    %% DEL character (127)
    ?assertNot(erlmd_util_char:is_ascii_whitespace(127)),
    ?assertNot(erlmd_util_char:is_ascii_punctuation(127)),
    ?assertNot(erlmd_util_char:is_ascii_alphanumeric(127)),
    ?assertEqual(other, erlmd_util_char:classify_byte(127)).

edge_case_boundary_chars_test() ->
    %% Test boundaries between punctuation ranges
    ?assert(erlmd_util_char:is_ascii_punctuation($!)),     % 0x21 - first in range
    ?assert(erlmd_util_char:is_ascii_punctuation($/)),     % 0x2F - last in range
    ?assertNot(erlmd_util_char:is_ascii_punctuation($0)),  % 0x30 - just after
    ?assertNot(erlmd_util_char:is_ascii_punctuation($ )),  % 0x20 - just before

    ?assert(erlmd_util_char:is_ascii_punctuation($:)),     % 0x3A - first in range
    ?assert(erlmd_util_char:is_ascii_punctuation($@)),     % 0x40 - last in range
    ?assertNot(erlmd_util_char:is_ascii_punctuation($A)),  % 0x41 - just after
    ?assertNot(erlmd_util_char:is_ascii_punctuation($9)).  % 0x39 - just before
