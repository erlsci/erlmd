%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for character reference utilities.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_util_char_ref_test).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Named Entity Tests
%%%=============================================================================

decode_named_amp_test() ->
    ?assertEqual({ok, <<"&">>}, erlmd_util_char_ref:decode_named(<<"amp">>)).

decode_named_lt_test() ->
    ?assertEqual({ok, <<"<">>}, erlmd_util_char_ref:decode_named(<<"lt">>)).

decode_named_gt_test() ->
    ?assertEqual({ok, <<">">>}, erlmd_util_char_ref:decode_named(<<"gt">>)).

decode_named_copy_test() ->
    ?assertEqual({ok, <<"\u00A9">>}, erlmd_util_char_ref:decode_named(<<"copy">>)).

decode_named_not_found_test() ->
    ?assertEqual({error, not_found}, erlmd_util_char_ref:decode_named(<<"unknown">>)).

%%%=============================================================================
%%% Numeric Reference Tests - Decimal
%%%=============================================================================

decode_decimal_simple_test() ->
    %% &#35; = #
    ?assertEqual({ok, <<"#">>}, erlmd_util_char_ref:decode_numeric(decimal, <<"35">>)).

decode_decimal_unicode_test() ->
    %% &#169; = ©
    ?assertEqual({ok, <<"Â©">>}, erlmd_util_char_ref:decode_numeric(decimal, <<"169">>)).

decode_decimal_emoji_test() ->
    %% &#128169; = 💩 (UTF-8 encoded)
    ?assertEqual({ok, <<240,159,146,169>>}, erlmd_util_char_ref:decode_numeric(decimal, <<"128169">>)).

decode_decimal_null_test() ->
    %% &#0; is invalid (NULL)
    ?assertEqual({error, invalid_codepoint}, erlmd_util_char_ref:decode_numeric(decimal, <<"0">>)).

decode_decimal_surrogate_test() ->
    %% &#55296; = U+D800 (surrogate, invalid)
    ?assertEqual({error, invalid_codepoint}, erlmd_util_char_ref:decode_numeric(decimal, <<"55296">>)).

decode_decimal_too_large_test() ->
    %% &#1114112; = U+110000 (beyond Unicode range)
    ?assertEqual({error, invalid_codepoint}, erlmd_util_char_ref:decode_numeric(decimal, <<"1114112">>)).

%%%=============================================================================
%%% Numeric Reference Tests - Hexadecimal
%%%=============================================================================

decode_hex_simple_test() ->
    %% &#x23; = #
    ?assertEqual({ok, <<"#">>}, erlmd_util_char_ref:decode_numeric(hex, <<"23">>)).

decode_hex_unicode_test() ->
    %% &#xA9; = ©
    ?assertEqual({ok, <<"Â©">>}, erlmd_util_char_ref:decode_numeric(hex, <<"A9">>)).

decode_hex_lowercase_test() ->
    %% &#xa9; = ©
    ?assertEqual({ok, <<"Â©">>}, erlmd_util_char_ref:decode_numeric(hex, <<"a9">>)).

decode_hex_emoji_test() ->
    %% &#x1F4A9; = 💩 (UTF-8 encoded)
    ?assertEqual({ok, <<240,159,146,169>>}, erlmd_util_char_ref:decode_numeric(hex, <<"1F4A9">>)).

decode_hex_null_test() ->
    %% &#x0; is invalid
    ?assertEqual({error, invalid_codepoint}, erlmd_util_char_ref:decode_numeric(hex, <<"0">>)).

%%%=============================================================================
%%% Validation Tests
%%%=============================================================================

is_valid_codepoint_valid_test() ->
    ?assert(erlmd_util_char_ref:is_valid_codepoint($A)),
    ?assert(erlmd_util_char_ref:is_valid_codepoint(16#A9)),
    ?assert(erlmd_util_char_ref:is_valid_codepoint(16#1F4A9)).

is_valid_codepoint_null_test() ->
    ?assertNot(erlmd_util_char_ref:is_valid_codepoint(0)).

is_valid_codepoint_surrogate_test() ->
    ?assertNot(erlmd_util_char_ref:is_valid_codepoint(16#D800)),
    ?assertNot(erlmd_util_char_ref:is_valid_codepoint(16#DFFF)).

is_valid_codepoint_too_large_test() ->
    ?assertNot(erlmd_util_char_ref:is_valid_codepoint(16#110000)).

%%%=============================================================================
%%% Utility Tests
%%%=============================================================================

value_max_test() ->
    ?assertEqual(7, erlmd_util_char_ref:value_max(decimal)),
    ?assertEqual(6, erlmd_util_char_ref:value_max(hex)).

value_test_decimal_test() ->
    Test = erlmd_util_char_ref:value_test(decimal),
    ?assert(Test($0)),
    ?assert(Test($5)),
    ?assert(Test($9)),
    ?assertNot(Test($a)),
    ?assertNot(Test($A)),
    ?assertNot(Test($-)).

value_test_hex_test() ->
    Test = erlmd_util_char_ref:value_test(hex),
    ?assert(Test($0)),
    ?assert(Test($9)),
    ?assert(Test($a)),
    ?assert(Test($f)),
    ?assert(Test($A)),
    ?assert(Test($F)),
    ?assertNot(Test($g)),
    ?assertNot(Test($G)),
    ?assertNot(Test($-)).
