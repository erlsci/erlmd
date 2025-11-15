%%%-----------------------------------------------------------------------------
%%% @doc Unit tests for character reference construct.
%%%
%%% Tests both named and numeric character references.
%%% Implements CommonMark 2.5 test coverage.
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_character_reference_test).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

%%%=============================================================================
%%% Test Helpers
%%%=============================================================================

parse_ref(Input) ->
    T = test_helper:make_tokenizer(Input),
    test_helper:run_construct(character_reference, T).

%%%=============================================================================
%%% Named Reference Tests
%%%=============================================================================

%% Test 1: Simple named reference - amp
named_amp_test() ->
    {ok, T1} = parse_ref(<<"&amp;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),

    %% Should have character_reference events
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)),  % Enter + Exit
    ?assertEqual(enter, (hd(RefEvents))#event.kind),
    ?assertEqual(exit, (lists:last(RefEvents))#event.kind).

%% Test 2: Named reference - lt
named_lt_test() ->
    {ok, T1} = parse_ref(<<"&lt;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 3: Named reference - copy
named_copy_test() ->
    {ok, T1} = parse_ref(<<"&copy;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 4: Named reference with numbers
named_with_numbers_test() ->
    %% &frac12; (½)
    {ok, _T1} = parse_ref(<<"&frac12;">>).

%%%=============================================================================
%%% Decimal Numeric Reference Tests
%%%=============================================================================

%% Test 5: Decimal reference - simple
decimal_simple_test() ->
    %% &#35; = #
    {ok, T1} = parse_ref(<<"&#35;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 6: Decimal reference - multi-digit
decimal_multi_test() ->
    %% &#169; = ©
    {ok, T1} = parse_ref(<<"&#169;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 7: Decimal reference - large (emoji)
decimal_emoji_test() ->
    %% &#128169; = 💩
    {ok, T1} = parse_ref(<<"&#128169;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%%%=============================================================================
%%% Hexadecimal Numeric Reference Tests
%%%=============================================================================

%% Test 8: Hex reference - lowercase x
hex_lowercase_x_test() ->
    %% &#x23; = #
    {ok, T1} = parse_ref(<<"&#x23;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 9: Hex reference - uppercase X
hex_uppercase_x_test() ->
    %% &#X23; = #
    {ok, T1} = parse_ref(<<"&#X23;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 10: Hex reference - lowercase digits
hex_lowercase_digits_test() ->
    %% &#xa9; = ©
    {ok, T1} = parse_ref(<<"&#xa9;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 11: Hex reference - uppercase digits
hex_uppercase_digits_test() ->
    %% &#XA9; = ©
    {ok, T1} = parse_ref(<<"&#XA9;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%% Test 12: Hex reference - emoji
hex_emoji_test() ->
    %% &#x1F4A9; = 💩
    {ok, T1} = parse_ref(<<"&#x1F4A9;">>),
    Events = lists:reverse(erlmd_tokeniser:get_events(T1)),
    RefEvents = [E || E <- Events, E#event.name =:= character_reference],
    ?assertEqual(2, length(RefEvents)).

%%%=============================================================================
%%% Negative Tests
%%%=============================================================================

%% Test 13: Missing semicolon - named
missing_semicolon_named_test() ->
    {nok, _T1} = parse_ref(<<"&amp">>).

%% Test 14: Missing semicolon - decimal
missing_semicolon_decimal_test() ->
    {nok, _T1} = parse_ref(<<"&#35">>).

%% Test 15: Missing semicolon - hex
missing_semicolon_hex_test() ->
    {nok, _T1} = parse_ref(<<"&#x23">>).

%% Test 16: Empty reference
empty_reference_test() ->
    {nok, _T1} = parse_ref(<<"&;">>).

%% Test 17: Empty decimal
empty_decimal_test() ->
    {nok, _T1} = parse_ref(<<"&#;">>).

%% Test 18: Empty hex
empty_hex_test() ->
    {nok, _T1} = parse_ref(<<"&#x;">>).

%% Test 19: Invalid character in named
invalid_char_named_test() ->
    {nok, _T1} = parse_ref(<<"&am-p;">>).

%% Test 20: No ampersand
no_ampersand_test() ->
    {nok, _T1} = parse_ref(<<"amp;">>).

%%%=============================================================================
%%% CommonMark Examples
%%%=============================================================================

%% Example 304: Named entities
commonmark_304_test() ->
    {ok, _T1} = parse_ref(<<"&nbsp;">>),
    {ok, _T2} = parse_ref(<<"&amp;">>),
    {ok, _T3} = parse_ref(<<"&copy;">>),
    {ok, _T4} = parse_ref(<<"&AElig;">>).

%% Example 305: Decimal numeric
commonmark_305_test() ->
    {ok, _T1} = parse_ref(<<"&#35;">>),
    {ok, _T2} = parse_ref(<<"&#1234;">>).

%% Example 306: Hex numeric
commonmark_306_test() ->
    {ok, _T1} = parse_ref(<<"&#X22;">>),
    {ok, _T2} = parse_ref(<<"&#x1F4A9;">>).

%% Example 307: Invalid - no semicolon
commonmark_307_test() ->
    %% &nbsp &x; (not valid - missing semicolon)
    {nok, _T1} = parse_ref(<<"&nbsp">>).

%%%=============================================================================
%%% Edge Cases
%%%=============================================================================

%% Test: Very long numeric value (should fail - max 7 decimal digits)
very_long_decimal_test() ->
    %% 8 digits should fail (max is 7)
    {nok, _T1} = parse_ref(<<"&#12345678;">>).

%% Test: Very long hex value (should fail - max 6 hex digits)
very_long_hex_test() ->
    %% 7 hex digits should fail (max is 6)
    {nok, _T1} = parse_ref(<<"&#x1234567;">>).

%% Test: Case sensitivity in hex
hex_case_test() ->
    {ok, _T1} = parse_ref(<<"&#xaBcDeF;">>),
    {ok, _T2} = parse_ref(<<"&#XABCDEF;">>).
