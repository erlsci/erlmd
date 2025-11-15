%%%-----------------------------------------------------------------------------
%%% @doc Character reference decoding utilities.
%%%
%%% Provides functions for decoding numeric and named character references.
%%% Implements CommonMark 2.5 "Entity and numeric character references".
%%%
%%% Supports:
%%% - Named entities: &amp; &lt; &copy; etc.
%%% - Decimal numeric: &#35; &#1234;
%%% - Hexadecimal numeric: &#x22; &#X1F4A9;
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_util_char_ref).

-export([
    decode_named/1,
    decode_numeric/2,
    value_max/1,
    value_test/1,
    is_valid_codepoint/1
]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec decode_named(binary()) -> {ok, binary()} | {error, not_found}.
%% @doc Decode a named character reference.
%%
%% The input should be the entity name without & prefix or ; suffix.
%% Returns {ok, Value} if the entity is found, {error, not_found} otherwise.
%%
%% Examples:
%%   decode_named(<<"amp">>) -> {ok, <<"&">>}
%%   decode_named(<<"unknown">>) -> {error, not_found}
decode_named(Name) when is_binary(Name) ->
    case erlmd_util_entity_table:lookup(Name) of
        {ok, Value} -> {ok, Value};
        not_found -> {error, not_found}
    end.

-spec decode_numeric(hex | decimal, binary()) -> {ok, binary()} | {error, term()}.
%% @doc Decode a numeric character reference.
%%
%% The input should be the numeric value as a binary string (without &#, &#x, or ; delimiters).
%%
%% Examples:
%%   decode_numeric(decimal, <<"35">>) -> {ok, <<"#">>}
%%   decode_numeric(hex, <<"1F4A9">>) -> {ok, <<"\u{1F4A9}">>}
%%   decode_numeric(decimal, <<"0">>) -> {error, invalid_codepoint}
decode_numeric(Type, Value) when is_binary(Value) ->
    try
        %% Parse the number
        Codepoint = case Type of
            decimal -> binary_to_integer(Value, 10);
            hex -> binary_to_integer(Value, 16)
        end,

        %% Validate the codepoint
        case is_valid_codepoint(Codepoint) of
            true ->
                %% Convert to UTF-8 binary
                {ok, <<Codepoint/utf8>>};
            false ->
                {error, invalid_codepoint}
        end
    catch
        error:badarg ->
            {error, invalid_format}
    end.

-spec value_max(hex | decimal) -> pos_integer().
%% @doc Maximum number of digits allowed for numeric references.
%%
%% Returns 7 for decimal, 6 for hexadecimal.
%% These limits prevent DoS attacks with extremely long numeric references.
value_max(decimal) -> 7;
value_max(hex) -> 6.

-spec value_test(hex | decimal) -> fun((byte()) -> boolean()).
%% @doc Returns a function that tests if a byte is a valid digit for the type.
%%
%% For decimal: tests if byte is 0-9
%% For hex: tests if byte is 0-9, a-f, A-F
value_test(decimal) ->
    fun(Byte) -> Byte >= $0 andalso Byte =< $9 end;
value_test(hex) ->
    fun(Byte) ->
        (Byte >= $0 andalso Byte =< $9) orelse
        (Byte >= $a andalso Byte =< $f) orelse
        (Byte >= $A andalso Byte =< $F)
    end.

-spec is_valid_codepoint(integer()) -> boolean().
%% @doc Check if a codepoint is valid for Unicode.
%%
%% Invalid codepoints:
%% - NULL (0)
%% - Surrogates (U+D800 to U+DFFF)
%% - Above Unicode range (> U+10FFFF)
%%
%% Note: Control characters and noncharacters are technically valid
%% in Unicode, though they may not render.
is_valid_codepoint(0) -> false;  % NULL
is_valid_codepoint(Codepoint) when Codepoint >= 16#D800, Codepoint =< 16#DFFF -> false;  % Surrogates
is_valid_codepoint(Codepoint) when Codepoint > 16#10FFFF -> false;  % Above Unicode max
is_valid_codepoint(Codepoint) when Codepoint > 0, Codepoint =< 16#10FFFF -> true;
is_valid_codepoint(_) -> false.
