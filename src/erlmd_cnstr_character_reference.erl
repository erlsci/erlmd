%%%-----------------------------------------------------------------------------
%%% @doc Character reference construct.
%%%
%%% Handles both named and numeric character references.
%%% Implements CommonMark 2.5 "Entity and numeric character references".
%%%
%%% Supports:
%%% - Named: &amp; &lt; &copy; (any HTML5 entity)
%%% - Decimal: &#35; &#169;
%%% - Hexadecimal: &#x23; &#X1F4A9;
%%%
%%% All valid references MUST end with semicolon.
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_cnstr_character_reference).

-export([start/1, open/1, numeric/1, value/1]).

-include("types.hrl").

-define(MAX_NAMED_LENGTH, 31).

%%%=============================================================================
%%% State Functions
%%%=============================================================================

%% @doc Entry point - checks for ampersand
start(T) ->
    case erlmd_tokeniser:current(T) of
        $& ->
            T1 = erlmd_tokeniser:enter(T, character_reference),
            T2 = erlmd_tokeniser:enter(T1, character_reference_marker),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, character_reference_marker),
            {{next, character_reference_open}, T4};
        _ ->
            {nok, T}
    end.

%% @doc After &, check if numeric or named
open(T) ->
    case erlmd_tokeniser:current(T) of
        $# ->
            %% Numeric reference - mark as numeric and consume #
            T1 = erlmd_tokeniser:set_state(T, char_ref_type, numeric),
            T2 = erlmd_tokeniser:enter(T1, character_reference_marker_numeric),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, character_reference_marker_numeric),
            {{next, character_reference_numeric}, T4};

        C when (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) ->
            %% Named reference - start collecting
            T1 = erlmd_tokeniser:set_state(T, char_ref_type, named),
            T2 = erlmd_tokeniser:set_state(T1, char_ref_size, 0),
            T3 = erlmd_tokeniser:enter(T2, character_reference_value),
            {{retry, character_reference_value}, T3};

        _ ->
            %% Not a valid character reference
            {nok, T}
    end.

%% @doc After &#, check if hex or decimal
numeric(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $x orelse C =:= $X ->
            %% Hexadecimal
            T1 = erlmd_tokeniser:set_state(T, char_ref_num_type, hex),
            T2 = erlmd_tokeniser:set_state(T1, char_ref_size, 0),
            T3 = erlmd_tokeniser:enter(T2, character_reference_marker_hexadecimal),
            T4 = erlmd_tokeniser:consume(T3),
            T5 = erlmd_tokeniser:exit(T4, character_reference_marker_hexadecimal),
            T6 = erlmd_tokeniser:enter(T5, character_reference_value),
            {{next, character_reference_value}, T6};

        C when C >= $0 andalso C =< $9 ->
            %% Decimal
            T1 = erlmd_tokeniser:set_state(T, char_ref_num_type, decimal),
            T2 = erlmd_tokeniser:set_state(T1, char_ref_size, 0),
            T3 = erlmd_tokeniser:enter(T2, character_reference_value),
            {{retry, character_reference_value}, T3};

        _ ->
            %% Invalid - not hex or decimal
            {nok, T}
    end.

%% @doc Collect value characters and validate
value(T) ->
    RefType = erlmd_tokeniser:get_state(T, char_ref_type),
    Size = erlmd_tokeniser:get_state(T, char_ref_size),

    case RefType of
        named -> value_named(T, Size);
        numeric ->
            NumType = erlmd_tokeniser:get_state(T, char_ref_num_type),
            value_numeric(T, NumType, Size)
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
%% Handle named character reference value collection
value_named(T, Size) ->
    case erlmd_tokeniser:current(T) of
        $; when Size > 0 ->
            %% End of named reference - consume semicolon and validate
            T1 = erlmd_tokeniser:exit(T, character_reference_value),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, character_reference),
            {ok, T3};

        $; when Size =:= 0 ->
            %% Empty name - invalid
            {nok, T};

        C when Size < ?MAX_NAMED_LENGTH,
               ((C >= $a andalso C =< $z) orelse
                (C >= $A andalso C =< $Z) orelse
                (C >= $0 andalso C =< $9)) ->
            %% Valid name character - consume and continue
            T1 = erlmd_tokeniser:consume(T),
            T2 = erlmd_tokeniser:set_state(T1, char_ref_size, Size + 1),
            {{next, character_reference_value}, T2};

        _ when Size >= ?MAX_NAMED_LENGTH ->
            %% Name too long
            {nok, T};

        _ ->
            %% Invalid character or EOF
            {nok, T}
    end.

%% @private
%% Handle numeric character reference value collection
value_numeric(T, NumType, Size) ->
    MaxSize = erlmd_util_char_ref:value_max(NumType),
    IsValidDigit = erlmd_util_char_ref:value_test(NumType),

    case erlmd_tokeniser:current(T) of
        $; when Size > 0 ->
            %% End of numeric reference - consume semicolon and validate
            T1 = erlmd_tokeniser:exit(T, character_reference_value),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, character_reference),
            {ok, T3};

        $; when Size =:= 0 ->
            %% Empty value - invalid
            {nok, T};

        C when Size < MaxSize ->
            case IsValidDigit(C) of
                true ->
                    %% Valid digit - consume and continue
                    T1 = erlmd_tokeniser:consume(T),
                    T2 = erlmd_tokeniser:set_state(T1, char_ref_size, Size + 1),
                    {{next, character_reference_value}, T2};
                false ->
                    %% Invalid digit
                    {nok, T}
            end;

        _ when Size >= MaxSize ->
            %% Value too long
            {nok, T};

        _ ->
            %% EOF or invalid
            {nok, T}
    end.
