%%%-----------------------------------------------------------------------------
%%% @doc Attention construct - Marks emphasis/strong delimiters.
%%%
%%% This construct identifies sequences of `*` or `_` characters that could
%%% potentially be emphasis or strong delimiters. The actual matching of
%%% opening/closing pairs is done by the attention resolver.
%%%
%%% Algorithm:
%%% 1. Detect `*` or `_` character
%%% 2. Count consecutive markers (same character)
%%% 3. Create attention_sequence event
%%% 4. Register attention resolver for later processing
%%%
%%% Reference: markdown-rs src/construct/attention.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_attention).

-export([start/1, inside/1]).

-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_state:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point called by text dispatcher.
%%
%% Checks if current byte is `*` or `_` and starts attention sequence.
start(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $*; C =:= $_ ->
            %% Store the marker for comparison
            T1 = erlmd_tokeniser:set_marker(T, C),
            T2 = erlmd_tokeniser:enter(T1, attention_sequence),
            {{retry, attention_inside}, T2};
        _ ->
            {nok, T}
    end.

-spec inside(erlmd_tokeniser:tokenizer()) ->
    {erlmd_state:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Count consecutive markers.
%%
%% Consumes all consecutive instances of the same marker character.
%% When a different character is encountered, exits the sequence and
%% registers the attention resolver.
inside(T) ->
    Marker = erlmd_tokeniser:get_marker(T),
    case erlmd_tokeniser:current(T) of
        Marker ->
            T1 = erlmd_tokeniser:consume(T),
            {{next, attention_inside}, T1};
        _ ->
            %% Sequence complete, register for resolution
            T1 = erlmd_tokeniser:exit(T, attention_sequence),
            T2 = erlmd_tokeniser:register_resolver(T1, attention),
            T3 = erlmd_tokeniser:set_marker(T2, 0),
            {ok, T3}
    end.
