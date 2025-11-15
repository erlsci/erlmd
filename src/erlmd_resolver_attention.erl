%%%-----------------------------------------------------------------------------
%%% @doc Attention resolver - Matches emphasis/strong delimiter pairs.
%%%
%%% This is Phase 7.8 - for now this is a stub that does nothing.
%%% The actual matching algorithm will be implemented in Phase 7.8.
%%%
%%% Reference: markdown-rs src/resolve/attention.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_resolver_attention).

-export([resolve/1]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec resolve(erlmd_tokeniser:tokenizer()) -> erlmd_tokeniser:tokenizer().
%% @doc Stub resolver - does nothing for now.
%%
%% Phase 7.8 will implement the full emphasis/strong matching algorithm.
resolve(T) ->
    %% For now, just return the tokenizer unchanged
    %% The attention_sequence events will remain in the event stream
    T.
