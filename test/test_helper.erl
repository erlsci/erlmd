%%%-----------------------------------------------------------------------------
%%% @doc Test helper utilities for erlmd tests.
%%%
%%% Provides common functions for creating test tokenizers and asserting
%%% on event sequences.
%%% @end
%%%-----------------------------------------------------------------------------
-module(test_helper).

-export([
    make_tokenizer/1,
    make_tokenizer/2,
    count_events/2
]).

-include("../src/types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec make_tokenizer(binary()) -> erlmd_tokenizer:tokenizer().
%% @doc Create a tokenizer with default settings.
make_tokenizer(Binary) ->
    make_tokenizer(Binary, #{}).

-spec make_tokenizer(binary(), map()) -> erlmd_tokenizer:tokenizer().
%% @doc Create a tokenizer with custom options.
make_tokenizer(Binary, Options) ->
    erlmd_tokenizer:new(Binary, Options).

-spec count_events([event()], atom()) -> non_neg_integer().
%% @doc Count how many events have a specific name.
count_events(Events, Name) ->
    length([E || E <- Events, E#event.name =:= Name]).
