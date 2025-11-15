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
    count_events/2,
    run_construct/2,
    run_construct/3,
    feed_until_done/2
]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec make_tokenizer(binary()) -> erlmd_tokeniser:tokenizer().
%% @doc Create a tokenizer with default settings.
make_tokenizer(Binary) ->
    make_tokenizer(Binary, #{}).

-spec make_tokenizer(binary(), map()) -> erlmd_tokeniser:tokenizer().
%% @doc Create a tokenizer with custom options.
make_tokenizer(Binary, Options) ->
    T = erlmd_tokeniser:new(Binary, Options),
    %% Prepare the first byte so constructs can consume
    erlmd_tokeniser:prepare_byte(T).

-spec count_events([event()], atom()) -> non_neg_integer().
%% @doc Count how many events have a specific name.
count_events(Events, Name) ->
    length([E || E <- Events, E#event.name =:= Name]).

-spec run_construct(atom(), erlmd_tokeniser:tokenizer()) ->
    {ok | nok, erlmd_tokeniser:tokenizer()}.
%% @doc Run a construct to completion (ok or nok).
%%
%% This simulates the tokenizer feed loop for testing purposes.
%% Returns {ok, T} or {nok, T} once construct completes.
run_construct(StartState, T) ->
    run_construct(StartState, T, 1000).  % Max 1000 iterations

-spec run_construct(atom(), erlmd_tokeniser:tokenizer(), pos_integer()) ->
    {ok | nok, erlmd_tokeniser:tokenizer()}.
%% @doc Run construct with max iteration limit.
run_construct(StartState, T, MaxIter) ->
    feed_loop({{retry, StartState}, T}, MaxIter).

-spec feed_until_done(atom(), binary()) -> {ok | nok, erlmd_tokeniser:tokenizer()}.
%% @doc Feed bytes until construct completes or EOF.
%%
%% This is for integration testing where we want to parse
%% the entire input through a starting construct.
feed_until_done(StartState, Binary) ->
    T = make_tokenizer(Binary),
    run_construct(StartState, T).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @doc Main feed loop - processes state results.
%%
%% State results come in the format: {{Action, State}, T} or {Action, T}
%% where Action is 'retry', 'next', 'ok', or 'nok'
feed_loop({ok, T}, _MaxIter) ->
    %% Run resolvers before returning
    T1 = erlmd_tokeniser:run_resolvers(T),
    {ok, T1};
feed_loop({nok, T}, _MaxIter) ->
    {nok, T};
feed_loop(_Result, 0) ->
    error(max_iterations_exceeded);
feed_loop({{retry, StateName}, T}, MaxIter) ->
    % Call state without consuming
    Result = erlmd_state:call(StateName, T),
    feed_loop(Result, MaxIter - 1);
feed_loop({{next, StateName}, T}, MaxIter) ->
    % Byte already consumed by construct, prepare next byte and call next state
    T1 = erlmd_tokeniser:prepare_byte(T),
    Result = erlmd_state:call(StateName, T1),
    feed_loop(Result, MaxIter - 1).
