%%%-----------------------------------------------------------------------------
%%% @doc Space or tab construct - parses spaces and tabs.
%%%
%%% This is one of the most frequently used constructs. It can parse:
%%% - Exactly N spaces/tabs
%%% - Between min and max spaces/tabs
%%% - Optional vs required whitespace
%%%
%%% Configuration is passed via tokenizer state.
%%%
%%% Reference: markdown-rs/src/construct/partial_space_or_tab.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_partial_space_or_tab).

-export([
    space_or_tab/1,
    space_or_tab_min_max/3,
    start/1,
    inside/1,
    after_space_or_tab/1
]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec space_or_tab(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Parse one or more spaces/tabs (no upper limit).
space_or_tab(T) ->
    space_or_tab_min_max(T, 1, infinity).

-spec space_or_tab_min_max(
    erlmd_tokenizer:tokenizer(),
    non_neg_integer(),
    non_neg_integer() | infinity
) -> {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Parse between min and max spaces/tabs.
space_or_tab_min_max(T, Min, Max) ->
    %% Store config in tokenizer state
    T1 = erlmd_tokenizer:set_state(T, space_or_tab_min, Min),
    T2 = erlmd_tokenizer:set_state(T1, space_or_tab_max, Max),
    T3 = erlmd_tokenizer:set_state(T2, space_or_tab_size, 0),
    {{retry, start}, T3}.

-spec start(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Start of space_or_tab parsing.
start(T) ->
    Max = erlmd_tokenizer:get_state(T, space_or_tab_max),
    Size = erlmd_tokenizer:get_state(T, space_or_tab_size),

    %% Check if we've hit the max
    AtMax = case Max of
        infinity -> false;
        N when is_integer(N) -> Size >= N
    end,

    case erlmd_tokenizer:current(T) of
        Byte when (Byte =:= $\s orelse Byte =:= $\t) andalso not AtMax ->
            %% Valid whitespace and we haven't hit max - enter and consume
            T1 = erlmd_tokenizer:enter(T, space_or_tab),
            {{retry, inside}, T1};
        _ ->
            %% No whitespace or hit max - check if we met minimum
            {{retry, after_space_or_tab}, T}
    end.

-spec inside(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Inside space_or_tab parsing - consume while valid.
inside(T) ->
    Size = erlmd_tokenizer:get_state(T, space_or_tab_size),
    Max = erlmd_tokenizer:get_state(T, space_or_tab_max),

    %% Check if we've hit the max
    AtMax = case Max of
        infinity -> false;
        N when is_integer(N) -> Size >= N
    end,

    case erlmd_tokenizer:current(T) of
        Byte when (Byte =:= $\s orelse Byte =:= $\t) andalso not AtMax ->
            %% Valid whitespace and haven't hit max - consume
            T1 = erlmd_tokenizer:consume(T),
            T2 = erlmd_tokenizer:set_state(T1, space_or_tab_size, Size + 1),
            {{next, inside}, T2};
        _ ->
            %% Done consuming - exit and check minimum
            T1 = erlmd_tokenizer:exit(T, space_or_tab),
            {{retry, after_space_or_tab}, T1}
    end.

-spec after_space_or_tab(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc After parsing - check if we met minimum requirement.
after_space_or_tab(T) ->
    Size = erlmd_tokenizer:get_state(T, space_or_tab_size),
    Min = erlmd_tokenizer:get_state(T, space_or_tab_min),

    %% Clean up state
    T1 = erlmd_tokenizer:clear_state(T, space_or_tab_size),
    T2 = erlmd_tokenizer:clear_state(T1, space_or_tab_max),
    T3 = erlmd_tokenizer:clear_state(T2, space_or_tab_min),

    if
        Size >= Min ->
            {ok, T3};
        true ->
            {nok, T3}
    end.
