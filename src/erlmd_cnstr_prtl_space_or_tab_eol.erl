%%%-----------------------------------------------------------------------------
%%% @doc Space, tab, or EOL construct - parses whitespace including line endings.
%%%
%%% Like space_or_tab but also accepts line endings (\n).
%%% Used in contexts where newlines are allowed (e.g., between link URL and title).
%%%
%%% Reference: markdown-rs src/construct/partial_space_or_tab.rs (eol variant)
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_prtl_space_or_tab_eol).

-export([start/1, inside/1, after_whitespace/1]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Entry point - start consuming whitespace.
start(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $\s; C =:= $\t; C =:= $\n ->
            %% Valid whitespace - enter and consume
            T1 = erlmd_tokeniser:enter(T, space_or_tab),
            T2 = erlmd_tokeniser:consume(T1),
            {{next, space_or_tab_eol_inside}, T2};
        _ ->
            %% No whitespace - done (success with zero chars)
            {ok, T}
    end.

%% @doc Inside whitespace - keep consuming.
inside(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $\s; C =:= $\t; C =:= $\n ->
            %% More whitespace - consume and continue
            T1 = erlmd_tokeniser:consume(T),
            {{next, space_or_tab_eol_inside}, T1};
        _ ->
            %% No more whitespace - exit and finish
            T1 = erlmd_tokeniser:exit(T, space_or_tab),
            {{retry, space_or_tab_eol_after}, T1}
    end.

%% @doc After consuming whitespace - always succeeds.
after_whitespace(T) ->
    {ok, T}.
