%%%-----------------------------------------------------------------------------
%%% @doc Label start link construct - marks opening `[` for potential links.
%%%
%%% This construct marks the opening bracket of a potential link. The actual
%%% matching happens later in label_end when we find the closing `]`.
%%%
%%% Examples:
%%%   [text](url)           - Resource link
%%%   [text][ref]           - Full reference link
%%%   [text][]              - Collapsed reference link
%%%   [text]                - Shortcut reference link
%%%
%%% Reference: markdown-rs src/construct/label_start_link.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_label_start_link).

-export([start/1]).

-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Entry point for label start link construct.
%%
%% Checks if current byte is `[` and marks it as a potential link start.
start(T) ->
    case erlmd_tokeniser:current(T) of
        $[ ->
            %% Record the starting event index
            StartIdx = erlmd_tokeniser:event_count(T),

            %% Emit events for the opening bracket
            T1 = erlmd_tokeniser:enter(T, label_link),
            T2 = erlmd_tokeniser:enter(T1, label_marker),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, label_marker),
            T5 = erlmd_tokeniser:exit(T4, label_link),

            %% Calculate the event range for this label start
            EndIdx = erlmd_tokeniser:event_count(T5) - 1,

            %% Add to label starts list
            LabelStart = #label_start{
                kind = link,
                start = {StartIdx, EndIdx},
                inactive = false
            },
            T6 = erlmd_tokeniser:add_label_start(T5, LabelStart),

            %% Add ] as a marker so data will stop before it
            Markers = erlmd_tokeniser:get_markers(T6),
            T7 = erlmd_tokeniser:set_markers(T6, [$] | Markers]),

            %% Register resolver to run before other resolvers
            T8 = erlmd_tokeniser:register_resolver_before(T7, label),

            {ok, T8};
        _ ->
            {nok, T}
    end.
