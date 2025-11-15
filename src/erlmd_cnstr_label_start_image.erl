%%%-----------------------------------------------------------------------------
%%% @doc Label start image construct - marks opening `![` for potential images.
%%%
%%% This construct marks the opening of a potential image. Similar to links
%%% but with the `!` prefix. The actual matching happens in label_end.
%%%
%%% Examples:
%%%   ![alt](url)           - Resource image
%%%   ![alt][ref]           - Full reference image
%%%   ![alt][]              - Collapsed reference image
%%%   ![alt]                - Shortcut reference image
%%%
%%% Reference: markdown-rs src/construct/label_start_image.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_label_start_image).

-export([start/1, open/1, after_open/1]).

-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Entry point for label start image construct.
%%
%% Checks if current byte is `!` and proceeds to look for `[`.
start(T) ->
    case erlmd_tokeniser:current(T) of
        $! ->
            %% Record starting event index
            StartIdx = erlmd_tokeniser:event_count(T),

            %% Store start index for later use
            T1 = erlmd_tokeniser:set_state(T, image_start_idx, StartIdx),

            %% Emit events for the `!` marker
            T2 = erlmd_tokeniser:enter(T1, label_image),
            T3 = erlmd_tokeniser:enter(T2, label_image_marker),
            T4 = erlmd_tokeniser:consume(T3),
            T5 = erlmd_tokeniser:exit(T4, label_image_marker),

            {{next, label_start_image_open}, T5};
        _ ->
            {nok, T}
    end.

%% @doc Check for the opening `[` after `!`.
open(T) ->
    case erlmd_tokeniser:current(T) of
        $[ ->
            %% Emit events for the `[` marker
            T1 = erlmd_tokeniser:enter(T, label_marker),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, label_marker),
            T4 = erlmd_tokeniser:exit(T3, label_image),

            {{next, label_start_image_after_open}, T4};
        _ ->
            {nok, T}
    end.

%% @doc Check that `^` doesn't follow (which would be a GFM footnote).
after_open(T) ->
    case erlmd_tokeniser:current(T) of
        $^ ->
            %% This would be a GFM footnote, not an image
            {nok, T};
        _ ->
            %% Valid image start - calculate event range
            StartIdx = erlmd_tokeniser:get_state(T, image_start_idx),
            EndIdx = erlmd_tokeniser:event_count(T) - 1,

            %% Clean up temporary state
            T1 = erlmd_tokeniser:clear_state(T, image_start_idx),

            %% Add to label starts
            LabelStart = #label_start{
                kind = image,
                start = {StartIdx, EndIdx},
                inactive = false
            },
            T2 = erlmd_tokeniser:add_label_start(T1, LabelStart),

            %% Add ] as a marker so data will stop before it
            Markers = erlmd_tokeniser:get_markers(T2),
            T3 = erlmd_tokeniser:set_markers(T2, [$] | Markers]),

            %% Register resolver
            T4 = erlmd_tokeniser:register_resolver_before(T3, label),

            {ok, T4}
    end.
