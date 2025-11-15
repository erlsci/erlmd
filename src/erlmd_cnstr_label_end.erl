%%%-----------------------------------------------------------------------------
%%% @doc Label end construct - matches closing `]` and looks for destination/reference.
%%%
%%% This is the MOST COMPLEX construct in Phase 7. It handles:
%%% - Resource links: [text](url) or [text](url "title")
%%% - Full references: [text][ref]
%%% - Collapsed references: [text][]
%%% - Shortcut references: [text]
%%%
%%% The construct tries each form in order, using backtracking (attempt).
%%%
%%% Reference: markdown-rs src/construct/label_end.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_label_end).

-export([start/1, after_marker/1, label_end_ok/1, label_end_nok/1]).
-export([resource_start/1, resource_before/1, resource_open/1,
         resource_destination_after/1, resource_destination_missing/1,
         resource_between/1, resource_title_after/1, resource_end/1]).
-export([reference_full_start/1, reference_full_open/1, reference_full_after/1,
         reference_collapsed_start/1, reference_shortcut/1]).

-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% API Functions - Core
%%%=============================================================================

%% @doc Entry point - checks for `]` and matching label start.
start(T) ->
    case erlmd_tokeniser:current(T) of
        $] ->
            %% Check if there's a matching opening
            case erlmd_tokeniser:has_label_starts(T) of
                true ->
                    LabelStart = erlmd_tokeniser:peek_label_start(T),

                    %% Store end index for later use
                    T1 = erlmd_tokeniser:set_end_index(T,
                        erlmd_tokeniser:event_count(T)),

                    %% Check if inactive (inside another link)
                    case LabelStart#label_start.inactive of
                        true ->
                            {{retry, label_end_nok}, T1};
                        false ->
                            %% Valid label end, consume the ]
                            T2 = erlmd_tokeniser:enter(T1, label_end),
                            T3 = erlmd_tokeniser:enter(T2, label_marker),
                            T4 = erlmd_tokeniser:consume(T3),
                            T5 = erlmd_tokeniser:exit(T4, label_marker),
                            T6 = erlmd_tokeniser:exit(T5, label_end),
                            {{next, label_end_after_marker}, T6}
                    end;
                false ->
                    {nok, T}
            end;
        _ ->
            {nok, T}
    end.

%% @doc After consuming `]`, try different reference types.
after_marker(T) ->
    %% Try different forms in priority order:
    %% 1. Resource: [text](url)
    %% 2. Full/collapsed reference: [text][ref] or [text][]
    %% 3. Shortcut reference: [text]
    Curr = erlmd_tokeniser:current(T),
    io:format("*** label_end after_marker, current = ~p~n", [Curr]),
    case Curr of
        $( ->
            %% Try resource: [text](url) or [text](url "title")
            io:format("*** Trying resource form~n"),
            {{retry, label_end_resource_start}, T};
        $[ ->
            %% Try full or collapsed reference: [text][ref] or [text][]
            io:format("*** Trying full/collapsed reference form~n"),
            {{retry, label_end_reference_full_start}, T};
        _ ->
            %% Try shortcut reference: [text] (use text content as reference)
            io:format("*** Trying shortcut reference form~n"),
            {{retry, label_end_reference_shortcut}, T}
    end.

%% @doc Success - we matched a link/image!
label_end_ok(T) ->
    %% Remove the label start from the stack
    {LabelStart, T1} = erlmd_tokeniser:pop_label_start(T),

    %% If this is a link (not image), mark earlier link starts as inactive
    %% (prevents nested links)
    T2 = case LabelStart#label_start.kind of
        image ->
            T1;
        _ ->
            erlmd_tokeniser:mark_link_starts_inactive(T1)
    end,

    %% Add matched label
    EndIndex = erlmd_tokeniser:get_end_index(T2),
    EventCount = erlmd_tokeniser:event_count(T2) - 1,

    Label = #label{
        kind = LabelStart#label_start.kind,
        start = LabelStart#label_start.start,
        'end' = {EndIndex, EventCount}
    },
    io:format("~n*** Adding label: ~p~n", [Label]),
    T3 = erlmd_tokeniser:add_label(T2, Label),
    io:format("*** Labels count after add: ~p~n", [length(erlmd_tokeniser:get_labels(T3))]),
    T4 = erlmd_tokeniser:set_end_index(T3, 0),
    T5 = erlmd_tokeniser:register_resolver_before(T4, label),

    {ok, T5}.

%% @doc Failure - didn't match.
label_end_nok(T) ->
    %% Don't modify state here - restore_progress will revert to correct state.
    %% Just clear temporary fields and return nok.
    T1 = erlmd_tokeniser:set_end_index(T, 0),
    {nok, T1}.

%%%=============================================================================
%%% Resource Parsing - [text](url) or [text](url "title")
%%%=============================================================================

%% @doc Start of resource - must be on `(`.
resource_start(T) ->
    io:format("*** resource_start called, current = ~p~n", [erlmd_tokeniser:current(T)]),
    case erlmd_tokeniser:current(T) of
        $( ->
            T1 = erlmd_tokeniser:enter(T, resource),
            T2 = erlmd_tokeniser:enter(T1, resource_marker),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, resource_marker),
            {{next, label_end_resource_before}, T4};
        _ ->
            {nok, T}
    end.

%% @doc Optional whitespace before destination.
resource_before(T) ->
    %% Try to consume optional whitespace (always succeeds, even with 0 chars)
    case erlmd_tokeniser:attempt_construct(T, space_or_tab_eol, nok) of
        {ok, T1} ->
            {{retry, label_end_resource_open}, T1};
        {nok, T1} ->
            %% Should never happen since space_or_tab_eol always succeeds
            {{retry, label_end_resource_open}, T1}
    end.

%% @doc Parse destination or handle empty resource.
resource_open(T) ->
    io:format("*** resource_open, current = ~p~n", [erlmd_tokeniser:current(T)]),
    case erlmd_tokeniser:current(T) of
        $) ->
            %% Empty resource: [text]()
            io:format("*** Empty resource, going to resource_end~n"),
            {{retry, label_end_resource_end}, T};
        _ ->
            %% Parse destination using partial_destination
            io:format("*** Parsing destination~n"),
            T1 = erlmd_tokeniser:set_token_names(T,
                resource_destination,
                resource_destination_literal,
                resource_destination_literal_marker,
                resource_destination_raw,
                resource_destination_string),

            %% Try to parse destination - check result and route accordingly
            case erlmd_tokeniser:attempt_construct(T1, prtl_destination_start, nok) of
                {ok, T2} ->
                    io:format("*** Destination parse succeeded~n"),
                    {{retry, label_end_resource_destination_after}, T2};
                {nok, T2} ->
                    io:format("*** Destination parse failed~n"),
                    {{retry, label_end_resource_destination_missing}, T2}
            end
    end.

%% @doc After successfully parsing destination.
resource_destination_after(T) ->
    io:format("*** Destination succeeded, going to resource_between~n"),
    {{retry, label_end_resource_between}, T}.

%% @doc Destination parsing failed.
resource_destination_missing(T) ->
    io:format("*** Destination FAILED~n"),
    {{retry, label_end_nok}, T}.

%% @doc Optional whitespace between destination and title.
resource_between(T) ->
    %% Try to consume optional whitespace
    case erlmd_tokeniser:attempt_construct(T, space_or_tab_eol, nok) of
        {ok, T1} ->
            %% After whitespace, check if there's a title
            {{retry, label_end_resource_title_after}, T1};
        {nok, T1} ->
            %% Should never happen
            {{retry, label_end_resource_title_after}, T1}
    end.

%% @doc After consuming whitespace, try to parse title.
resource_title_after(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $"; C =:= $'; C =:= $( ->
            %% Try to parse title
            T1 = erlmd_tokeniser:set_token_names(T,
                resource_title,
                resource_title_marker,
                resource_title_string,
                undefined, undefined),

            %% Parse title - success or failure both go to resource_end
            case erlmd_tokeniser:attempt_construct(T1, prtl_title_start, nok) of
                {ok, T2} ->
                    %% Title parsed successfully
                    {{retry, label_end_resource_end}, T2};
                {nok, T2} ->
                    %% Title parsing failed - the whole resource fails
                    {{retry, label_end_nok}, T2}
            end;
        _ ->
            %% No title, go to end
            {{retry, label_end_resource_end}, T}
    end.

%% @doc End of resource - must be `)`.
resource_end(T) ->
    Curr = erlmd_tokeniser:current(T),
    io:format("*** resource_end, current = ~p~n", [Curr]),
    case Curr of
        $) ->
            io:format("*** Found ), calling label_end_ok~n"),
            T1 = erlmd_tokeniser:enter(T, resource_marker),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, resource_marker),
            T4 = erlmd_tokeniser:exit(T3, resource),
            %% Success! Call label_end_ok to finalize the link/image
            {{retry, label_end_ok}, T4};
        _ ->
            %% Failed to find closing ), call label_end_nok
            io:format("*** No ), failing~n"),
            {{retry, label_end_nok}, T}
    end.

%%%=============================================================================
%%% Reference Parsing - [text][ref], [text][], [text]
%%%=============================================================================

%% @doc Start of full/collapsed reference - must be on `[`.
reference_full_start(T) ->
    io:format("*** reference_full_start, current = ~p~n", [erlmd_tokeniser:current(T)]),
    case erlmd_tokeniser:current(T) of
        $[ ->
            T1 = erlmd_tokeniser:enter(T, reference),
            T2 = erlmd_tokeniser:enter(T1, reference_marker),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, reference_marker),
            {{next, label_end_reference_full_open}, T4};
        _ ->
            {nok, T}
    end.

%% @doc After opening `[`, try to parse reference label.
reference_full_open(T) ->
    io:format("*** reference_full_open, current = ~p~n", [erlmd_tokeniser:current(T)]),
    case erlmd_tokeniser:current(T) of
        $] ->
            %% Collapsed reference: [text][]
            io:format("*** Collapsed reference (empty label)~n"),
            {{retry, label_end_reference_collapsed_start}, T};
        _ ->
            %% Try to parse full reference label
            io:format("*** Trying to parse reference label~n"),
            T1 = erlmd_tokeniser:set_token_names(T,
                reference_string,
                undefined, undefined, undefined, undefined),

            case erlmd_tokeniser:attempt_construct(T1, prtl_label_start, nok) of
                {ok, T2} ->
                    io:format("*** Reference label parsed successfully~n"),
                    {{retry, label_end_reference_full_after}, T2};
                {nok, T2} ->
                    io:format("*** Reference label parsing failed~n"),
                    {{retry, label_end_nok}, T2}
            end
    end.

%% @doc After parsing reference label, consume closing `]`.
reference_full_after(T) ->
    io:format("*** reference_full_after, current = ~p~n", [erlmd_tokeniser:current(T)]),
    case erlmd_tokeniser:current(T) of
        $] ->
            T1 = erlmd_tokeniser:enter(T, reference_marker),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, reference_marker),
            T4 = erlmd_tokeniser:exit(T3, reference),
            %% Success! Full reference matched
            io:format("*** Full reference complete, calling label_end_ok~n"),
            {{retry, label_end_ok}, T4};
        _ ->
            io:format("*** No closing ], failing~n"),
            {{retry, label_end_nok}, T}
    end.

%% @doc Handle collapsed reference [text][].
reference_collapsed_start(T) ->
    io:format("*** reference_collapsed_start, current = ~p~n", [erlmd_tokeniser:current(T)]),
    case erlmd_tokeniser:current(T) of
        $] ->
            T1 = erlmd_tokeniser:enter(T, reference_marker),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, reference_marker),
            T4 = erlmd_tokeniser:exit(T3, reference),
            %% Success! Collapsed reference matched
            io:format("*** Collapsed reference complete, calling label_end_ok~n"),
            {{retry, label_end_ok}, T4};
        _ ->
            io:format("*** Expected ], failing~n"),
            {{retry, label_end_nok}, T}
    end.

%% @doc Handle shortcut reference [text].
reference_shortcut(T) ->
    io:format("*** reference_shortcut - treating as shortcut reference~n"),
    %% No additional parsing needed - just use the text content as reference
    %% The resolver will extract the text between the original [...]
    {{retry, label_end_ok}, T}.
