%%%-----------------------------------------------------------------------------
%%% @doc Label resolver - Converts matched labels into link/image events.
%%%
%%% This is the final step of the two-pass link/image parsing:
%%% 1. Label start constructs mark potential links/images
%%% 2. Label end constructs match closing brackets and create labels
%%% 3. **Label resolver** converts matched labels into actual events
%%%
%%% The resolver:
%%% - Runs before other resolvers (registered as "before" resolver)
%%% - Iterates through all matched labels
%%% - Creates link or image events wrapping the label content
%%% - Handles both resource and reference forms
%%%
%%% Reference: markdown-rs src/resolve/label.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_resolver_label).

-export([resolve/1]).

-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec resolve(erlmd_tokeniser:tokenizer()) -> erlmd_tokeniser:tokenizer().
%% @doc Resolve all matched labels into link/image events.
%%
%% For each label in the labels list:
%% 1. Find the events between label start and end
%% 2. Create appropriate link or image wrapper events
%% 3. Insert them into the event stream
%%
%% This modifies the events list in the tokenizer.
resolve(T) ->
    Labels = erlmd_tokeniser:get_labels(T),

    %% Process labels in reverse order (last to first)
    %% This ensures positions remain valid as we insert events
    ReversedLabels = lists:reverse(Labels),

    %% Process each label
    lists:foldl(fun resolve_label/2, T, ReversedLabels).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
%% Resolve a single label into link/image events
-spec resolve_label(label(), erlmd_tokeniser:tokenizer()) ->
    erlmd_tokeniser:tokenizer().
resolve_label(Label, T) ->
    #label{
        kind = Kind,
        start = {StartPoint, _StartEvent},
        'end' = {EndPoint, _EndEvent}
    } = Label,

    %% Get events list (in reverse order - newest first)
    Events = erlmd_tokeniser:get_events(T),

    %% Determine token name based on label kind
    TokenName = case Kind of
        link -> link;
        image -> image
    end,

    %% Find the first event AT or AFTER the start point
    %% and the first event AT or AFTER the end point
    {EventsBefore, EventsFromStart} = split_at_point(Events, StartPoint),
    {EventsMiddle, EventsAfter} = split_at_point(EventsFromStart, EndPoint),

    %% Create enter and exit events with the exact points
    %% StartPoint and EndPoint are integers (offsets), convert to point records
    EnterEvent = #event{
        kind = enter,
        name = TokenName,
        point = #point{line = 1, column = StartPoint + 1, offset = StartPoint},
        link = undefined,
        content = undefined
    },

    ExitEvent = #event{
        kind = exit,
        name = TokenName,
        point = #point{line = 1, column = EndPoint + 1, offset = EndPoint},
        link = undefined,
        content = undefined
    },

    %% Reconstruct events: Before + Enter + Middle + Exit + After
    NewEvents = EventsBefore ++ [EnterEvent] ++ EventsMiddle ++ [ExitEvent] ++ EventsAfter,

    %% Update tokenizer with new events
    T#tokenizer{events = NewEvents}.

%% @private
%% Split events list at the first event with point >= TargetPoint
%% Events are in reverse order (newest/highest point first)
-spec split_at_point([event()], non_neg_integer()) -> {[event()], [event()]}.
split_at_point(Events, TargetPoint) ->
    split_at_point(Events, TargetPoint, []).

split_at_point([], _TargetPoint, Acc) ->
    {lists:reverse(Acc), []};
split_at_point([Event | Rest] = Events, TargetPoint, Acc) ->
    EventPoint = case Event#event.point of
        #point{offset = Offset} -> Offset;
        PointOffset when is_integer(PointOffset) -> PointOffset;
        undefined -> 0  % Handle events without points
    end,
    if
        EventPoint >= TargetPoint ->
            %% This event is at or after target, it goes in the second list
            {lists:reverse(Acc), Events};
        true ->
            %% This event is before target, keep looking
            split_at_point(Rest, TargetPoint, [Event | Acc])
    end.
