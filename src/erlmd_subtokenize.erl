%%%-----------------------------------------------------------------------------
%%% @doc Subtokenization - Recursively tokenize nested content.
%%%
%%% This module handles the recursive processing of linked events to parse
%%% nested content within block constructs. For example, parsing inline content
%%% within a paragraph, or parsing text within emphasis markers.
%%%
%%% ## Key Concepts
%%%
%%% **Event Links**: Events can be "linked" to indicate nested content:
%%% ```erlang
%%% #event{
%%%     kind = enter,
%%%     name = paragraph,
%%%     link = #link{
%%%         previous = undefined,
%%%         next = 5,  % Index of next linked event
%%%         content = text  % Content type to parse
%%%     }
%%% }
%%% ```
%%%
%%% **Algorithm**:
%%% 1. Find events with `link` field set
%%% 2. For each link chain, create a new tokenizer
%%% 3. Feed linked slices to the subtokenizer
%%% 4. Collect resulting sub-events
%%% 5. Divide sub-events back into original event positions
%%% 6. Replace links with actual parsed events
%%%
%%% Reference: markdown-rs src/subtokenize.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_subtokenize).

-export([
    subtokenize/3,
    link/2,
    link_to/3,
    extract_slice/3  % For testing
]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions - Link Helpers
%%%=============================================================================

-spec link(Events :: [event()], Index :: non_neg_integer()) -> [event()].
%% @doc Link current event to previous void event.
%%
%% This is an optimization for the common case where we want to link
%% the current event (at Index) to the previous void event (at Index - 2).
%%
%% A "void" event is an enter/exit pair with no content between them.
link(Events, Index) ->
    link_to(Events, Index - 2, Index).

-spec link_to(Events, Previous, Next) -> [event()]
    when Events :: [event()],
         Previous :: non_neg_integer(),
         Next :: non_neg_integer().
%% @doc Link two arbitrary events together.
%%
%% Creates a bidirectional link between events at Previous and Next indices.
%% The Previous event must be a void event (enter immediately followed by exit).
%% The Next event must be an enter event.
%% Both must have the same content type.
%%
%% ```markdown
%% Events before:
%%   [0] Enter(paragraph, link=#link{content=text})
%%   [1] Exit(paragraph)
%%   [2] Enter(data)          <- wants to link back
%%   [3] Exit(data)
%%
%% After link_to(Events, 0, 2):
%%   [0] Enter(paragraph, link=#link{next=2, content=text})
%%   [1] Exit(paragraph)
%%   [2] Enter(data, link=#link{previous=0, content=text})
%%   [3] Exit(data)
%% ```
link_to(Events, Previous, Next) ->
    % Validate indices
    true = Previous >= 0,
    true = Next < length(Events),
    true = Previous < Next,

    % Get events
    PrevEvent = lists:nth(Previous + 1, Events),
    NextEvent = lists:nth(Next + 1, Events),

    % Previous must be an Enter event
    true = PrevEvent#event.kind =:= enter,

    % Previous must be a void event (has matching Exit immediately after)
    PrevExit = lists:nth(Previous + 2, Events),
    true = PrevExit#event.kind =:= exit,
    true = PrevExit#event.name =:= PrevEvent#event.name,

    % Next must be Enter
    true = NextEvent#event.kind =:= enter,

    % Get or create links
    PrevLink = case PrevEvent#event.link of
        undefined -> #link{content = text};  % Default content type
        PL -> PL
    end,
    NextLink = case NextEvent#event.link of
        undefined -> #link{content = text};
        NL -> NL
    end,

    % Set next pointer on previous
    PrevLink2 = PrevLink#link{next = Next},
    PrevEvent2 = PrevEvent#event{link = PrevLink2},

    % Set previous pointer on next
    NextLink2 = NextLink#link{previous = Previous},
    NextEvent2 = NextEvent#event{link = NextLink2},

    % Content types must match
    true = PrevLink2#link.content =:= NextLink2#link.content,

    % Update events list
    Events2 = lists:sublist(Events, Previous) ++
              [PrevEvent2] ++
              lists:sublist(Events, Previous + 2, Next - Previous - 1) ++
              [NextEvent2] ++
              lists:nthtail(Next + 1, Events),

    Events2.

%%%=============================================================================
%%% API Functions - Subtokenization
%%%=============================================================================

-spec subtokenize(Events, ParseState, Filter) -> {ok, subresult(), [event()]} | {error, term()}
    when Events :: [event()],
         ParseState :: map(),
         Filter :: content_type() | undefined.
%% @doc Main subtokenization entry point.
%%
%% Processes all linked events in the event stream, recursively parsing
%% their nested content and inserting the results back into the event stream.
%%
%% Returns:
%% - `{ok, subresult(), Events2}` on success
%% - `{error, Reason}` on failure
%%
%% The subresult indicates:
%% - `done`: true if no more links to process, false otherwise
%% - `definitions`: any link reference definitions found
%% - `gfm_footnote_definitions`: any footnote definitions found
subtokenize(Events, ParseState, Filter) ->
    Map = erlmd_edit_map:new(),
    Result = #subresult{
        done = true,
        definitions = [],
        gfm_footnote_definitions = []
    },

    % Process events to find and handle links
    case process_events(Events, ParseState, Filter, Map, Result, 0) of
        {ok, FinalMap, FinalResult} ->
            % Apply all edits
            Events2 = erlmd_edit_map:consume(FinalMap, Events),
            {ok, FinalResult, Events2};
        {error, Reason} ->
            {error, Reason}
    end.

%%%=============================================================================
%%% Internal Functions - Event Processing
%%%=============================================================================

-spec process_link_chain(Events, ParseState, StartIndex, Link, Map, Result) ->
    {erlmd_edit_map:edit_map(), subresult()}
    when Events :: [event()],
         ParseState :: map(),
         StartIndex :: non_neg_integer(),
         Link :: link(),
         Map :: erlmd_edit_map:edit_map(),
         Result :: subresult().
%% @doc Process a chain of linked events.
%%
%% This function:
%% 1. Walks the link chain from StartIndex
%% 2. Extracts content slices from each linked event
%% 3. Creates a subtokenizer for the content type
%% 4. Feeds all slices to the subtokenizer
%% 5. Collects sub-events and updates edit map
%%
%% For Week 1 Days 3-4, this is a simplified implementation that
%% collects the chain but doesn't yet create full subtokenizers.
process_link_chain(Events, ParseState, StartIndex, Link, Map, Result) ->
    % Walk the link chain to collect all linked events
    ChainIndices = collect_chain_indices(Events, StartIndex, Link),

    % Extract slices from linked events
    {ok, _Slices} = feed_link_chain(Events, ParseState, ChainIndices),

    % For now, simplified: just mark that we processed a link chain
    % TODO (Week 1 Day 5+): Create actual subtokenizer, feed content, divide events
    Result2 = Result#subresult{done = false},

    % TODO: Use edit_map to replace linked events with sub-events
    % For now, return unchanged
    {Map, Result2}.

-spec collect_chain_indices(Events, Index, Link) -> [non_neg_integer()]
    when Events :: [event()],
         Index :: non_neg_integer(),
         Link :: link().
%% @doc Collect all indices in a link chain.
%%
%% Starting from Index, follow Link.next pointers to build
%% a list of all linked event indices.
collect_chain_indices(Events, Index, Link) ->
    collect_chain_indices(Events, Index, Link, [Index]).

-spec collect_chain_indices(Events, Index, Link, Acc) -> [non_neg_integer()]
    when Events :: [event()],
         Index :: non_neg_integer(),
         Link :: link(),
         Acc :: [non_neg_integer()].
collect_chain_indices(_Events, _Index, #link{next = undefined}, Acc) ->
    lists:reverse(Acc);
collect_chain_indices(Events, _Index, #link{next = NextIndex}, Acc) ->
    NextEvent = lists:nth(NextIndex + 1, Events),
    case NextEvent#event.link of
        undefined ->
            lists:reverse([NextIndex | Acc]);
        NextLink ->
            collect_chain_indices(Events, NextIndex, NextLink, [NextIndex | Acc])
    end.

-spec feed_link_chain(Events, ParseState, ChainIndices) -> {ok, [event()]} | {error, term()}
    when Events :: [event()],
         ParseState :: map(),
         ChainIndices :: [non_neg_integer()].
%% @doc Feed linked event slices to a subtokenizer.
%%
%% For each linked event in the chain:
%% 1. Extract the content slice (between enter/exit events)
%% 2. Feed it to the subtokenizer
%% 3. Collect resulting sub-events
%%
%% Week 1 Days 3-4: Extract slices (simplified - no actual subtokenizer yet)
%% TODO (Week 1 Day 5+): Create subtokenizer and feed extracted slices
feed_link_chain(Events, ParseState, ChainIndices) ->
    % Extract content slices from each linked event
    Slices = extract_slices(Events, ParseState, ChainIndices),

    % TODO (Week 1 Day 5+): Create subtokenizer for the content type
    % TODO (Week 1 Day 5+): Feed each slice to subtokenizer
    % TODO (Week 1 Day 5+): Collect sub-events

    % For now, return empty list (no sub-events yet)
    {ok, Slices}.

-spec extract_slices(Events, ParseState, ChainIndices) -> [binary()]
    when Events :: [event()],
         ParseState :: map(),
         ChainIndices :: [non_neg_integer()].
%% @doc Extract byte slices from linked events.
%%
%% For each linked event index, find the enter/exit pair and
%% extract the byte slice between them from the source bytes.
extract_slices(Events, ParseState, ChainIndices) ->
    Bytes = maps:get(bytes, ParseState, <<>>),
    lists:map(
        fun(Index) ->
            extract_slice(Events, Bytes, Index)
        end,
        ChainIndices
    ).

-spec extract_slice(Events, Bytes, Index) -> binary()
    when Events :: [event()],
         Bytes :: binary(),
         Index :: non_neg_integer().
%% @doc Extract byte slice for a single linked event.
%%
%% Finds the enter event at Index, locates its matching exit,
%% and extracts the bytes between them.
extract_slice(Events, Bytes, Index) ->
    EnterEvent = lists:nth(Index + 1, Events),

    % Find the matching exit event
    % For now, assume it's immediately after (void event)
    % TODO: Handle non-void events (search for matching exit)
    ExitIndex = find_matching_exit(Events, Index, EnterEvent#event.name),
    ExitEvent = lists:nth(ExitIndex + 1, Events),

    % Extract byte slice between enter and exit
    StartOffset = EnterEvent#event.point#point.offset,
    EndOffset = ExitEvent#event.point#point.offset,

    % Handle case where StartOffset >= EndOffset (void event)
    if
        StartOffset >= EndOffset ->
            <<>>;
        true ->
            Length = EndOffset - StartOffset,
            <<_:StartOffset/binary, Slice:Length/binary, _/binary>> = Bytes,
            Slice
    end.

-spec find_matching_exit(Events, StartIndex, Name) -> non_neg_integer()
    when Events :: [event()],
         StartIndex :: non_neg_integer(),
         Name :: event_name().
%% @doc Find the matching exit event for an enter event.
%%
%% Searches forward from StartIndex to find the exit event
%% with the matching name. Handles nested events by counting depth.
find_matching_exit(Events, StartIndex, Name) ->
    find_matching_exit(Events, StartIndex + 1, Name, 1).

-spec find_matching_exit(Events, Index, Name, Depth) -> non_neg_integer()
    when Events :: [event()],
         Index :: non_neg_integer(),
         Name :: event_name(),
         Depth :: pos_integer().
find_matching_exit(Events, Index, Name, Depth) ->
    Event = lists:nth(Index + 1, Events),
    case Event of
        #event{kind = enter, name = Name} ->
            % Another enter with same name - increase depth
            find_matching_exit(Events, Index + 1, Name, Depth + 1);
        #event{kind = exit, name = Name} when Depth =:= 1 ->
            % Found matching exit at depth 1
            Index;
        #event{kind = exit, name = Name} ->
            % Exit but still nested - decrease depth
            find_matching_exit(Events, Index + 1, Name, Depth - 1);
        _ ->
            % Other event - keep searching
            find_matching_exit(Events, Index + 1, Name, Depth)
    end.

-spec process_events(Events, ParseState, Filter, Map, Result, Index) ->
    {ok, erlmd_edit_map:edit_map(), subresult()} | {error, term()}
    when Events :: [event()],
         ParseState :: map(),
         Filter :: content_type() | undefined,
         Map :: erlmd_edit_map:edit_map(),
         Result :: subresult(),
         Index :: non_neg_integer().
%% @doc Process events to find and handle linked content.
process_events(Events, _ParseState, _Filter, Map, Result, Index)
        when Index >= length(Events) ->
    % Done processing all events
    {ok, Map, Result};

process_events(Events, ParseState, Filter, Map, Result, Index) ->
    Event = lists:nth(Index + 1, Events),

    case Event#event.link of
        undefined ->
            % No link, continue to next event
            process_events(Events, ParseState, Filter, Map, Result, Index + 1);

        Link when Event#event.kind =:= enter ->
            % Check if this is the start of a link chain
            case Link#link.previous of
                undefined when Filter =:= undefined;
                              Filter =:= Link#link.content ->
                    % This is a link start and passes filter - process the chain
                    {Map2, Result2} = process_link_chain(Events, ParseState, Index, Link, Map, Result),
                    process_events(Events, ParseState, Filter, Map2, Result2, Index + 1);

                _ ->
                    % Already processed or filtered out
                    process_events(Events, ParseState, Filter, Map, Result, Index + 1)
            end;

        _Link ->
            % Not an enter event with link
            process_events(Events, ParseState, Filter, Map, Result, Index + 1)
    end.
