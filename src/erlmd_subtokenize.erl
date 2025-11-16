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
    link_to/3
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

-spec subtokenize(Events, ParseState, Filter) -> {ok, subresult()} | {error, term()}
    when Events :: [event()],
         ParseState :: map(),
         Filter :: content_type() | undefined.
%% @doc Main subtokenization entry point.
%%
%% Processes all linked events in the event stream, recursively parsing
%% their nested content and inserting the results back into the event stream.
%%
%% Returns a subresult indicating:
%% - `done`: true if no more links to process, false otherwise
%% - `definitions`: any link reference definitions found
%% - `gfm_footnote_definitions`: any footnote definitions found
subtokenize(_Events, _ParseState, _Filter) ->
    % TODO: Implement
    {ok, #subresult{done = true}}.
