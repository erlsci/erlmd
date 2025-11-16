%%%-----------------------------------------------------------------------------
%%% @doc Edit Map - Accumulate and apply event list modifications.
%%%
%%% This module provides a way to accumulate modifications to an event list
%%% and then apply them all at once. This avoids index shifting issues when
%%% adding/removing events during iteration.
%%%
%%% ## Usage
%%%
%%% ```erlang
%%% Map = erlmd_edit_map:new(),
%%% erlmd_edit_map:add(Map, 5, 0, [NewEvent1, NewEvent2]),  % Insert at 5
%%% erlmd_edit_map:add(Map, 10, 2, []),                     % Delete 2 at 10
%%% Events2 = erlmd_edit_map:consume(Map, Events).
%%% ```
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_edit_map).

-export([
    new/0,
    add/4,
    consume/2
]).

-include("types.hrl").

%%%=============================================================================
%%% Types
%%%=============================================================================

%% Edit operation: {Index, DeleteCount, InsertEvents}
-type edit() :: {non_neg_integer(), non_neg_integer(), [event()]}.
-type edit_map() :: [edit()].

-export_type([edit_map/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec new() -> edit_map().
%% @doc Create a new empty edit map.
new() ->
    [].

-spec add(Map, Index, DeleteCount, InsertEvents) -> edit_map()
    when Map :: edit_map(),
         Index :: non_neg_integer(),
         DeleteCount :: non_neg_integer(),
         InsertEvents :: [event()].
%% @doc Add an edit operation to the map.
%%
%% Operations are stored in reverse order and will be applied from
%% highest index to lowest to avoid index shifting issues.
add(Map, Index, DeleteCount, InsertEvents) ->
    [{Index, DeleteCount, InsertEvents} | Map].

-spec consume(Map :: edit_map(), Events :: [event()]) -> [event()].
%% @doc Apply all edits to the event list.
%%
%% Edits are applied from highest index to lowest to maintain
%% correct indices throughout the process.
consume(Map, Events) ->
    % Sort edits by index (highest first)
    Sorted = lists:sort(fun({I1, _, _}, {I2, _, _}) -> I1 >= I2 end, Map),

    % Apply each edit
    lists:foldl(fun apply_edit/2, Events, Sorted).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec apply_edit(Edit :: edit(), Events :: [event()]) -> [event()].
%% @doc Apply a single edit operation to the event list.
apply_edit({Index, DeleteCount, InsertEvents}, Events) ->
    % Split at index
    {Before, AtAndAfter} = lists:split(Index, Events),

    % Remove DeleteCount items
    After = lists:nthtail(DeleteCount, AtAndAfter),

    % Reassemble with insertions
    Before ++ InsertEvents ++ After.
