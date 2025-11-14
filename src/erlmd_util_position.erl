%%%-----------------------------------------------------------------------------
%%% @doc Position tracking utilities for markdown parsing.
%%%
%%% This module provides utilities for working with points and positions,
%%% including creation, advancement, and formatting.
%%%
%%% Based on markdown-rs src/unist.rs
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_util_position).

-include("types.hrl").
-include("consts.hrl").

%% API exports
-export([
    new_point/3,
    new_position/2,
    new_position/6,
    advance_column/1,
    advance_line/1,
    advance_by/2,
    advance_by_tab/1,
    to_unist_point/1,
    to_unist_position/1,
    format_point/1,
    format_position/1
]).

%%%=============================================================================
%%% API Functions - Point Creation
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new point with the given line, column, and offset.
%%
%% Line and column are 1-indexed (as per unist spec).
%% Offset is 0-indexed (byte position in the input).
%% @end
%%------------------------------------------------------------------------------
-spec new_point(pos_integer(), pos_integer(), non_neg_integer()) -> point().
new_point(Line, Column, Offset) ->
    #point{line = Line, column = Column, offset = Offset}.

%%%=============================================================================
%%% API Functions - Position Creation
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new position from start and end points.
%% @end
%%------------------------------------------------------------------------------
-spec new_position(point(), point()) -> position().
new_position(Start, End) ->
    #position{start = Start, 'end' = End}.

%%------------------------------------------------------------------------------
%% @doc Create a new position from explicit line/column/offset values.
%%
%% This is a convenience function for creating positions without first
%% creating point records.
%% @end
%%------------------------------------------------------------------------------
-spec new_position(pos_integer(), pos_integer(), non_neg_integer(),
                   pos_integer(), pos_integer(), non_neg_integer()) -> position().
new_position(StartLine, StartCol, StartOff, EndLine, EndCol, EndOff) ->
    #position{
        start = new_point(StartLine, StartCol, StartOff),
        'end' = new_point(EndLine, EndCol, EndOff)
    }.

%%%=============================================================================
%%% API Functions - Point Advancement
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Advance a point by one column (one byte, not a newline).
%%
%% Used when consuming a single character that is not a newline.
%% @end
%%------------------------------------------------------------------------------
-spec advance_column(point()) -> point().
advance_column(#point{column = Col, offset = Off} = P) ->
    P#point{column = Col + 1, offset = Off + 1}.

%%------------------------------------------------------------------------------
%% @doc Advance a point for a newline character.
%%
%% Increments line number, resets column to 1, and advances offset by 1.
%% @end
%%------------------------------------------------------------------------------
-spec advance_line(point()) -> point().
advance_line(#point{line = Line, offset = Off} = P) ->
    P#point{line = Line + 1, column = 1, offset = Off + 1}.

%%------------------------------------------------------------------------------
%% @doc Advance a point by N bytes (for multi-byte sequences like "```").
%%
%% Increments both column and offset by N.
%% Does not handle newlines specially - caller must check.
%% @end
%%------------------------------------------------------------------------------
-spec advance_by(point(), pos_integer()) -> point().
advance_by(#point{column = Col, offset = Off} = P, N) ->
    P#point{column = Col + N, offset = Off + N}.

%%------------------------------------------------------------------------------
%% @doc Advance a point by a tab character, respecting tab stops.
%%
%% Tabs advance to the next multiple of TAB_SIZE (default 4).
%% For example, at column 1, a tab advances to column 5.
%% At column 3, a tab advances to column 5 (next tab stop).
%% @end
%%------------------------------------------------------------------------------
-spec advance_by_tab(point()) -> point().
advance_by_tab(#point{column = Col, offset = Off} = P) ->
    %% Calculate how many spaces until the next tab stop
    %% Tab stops are at columns 1, 5, 9, 13, ...
    %% If we're at column 1, we advance by 4 to column 5
    %% If we're at column 3, we advance by 2 to column 5
    Remainder = (Col - 1) rem ?TAB_SIZE,
    Advance = ?TAB_SIZE - Remainder,
    P#point{column = Col + Advance, offset = Off + 1}.

%%%=============================================================================
%%% API Functions - Conversion to Unist Format
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Convert a point record to a unist-compatible map.
%%
%% Unist is the Universal Syntax Tree format used by markdown-rs.
%% This is used when outputting AST for external consumption.
%% @end
%%------------------------------------------------------------------------------
-spec to_unist_point(point()) -> map().
to_unist_point(#point{line = Line, column = Col, offset = Off}) ->
    #{line => Line, column => Col, offset => Off}.

%%------------------------------------------------------------------------------
%% @doc Convert a position record to a unist-compatible map.
%% @end
%%------------------------------------------------------------------------------
-spec to_unist_position(position()) -> map().
to_unist_position(#position{start = Start, 'end' = End}) ->
    #{
        start => to_unist_point(Start),
        'end' => to_unist_point(End)
    }.

%%%=============================================================================
%%% API Functions - Formatting for Debugging
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Format a point as a human-readable binary string.
%%
%% Format: "line:column (offset)"
%% Example: "5:10 (42)"
%% @end
%%------------------------------------------------------------------------------
-spec format_point(point()) -> binary().
format_point(#point{line = Line, column = Col, offset = Off}) ->
    iolist_to_binary(io_lib:format("~p:~p (~p)", [Line, Col, Off])).

%%------------------------------------------------------------------------------
%% @doc Format a position as a human-readable binary string.
%%
%% Format: "start-end"
%% Example: "1:1 (0)-5:10 (42)"
%% @end
%%------------------------------------------------------------------------------
-spec format_position(position()) -> binary().
format_position(#position{start = Start, 'end' = End}) ->
    iolist_to_binary(io_lib:format("~s-~s",
        [format_point(Start), format_point(End)])).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% None for Phase 1
