%%%-----------------------------------------------------------------------------
%%% @doc Data construct - parses plain text data.
%%%
%%% Data occurs in string and text content types. It consumes any bytes
%%% that are not line endings and not in the tokenizer's markers list.
%%%
%%% This is the fallback construct - when nothing else matches, we parse data.
%%%
%%% Reference: markdown-rs/src/construct/partial_data.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_partial_data).

-export([start/1, at_break/1, inside/1]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Entry point for data construct.
%%
%% Checks if we should immediately consume data or check for breaks.
start(T) ->
    at_break(T).

-spec at_break(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Check what to do at a potential break point.
%%
%% This is called when we're between data chunks or at the start.
at_break(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte) ->
            case is_marker(T, Byte) of
                false ->
                    if
                        Byte =:= $\n ->
                            %% Line ending - emit it as line_ending event
                            T1 = erlmd_tokenizer:enter(T, line_ending),
                            T2 = erlmd_tokenizer:consume(T1),
                            T3 = erlmd_tokenizer:exit(T2, line_ending),
                            {{next, at_break}, T3};
                        true ->
                            %% Regular data - enter and parse
                            T1 = erlmd_tokenizer:enter(T, data),
                            {{retry, inside}, T1}
                    end;
                true ->
                    %% Marker found - we're done
                    {ok, T}
            end;
        eof ->
            %% End of input - done
            {ok, T}
    end.

-spec inside(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Parse the inside of a data chunk.
%%
%% Consumes bytes until we hit a line ending, marker, or EOF.
inside(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte) ->
            IsMarker = is_marker(T, Byte),
            if
                Byte =/= $\n andalso not IsMarker ->
                    %% Regular byte - consume and continue
                    T1 = erlmd_tokenizer:consume(T),
                    {{next, inside}, T1};
                true ->
                    %% Hit a line ending or marker - exit data
                    T1 = erlmd_tokenizer:exit(T, data),
                    {{retry, at_break}, T1}
            end;
        eof ->
            %% End of input - exit data
            T1 = erlmd_tokenizer:exit(T, data),
            {{retry, at_break}, T1}
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @doc Check if a byte is a marker.
%%
%% Markers are special characters that indicate the start of other constructs.
-spec is_marker(erlmd_tokenizer:tokenizer(), byte()) -> boolean().
is_marker(T, Byte) ->
    Markers = erlmd_tokenizer:get_markers(T),
    lists:member(Byte, Markers).
