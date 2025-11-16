%%%-----------------------------------------------------------------------------
%%% @doc Attention resolver - Matches emphasis/strong delimiter pairs.
%%%
%%% This implements the CommonMark emphasis/strong matching algorithm (§6.2):
%%% 1. Extract all attention sequences with open/close capability
%%% 2. Walk through potential closers
%%% 3. Find matching openers
%%% 4. Apply the "rule of 3" to prevent mismatches
%%% 5. Create emphasis or strong events
%%%
%%% Reference: markdown-rs src/resolve/attention.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_resolver_attention).

-export([resolve/1]).

-include("types.hrl").
-include("tokeniser.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec resolve(erlmd_tokeniser:tokenizer()) -> erlmd_tokeniser:tokenizer().
%% @doc Resolve attention sequences into emphasis/strong events.
%%
%% Implements the inside-out matching algorithm from CommonMark spec.
resolve(T) ->
    io:format("~n=== Attention Resolver ===~n"),

    %% Get all attention sequences with metadata
    Sequences = get_sequences(T),
    io:format("Found ~p attention sequences~n", [length(Sequences)]),

    %% Match sequences and create emphasis/strong events
    T1 = match_sequences(T, Sequences),

    io:format("=== Attention Resolution Complete ===~n~n"),
    T1.

%%%=============================================================================
%%% Sequence Extraction
%%%=============================================================================

-spec get_sequences(erlmd_tokeniser:tokenizer()) -> [attention_sequence()].
%% @doc Extract all attention sequences from events with open/close metadata.
get_sequences(T) ->
    Events = lists:reverse(erlmd_tokeniser:get_events(T)),
    Bytes = erlmd_tokeniser:get_bytes(T),
    get_sequences(Events, Bytes, 0, [], []).

get_sequences([], _Bytes, _Index, _Balance, Acc) ->
    lists:reverse(Acc);
get_sequences([Event | Rest], Bytes, Index, Balance, Acc) ->
    case Event of
        #event{kind = enter, name = attention_sequence} ->
            %% Find the matching exit event (should be next)
            ExitEvent = case Rest of
                [#event{kind = exit, name = attention_sequence} = E | _] -> E;
                _ -> undefined
            end,

            if
                ExitEvent =/= undefined ->
                    %% Get the marker byte at the start position
                    StartOffset = Event#event.point#point.offset,
                    EndOffset = ExitEvent#event.point#point.offset,
                    Marker = binary:at(Bytes, StartOffset),
                    Size = EndOffset - StartOffset,

                    %% Get surrounding characters
                    BeforeChar = char_before_offset(Bytes, StartOffset),
                    AfterChar = char_at_offset(Bytes, EndOffset),

                    %% Classify surrounding characters
                    Before = classify_char(BeforeChar),
                    After = classify_char(AfterChar),

                    %% Determine if can open/close
                    {CanOpen, CanClose} = can_open_close(Marker, Before, After),

                    Sequence = #attention_sequence{
                        marker = Marker,
                        index = Index,
                        start_point = Event#event.point,
                        end_point = ExitEvent#event.point,
                        size = Size,
                        open = CanOpen,
                        close = CanClose,
                        balance = Balance
                    },

                    get_sequences(Rest, Bytes, Index + 1, Balance, [Sequence | Acc]);
                true ->
                    %% Malformed - skip
                    get_sequences(Rest, Bytes, Index, Balance, Acc)
            end;

        #event{kind = enter} ->
            %% Track nesting depth for balance checking
            get_sequences(Rest, Bytes, Index + 1, [Index | Balance], Acc);

        #event{kind = exit} ->
            %% Pop balance stack
            NewBalance = case Balance of
                [_ | Rest1] -> Rest1;
                [] -> []
            end,
            get_sequences(Rest, Bytes, Index + 1, NewBalance, Acc);

        _ ->
            get_sequences(Rest, Bytes, Index + 1, Balance, Acc)
    end.

%%%=============================================================================
%%% Character Helpers
%%%=============================================================================

char_before_offset(_Bytes, 0) ->
    undefined;
char_before_offset(Bytes, Offset) when Offset > 0 ->
    binary:at(Bytes, Offset - 1).

char_at_offset(Bytes, Offset) when Offset < byte_size(Bytes) ->
    binary:at(Bytes, Offset);
char_at_offset(_Bytes, _Offset) ->
    undefined.

classify_char(undefined) ->
    %% Treat undefined (start/end of input) as whitespace
    whitespace;
classify_char(Char) ->
    erlmd_util_char:classify(Char).

%%%=============================================================================
%%% Open/Close Rules
%%%=============================================================================

-spec can_open_close(byte(), atom() | undefined, atom() | undefined) ->
    {boolean(), boolean()}.
%% @doc Determine if a sequence can open/close based on surrounding context.
%%
%% CommonMark rules:
%% - Can open if followed by non-whitespace (with special rules for _)
%% - Can close if preceded by non-whitespace (with special rules for _)
can_open_close(Marker, Before, After) ->
    %% Preliminary checks (before applying underscore rules)
    CanOpenPrelim = After =:= other orelse
                    (After =:= punctuation andalso Before =/= other),

    CanClosePrelim = Before =:= other orelse
                     (Before =:= punctuation andalso After =/= other),

    %% Special rules for underscore (CommonMark §6.2)
    %% Underscore: cannot open if preceded by alphanumeric
    %% Underscore: cannot close if followed by alphanumeric
    CanOpen = if
        Marker =:= $_ ->
            CanOpenPrelim andalso (Before =/= other orelse not CanClosePrelim);
        true ->
            CanOpenPrelim
    end,

    CanClose = if
        Marker =:= $_ ->
            CanClosePrelim andalso (After =/= other orelse not CanOpenPrelim);
        true ->
            CanClosePrelim
    end,

    {CanOpen, CanClose}.

%%%=============================================================================
%%% Sequence Matching
%%%=============================================================================

-spec match_sequences(erlmd_tokeniser:tokenizer(), [attention_sequence()]) ->
    erlmd_tokeniser:tokenizer().
%% @doc Match opener/closer pairs and create emphasis/strong events.
%%
%% Uses inside-out algorithm: process closers from left to right,
%% matching with openers from right to left.
match_sequences(T, Sequences) ->
    match_sequences(T, Sequences, 0).

match_sequences(T, _Sequences, CloseIdx) when CloseIdx >= length(_Sequences) ->
    T;
match_sequences(T, Sequences, CloseIdx) ->
    Closer = lists:nth(CloseIdx + 1, Sequences),

    case Closer#attention_sequence.close of
        true ->
            %% Try to find matching opener
            case find_opener(Sequences, CloseIdx) of
                {ok, OpenIdx, TakeCount} ->
                    io:format("Matched sequences ~p and ~p (take ~p)~n",
                             [OpenIdx, CloseIdx, TakeCount]),
                    %% Match found, create emphasis/strong events
                    T1 = create_emphasis_events(T, Sequences, OpenIdx, CloseIdx, TakeCount),
                    %% Update sequences list
                    {NewSequences, NextIdx} =
                        update_sequences(Sequences, OpenIdx, CloseIdx, TakeCount),
                    match_sequences(T1, NewSequences, NextIdx);

                not_found ->
                    match_sequences(T, Sequences, CloseIdx + 1)
            end;
        false ->
            match_sequences(T, Sequences, CloseIdx + 1)
    end.

%%%=============================================================================
%%% Opener Finding
%%%=============================================================================

-spec find_opener([attention_sequence()], non_neg_integer()) ->
    {ok, non_neg_integer(), pos_integer()} | not_found.
%% @doc Find matching opener for a closer.
%%
%% Walks backward from closer to find compatible opener.
find_opener(Sequences, CloseIdx) ->
    find_opener(Sequences, CloseIdx, CloseIdx - 1).

find_opener(_Sequences, _CloseIdx, OpenIdx) when OpenIdx < 0 ->
    not_found;
find_opener(Sequences, CloseIdx, OpenIdx) ->
    Opener = lists:nth(OpenIdx + 1, Sequences),
    Closer = lists:nth(CloseIdx + 1, Sequences),

    %% Check if this opener matches
    if
        Opener#attention_sequence.open andalso
        Opener#attention_sequence.marker =:= Closer#attention_sequence.marker andalso
        Opener#attention_sequence.balance =:= Closer#attention_sequence.balance ->

            %% Check rule of 3
            Rule3Violated =
                (Opener#attention_sequence.close orelse
                 Closer#attention_sequence.open) andalso
                (Closer#attention_sequence.size rem 3 =/= 0) andalso
                ((Opener#attention_sequence.size +
                  Closer#attention_sequence.size) rem 3 =:= 0),

            if
                Rule3Violated ->
                    %% Rule of 3 violation, keep searching
                    find_opener(Sequences, CloseIdx, OpenIdx - 1);
                true ->
                    %% Match found - determine how many markers to take
                    TakeCount = determine_take_count(Opener, Closer),
                    {ok, OpenIdx, TakeCount}
            end;
        true ->
            find_opener(Sequences, CloseIdx, OpenIdx - 1)
    end.

-spec determine_take_count(attention_sequence(), attention_sequence()) -> 1 | 2.
%% @doc Determine how many markers to consume (1 for emphasis, 2 for strong).
determine_take_count(Opener, Closer) ->
    %% Both have 2+ markers: strong (**text**)
    if
        Opener#attention_sequence.size >= 2 andalso
        Closer#attention_sequence.size >= 2 ->
            2;
        true ->
            1
    end.

%%%=============================================================================
%%% Event Creation
%%%=============================================================================

-spec create_emphasis_events(erlmd_tokeniser:tokenizer(),
                            [attention_sequence()],
                            non_neg_integer(),
                            non_neg_integer(),
                            pos_integer()) ->
    erlmd_tokeniser:tokenizer().
%% @doc Create emphasis or strong events for a matched pair.
create_emphasis_events(T, Sequences, OpenIdx, CloseIdx, TakeCount) ->
    Opener = lists:nth(OpenIdx + 1, Sequences),
    Closer = lists:nth(CloseIdx + 1, Sequences),

    TokenName = if
        TakeCount =:= 2 -> strong;
        true -> emphasis
    end,

    %% Calculate event positions
    %% Opener: starts at start_point, consumes TakeCount markers
    %% Closer: starts at end_point - TakeCount, consumes TakeCount markers
    OpenStart = Opener#attention_sequence.start_point#point.offset,
    OpenEnd = OpenStart + TakeCount,
    CloseStart = Closer#attention_sequence.end_point#point.offset - TakeCount,
    CloseEnd = Closer#attention_sequence.end_point#point.offset,

    Events = erlmd_tokeniser:get_events(T),

    %% Insert events at appropriate positions
    %% Need to insert: enter(emphasis), exit(emphasis) around the text
    %% And: enter(emphasis_sequence), exit(emphasis_sequence) for each marker run

    EnterEvent = #event{
        kind = enter,
        name = TokenName,
        point = #point{line = 1, column = OpenStart + 1, offset = OpenStart},
        link = undefined,
        content = undefined
    },

    ExitEvent = #event{
        kind = exit,
        name = TokenName,
        point = #point{line = 1, column = CloseEnd + 1, offset = CloseEnd},
        link = undefined,
        content = undefined
    },

    %% Insert the events (simplified - just add them to the list for now)
    %% In a full implementation, we'd need to properly splice them into the event stream
    NewEvents = [ExitEvent, EnterEvent | Events],

    T#tokenizer{events = NewEvents}.

%%%=============================================================================
%%% Sequence Updating
%%%=============================================================================

-spec update_sequences([attention_sequence()],
                       non_neg_integer(),
                       non_neg_integer(),
                       pos_integer()) ->
    {[attention_sequence()], non_neg_integer()}.
%% @doc Update sequences after matching a pair.
%%
%% Reduces size of opener/closer by TakeCount, removes if size becomes 0.
update_sequences(Sequences, OpenIdx, CloseIdx, TakeCount) ->
    Opener = lists:nth(OpenIdx + 1, Sequences),
    Closer = lists:nth(CloseIdx + 1, Sequences),

    %% Update opener
    NewOpenerSize = Opener#attention_sequence.size - TakeCount,
    NewOpener = Opener#attention_sequence{size = NewOpenerSize},

    %% Update closer
    NewCloserSize = Closer#attention_sequence.size - TakeCount,
    NewCloser = Closer#attention_sequence{size = NewCloserSize},

    %% Rebuild sequences list
    {Before, [_ | AfterOpener]} = lists:split(OpenIdx, Sequences),
    {Middle, [_ | AfterCloser]} = lists:split(CloseIdx - OpenIdx - 1, AfterOpener),

    %% Include updated sequences only if size > 0
    NewSequences = Before ++
                   (if NewOpenerSize > 0 -> [NewOpener]; true -> [] end) ++
                   Middle ++
                   (if NewCloserSize > 0 -> [NewCloser]; true -> [] end) ++
                   AfterCloser,

    %% Next index to check (start over from beginning)
    NextIdx = 0,

    {NewSequences, NextIdx}.
