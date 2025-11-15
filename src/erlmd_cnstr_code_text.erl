%%%-----------------------------------------------------------------------------
%%% @doc Code text (inline code) construct.
%%%
%%% Handles backtick-delimited inline code spans.
%%% Implements CommonMark 6.1 "Code spans".
%%%
%%% Code spans are delimited by matching sequences of backticks.
%%% The opening and closing sequences must have the same length.
%%% Line endings within code spans are converted to spaces.
%%%
%%% Examples:
%%%   `code`
%%%   ``code with ` backtick``
%%%   ``` lots ``` of ``` backticks ```
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_cnstr_code_text).

-export([start/1, sequence_open/1, between/1, data/1, sequence_close/1]).

-include("types.hrl").
-include("tokenizer_internal.hrl").

%%%=============================================================================
%%% State Functions
%%%=============================================================================

%% @doc Entry point - checks for backtick
start(T) ->
    case erlmd_tokenizer:current(T) of
        $` ->
            T1 = erlmd_tokenizer:enter(T, code_text),
            T2 = erlmd_tokenizer:enter(T1, code_text_sequence),
            %% Initialize size counter for sequence length
            T3 = erlmd_tokenizer:set_state(T2, code_text_open_size, 0),
            {{retry, code_text_sequence_open}, T3};
        _ ->
            {nok, T}
    end.

%% @doc Count opening backtick sequence
sequence_open(T) ->
    Size = erlmd_tokenizer:get_state(T, code_text_open_size),

    case erlmd_tokenizer:current(T) of
        $` ->
            T1 = erlmd_tokenizer:consume(T),
            T2 = erlmd_tokenizer:set_state(T1, code_text_open_size, Size + 1),
            {{next, code_text_sequence_open}, T2};
        _ ->
            %% End of opening sequence
            T1 = erlmd_tokenizer:exit(T, code_text_sequence),
            %% Initialize close counter
            T2 = erlmd_tokenizer:set_state(T1, code_text_close_size, 0),
            {{retry, code_text_between}, T2}
    end.

%% @doc Between opening and closing - handle whitespace and check for closer
between(T) ->
    case erlmd_tokenizer:current(T) of
        $` ->
            %% Potential closing sequence - reset close counter
            T1 = erlmd_tokenizer:enter(T, code_text_sequence),
            T2 = erlmd_tokenizer:set_state(T1, code_text_close_size, 0),
            {{retry, code_text_sequence_close}, T2};

        $\n ->
            %% Line ending - emit as line_ending and convert to space in output
            T1 = erlmd_tokenizer:enter(T, line_ending),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2, line_ending),
            {{next, code_text_between}, T3};

        eof ->
            %% No closing sequence found - reclassify opening as data
            reclassify_and_fail(T);

        _ ->
            %% Regular data
            T1 = erlmd_tokenizer:enter(T, code_text_data),
            {{retry, code_text_data}, T1}
    end.

%% @doc Consume code text data (non-backtick, non-line-ending content)
data(T) ->
    case erlmd_tokenizer:current(T) of
        $` ->
            %% End of data, start of potential closing sequence
            T1 = erlmd_tokenizer:exit(T, code_text_data),
            {{retry, code_text_between}, T1};

        $\n ->
            %% End of data, line ending
            T1 = erlmd_tokenizer:exit(T, code_text_data),
            {{retry, code_text_between}, T1};

        eof ->
            %% End of data, EOF
            T1 = erlmd_tokenizer:exit(T, code_text_data),
            {{retry, code_text_between}, T1};

        _ ->
            %% Consume data byte
            T1 = erlmd_tokenizer:consume(T),
            {{next, code_text_data}, T1}
    end.

%% @doc Count closing backtick sequence and check if it matches opening
sequence_close(T) ->
    OpenSize = erlmd_tokenizer:get_state(T, code_text_open_size),
    CloseSize = erlmd_tokenizer:get_state(T, code_text_close_size),

    case erlmd_tokenizer:current(T) of
        $` when CloseSize < OpenSize ->
            %% Still counting closing sequence
            T1 = erlmd_tokenizer:consume(T),
            T2 = erlmd_tokenizer:set_state(T1, code_text_close_size, CloseSize + 1),
            {{next, code_text_sequence_close}, T2};

        $` when CloseSize =:= OpenSize ->
            %% One more backtick than needed - no match, reclassify and continue
            T1 = erlmd_tokenizer:exit(T, code_text_sequence),
            %% Reclassify this sequence as data and continue looking
            T2 = reclassify_sequence_as_data(T1),
            {{retry, code_text_between}, T2};

        _ when CloseSize =:= OpenSize ->
            %% Exact match! Close the code text
            T1 = erlmd_tokenizer:exit(T, code_text_sequence),
            T2 = erlmd_tokenizer:exit(T1, code_text),
            {ok, T2};

        _ when CloseSize < OpenSize ->
            %% Not enough backticks - reclassify as data and continue
            T1 = erlmd_tokenizer:exit(T, code_text_sequence),
            T2 = reclassify_sequence_as_data(T1),
            {{retry, code_text_between}, T2};

        _ ->
            %% Should not happen
            {nok, T}
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% @doc Reclassify opening sequence events as data when no closer is found
reclassify_and_fail(T) ->
    %% Get all events
    Events = T#tokenizer.events,

    %% Find the code_text enter event and code_text_sequence events
    %% Reclassify them as data
    NewEvents = lists:map(fun(Event) ->
        case Event#event.name of
            code_text -> Event#event{name = data};
            code_text_sequence -> Event#event{name = data};
            _ -> Event
        end
    end, Events),

    T1 = T#tokenizer{events = NewEvents},
    {nok, T1}.

%% @doc Reclassify a closing sequence that didn't match as data
reclassify_sequence_as_data(T) ->
    %% Get events, find most recent code_text_sequence pair, change to data
    Events = T#tokenizer.events,

    %% The most recent events should be code_text_sequence exit and enter
    %% We need to reclassify them as data
    NewEvents = reclassify_recent_sequence(Events, 0, []),

    T#tokenizer{events = NewEvents}.

%% @doc Helper to reclassify the most recent sequence events
reclassify_recent_sequence([], _Count, Acc) ->
    lists:reverse(Acc);
reclassify_recent_sequence([Event | Rest], Count, Acc) when Count < 2 ->
    case Event#event.name of
        code_text_sequence ->
            %% Reclassify this event
            NewEvent = Event#event{name = data},
            reclassify_recent_sequence(Rest, Count + 1, [NewEvent | Acc]);
        _ ->
            reclassify_recent_sequence(Rest, Count, [Event | Acc])
    end;
reclassify_recent_sequence([Event | Rest], Count, Acc) ->
    %% Already reclassified 2 events, keep the rest as is
    reclassify_recent_sequence(Rest, Count, [Event | Acc]).
