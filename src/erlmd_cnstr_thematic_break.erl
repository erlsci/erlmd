%%%-----------------------------------------------------------------------------
%%% @doc Thematic break construct - horizontal rules.
%%%
%%% A thematic break consists of 0-3 spaces of indentation, followed by
%%% a sequence of three or more matching -, _, or * characters, each
%%% followed optionally by any number of spaces or tabs.
%%%
%%% CommonMark Spec: §4.1 Thematic breaks
%%% Reference: markdown-rs/src/construct/thematic_break.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_thematic_break).

-export([start/1, at_break/1, inside/1, consume_whitespace/1, after_prefix/1]).

-include("types.hrl").
-include("consts.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point for thematic break construct.
%%
%% Tries to parse a thematic break (horizontal rule). Returns {ok, T} if
%% successful, {nok, T} if not a thematic break.
start(Tokenizer) ->
    T1 = erlmd_tokeniser:enter(Tokenizer, thematic_break),
    T2 = erlmd_tokeniser:set_state(T1, thematic_break_count, 0),

    %% Handle optional leading whitespace (up to 3 spaces)
    case erlmd_tokeniser:current(T2) of
        C when C =:= $\t; C =:= $\s ->
            {{retry, thematic_break_consume_prefix}, T2};
        _ ->
            before(T2)
    end.

%% @doc Consume optional leading whitespace (up to 3 spaces).
%% Exported for state machine.
-spec after_prefix(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
after_prefix(Tokenizer) ->
    Prefix = erlmd_tokeniser:get_state(Tokenizer, thematic_break_prefix),
    PrefixCount = if Prefix =:= undefined -> 0; true -> Prefix end,

    case erlmd_tokeniser:current(Tokenizer) of
        C when C =:= $\t orelse C =:= $\s ->
            if
                PrefixCount < 3 ->
                    %% Consume prefix whitespace
                    T1 = erlmd_tokeniser:consume(Tokenizer),
                    T2 = erlmd_tokeniser:set_state(T1, thematic_break_prefix, PrefixCount + 1),
                    {{next, thematic_break_consume_prefix}, T2};
                true ->
                    %% Hit max prefix, start parsing markers
                    before(Tokenizer)
            end;
        _ ->
            %% Done consuming prefix, start parsing markers
            before(Tokenizer)
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @doc Before the first marker - check for valid marker character.
%% @private
-spec before(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
before(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        C when C =:= $*; C =:= $-; C =:= $_ ->
            %% Valid marker - store it and start sequence
            T1 = erlmd_tokeniser:set_state(Tokenizer, thematic_break_marker, C),
            T2 = erlmd_tokeniser:enter(T1, thematic_break_sequence),
            {{retry, thematic_break_inside}, T2};
        _ ->
            %% Not a thematic break marker
            T1 = erlmd_tokeniser:exit(Tokenizer, thematic_break),
            {nok, T1}
    end.

%% @doc Inside marker sequence - counting markers.
%% Exported for state machine.
-spec inside(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
inside(Tokenizer) ->
    Marker = erlmd_tokeniser:get_state(Tokenizer, thematic_break_marker),
    Count = erlmd_tokeniser:get_state(Tokenizer, thematic_break_count),

    case erlmd_tokeniser:current(Tokenizer) of
        M when M =:= Marker ->
            %% Same marker - consume and continue counting
            T1 = erlmd_tokeniser:consume(Tokenizer),
            T2 = erlmd_tokeniser:set_state(T1, thematic_break_count, Count + 1),
            {{next, thematic_break_inside}, T2};

        C when C =:= $\t; C =:= $\s ->
            %% Whitespace - exit sequence and continue at break
            T1 = erlmd_tokeniser:exit(Tokenizer, thematic_break_sequence),
            {{retry, thematic_break_at_break}, T1};

        C when C =:= eof; C =:= $\n ->
            %% End of line - exit sequence and check if valid
            T1 = erlmd_tokeniser:exit(Tokenizer, thematic_break_sequence),
            finish(T1);

        _ ->
            %% Other character - not a thematic break
            T1 = erlmd_tokeniser:exit(Tokenizer, thematic_break_sequence),
            T2 = erlmd_tokeniser:exit(T1, thematic_break),
            {nok, T2}
    end.

%% @doc At a break point between markers or at whitespace.
%% Exported for state machine.
-spec at_break(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
at_break(Tokenizer) ->
    Marker = erlmd_tokeniser:get_state(Tokenizer, thematic_break_marker),

    case erlmd_tokeniser:current(Tokenizer) of
        M when M =:= Marker ->
            %% Another marker - enter sequence again
            T1 = erlmd_tokeniser:enter(Tokenizer, thematic_break_sequence),
            {{retry, thematic_break_inside}, T1};

        C when C =:= $\t; C =:= $\s ->
            %% More whitespace - consume and continue
            {{retry, thematic_break_consume_whitespace}, Tokenizer};

        C when C =:= eof; C =:= $\n ->
            %% End of line - check if valid
            finish(Tokenizer);

        _ ->
            %% Other character - not a thematic break
            T1 = erlmd_tokeniser:exit(Tokenizer, thematic_break),
            {nok, T1}
    end.

%% @doc Consume whitespace between markers.
%% Exported for state machine.
-spec consume_whitespace(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
consume_whitespace(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        C when C =:= $\t; C =:= $\s ->
            %% Consume whitespace and go back to at_break
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, thematic_break_at_break}, T1};
        _ ->
            %% Not whitespace, go back to at_break
            {{retry, thematic_break_at_break}, Tokenizer}
    end.

%% @doc Finish parsing - check if we have enough markers.
%% @private
-spec finish(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
finish(Tokenizer) ->
    Count = erlmd_tokeniser:get_state(Tokenizer, thematic_break_count),

    if
        Count >= ?THEMATIC_BREAK_MARKER_COUNT_MIN ->
            %% Valid thematic break
            T1 = erlmd_tokeniser:exit(Tokenizer, thematic_break),
            {ok, T1};
        true ->
            %% Not enough markers
            T1 = erlmd_tokeniser:exit(Tokenizer, thematic_break),
            {nok, T1}
    end.
