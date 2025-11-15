%%%-----------------------------------------------------------------------------
%%% @doc ATX Heading construct - hash-style headings (# through ######).
%%%
%%% An ATX heading consists of:
%%% - Optional leading whitespace (up to 3 spaces)
%%% - Opening sequence of 1-6 '#' characters
%%% - Required space/tab (or EOF/EOL for empty heading)
%%% - Heading text parsed as inline content
%%%
%%% Examples:
%%%   # Heading 1
%%%   ## Heading 2
%%%   ### Heading 3
%%%
%%% CommonMark Spec: §4.2 ATX headings
%%% Reference: markdown-rs/src/construct/heading_atx.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_heading_atx).

-export([start/1, before/1, after_prefix/1, sequence_open/1, after_sequence/1,
         content_inside/1]).

-include("types.hrl").
-include("consts.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point for ATX heading construct.
start(Tokenizer) ->
    T1 = erlmd_tokeniser:enter(Tokenizer, heading_atx),
    T2 = erlmd_tokeniser:set_state(T1, heading_atx_prefix, 0),

    case erlmd_tokeniser:current(T2) of
        $# ->
            %% Start directly with '#' - go to before via state machine
            {{retry, heading_atx_before}, T2};
        C when C =:= $\s; C =:= $\t ->
            %% Optional leading whitespace
            {{retry, heading_atx_after_prefix}, T2};
        _ ->
            %% Not a heading
            T3 = erlmd_tokeniser:exit(T2, heading_atx),
            {nok, T3}
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @doc Handle optional leading whitespace (up to 3 spaces/tabs).
-spec after_prefix(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
after_prefix(Tokenizer) ->
    Prefix = erlmd_tokeniser:get_state(Tokenizer, heading_atx_prefix),
    PrefixCount = if Prefix =:= undefined -> 0; true -> Prefix end,

    case erlmd_tokeniser:current(Tokenizer) of
        C when C =:= $\t orelse C =:= $\s ->
            if
                PrefixCount < 3 ->
                    %% Consume prefix whitespace
                    T1 = erlmd_tokeniser:consume(Tokenizer),
                    T2 = erlmd_tokeniser:set_state(T1, heading_atx_prefix, PrefixCount + 1),
                    {{next, heading_atx_after_prefix}, T2};
                true ->
                    %% Hit max prefix - go to before
                    {{retry, heading_atx_before}, Tokenizer}
            end;
        _ ->
            %% Done consuming prefix - go to before
            {{retry, heading_atx_before}, Tokenizer}
    end.

%% @doc After optional leading whitespace, expecting '#'.
%% @private
-spec before(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
before(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        $# ->
            T1 = erlmd_tokeniser:enter(Tokenizer, heading_atx_sequence),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:set_state(T2, heading_atx_count, 1),
            {{next, heading_atx_sequence_open}, T3};
        _ ->
            %% Not a heading
            T1 = erlmd_tokeniser:exit(Tokenizer, heading_atx),
            {nok, T1}
    end.

%% @doc Count opening '#' sequence (1-6 marks).
-spec sequence_open(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
sequence_open(Tokenizer) ->
    Count = erlmd_tokeniser:get_state(Tokenizer, heading_atx_count),

    case erlmd_tokeniser:current(Tokenizer) of
        $# when Count < ?HEADING_ATX_OPENING_FENCE_SIZE_MAX ->
            %% More hash marks and haven't hit limit
            T1 = erlmd_tokeniser:consume(Tokenizer),
            T2 = erlmd_tokeniser:set_state(T1, heading_atx_count, Count + 1),
            {{next, heading_atx_sequence_open}, T2};

        C when C =:= eof; C =:= $\n ->
            %% Empty heading at EOF or newline
            T1 = erlmd_tokeniser:exit(Tokenizer, heading_atx_sequence),
            T2 = erlmd_tokeniser:exit(T1, heading_atx),
            {ok, T2};

        C when C =:= $\s; C =:= $\t ->
            %% Space after sequence - valid heading, start content
            T1 = erlmd_tokeniser:exit(Tokenizer, heading_atx_sequence),
            {{retry, heading_atx_after_sequence}, T1};

        $# when Count >= ?HEADING_ATX_OPENING_FENCE_SIZE_MAX ->
            %% Too many hash marks (7+) - not a heading
            T1 = erlmd_tokeniser:exit(Tokenizer, heading_atx_sequence),
            T2 = erlmd_tokeniser:exit(T1, heading_atx),
            {nok, T2};

        _ ->
            %% No space after '#' - not a valid heading
            T1 = erlmd_tokeniser:exit(Tokenizer, heading_atx_sequence),
            T2 = erlmd_tokeniser:exit(T1, heading_atx),
            {nok, T2}
    end.

%% @doc After opening sequence, skip whitespace and start content.
-spec after_sequence(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
after_sequence(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% Empty heading at EOF
            T1 = erlmd_tokeniser:exit(Tokenizer, heading_atx),
            {ok, T1};

        $\n ->
            %% Empty heading at newline
            T1 = erlmd_tokeniser:exit(Tokenizer, heading_atx),
            {ok, T1};

        C when C =:= $\s; C =:= $\t ->
            %% Skip whitespace
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, heading_atx_after_sequence}, T1};

        _ ->
            %% Start content
            T1 = erlmd_tokeniser:enter(Tokenizer, data),
            {{retry, heading_atx_content_inside}, T1}
    end.

%% @doc Inside heading content - parse until EOL/EOF.
-spec content_inside(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
content_inside(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% EOF - exit data and heading
            T1 = erlmd_tokeniser:exit(Tokenizer, data),
            T2 = erlmd_tokeniser:exit(T1, heading_atx),
            {ok, T2};

        $\n ->
            %% End of heading
            T1 = erlmd_tokeniser:exit(Tokenizer, data),
            T2 = erlmd_tokeniser:exit(T1, heading_atx),
            {ok, T2};

        _ ->
            %% Regular content - consume and continue
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, heading_atx_content_inside}, T1}
    end.
