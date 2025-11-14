%%%-----------------------------------------------------------------------------
%%% @doc Blank line construct - detects lines with only whitespace.
%%%
%%% A blank line is a line containing only spaces and tabs (or nothing)
%%% followed by a line ending or EOF.
%%%
%%% Blank lines are important for separating block-level constructs like
%%% paragraphs, headings, etc.
%%%
%%% Reference: markdown-rs/src/construct/blank_line.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_construct_blank_line).

-export([start/1, after_whitespace/1]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Entry point for blank line detection.
%%
%% A blank line starts with optional whitespace, then ends with EOL or EOF.
start(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when Byte =:= $\s; Byte =:= $\t ->
            %% Has whitespace - try to consume it
            %% Use attempt: if whitespace parse succeeds, check what's after
            T1 = erlmd_tokenizer:attempt(T,
                {next, after_whitespace},  % On success
                nok),                       % On failure
            %% Delegate to space_or_tab construct
            {{retry, space_or_tab_start}, T1};
        eof ->
            %% EOF - valid blank line
            {ok, T};
        $\n ->
            %% Just a newline - valid blank line
            {ok, T};
        _Other ->
            %% Something else - not a blank line
            {nok, T}
    end.

-spec after_whitespace(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Called after consuming optional whitespace.
%%
%% Now we must be at EOL or EOF for this to be a blank line.
after_whitespace(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            %% EOF - blank line confirmed
            {ok, T};
        $\n ->
            %% Line ending - blank line confirmed
            {ok, T};
        _Other ->
            %% Something else - not a blank line
            {nok, T}
    end.
