%%%-----------------------------------------------------------------------------
%%% @doc Paragraph construct - default block container for text content.
%%%
%%% A paragraph is a sequence of non-blank lines that cannot be interpreted
%%% as other kinds of blocks. The contents are parsed as inline content.
%%%
%%% Paragraphs have the LOWEST priority - they are the fallback when no other
%%% block construct matches.
%%%
%%% CommonMark Spec: §4.8 Paragraphs
%%% Reference: markdown-rs/src/construct/paragraph.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_paragraph).

-export([start/1, inside/1, line_start/1, after_line/1]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point for paragraph construct.
%%
%% Paragraphs are the default - if we get here, we're starting a paragraph.
%% This should only be called when no other block construct matches.
start(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% Empty input - no paragraph
            {nok, Tokenizer};
        $\n ->
            %% Just a blank line - no paragraph
            {nok, Tokenizer};
        _ ->
            %% Start paragraph and parse first line
            T1 = erlmd_tokeniser:enter(Tokenizer, paragraph),
            {{retry, paragraph_line_start}, T1}
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @doc Start of a line in the paragraph.
%% Exported for state machine.
-spec line_start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
line_start(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% EOF - end paragraph
            T1 = erlmd_tokeniser:exit(Tokenizer, paragraph),
            {ok, T1};
        $\n ->
            %% Blank line - end paragraph
            T1 = erlmd_tokeniser:exit(Tokenizer, paragraph),
            {ok, T1};
        _ ->
            %% Content - enter data and parse
            T1 = erlmd_tokeniser:enter(Tokenizer, data),
            {{retry, paragraph_inside}, T1}
    end.

%% @doc Inside paragraph content - parsing text.
%% Exported for state machine.
-spec inside(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
inside(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% EOF - exit data and paragraph
            T1 = erlmd_tokeniser:exit(Tokenizer, data),
            T2 = erlmd_tokeniser:exit(T1, paragraph),
            {ok, T2};

        $\n ->
            %% Line ending - exit data, consume newline, check for continuation
            T1 = erlmd_tokeniser:exit(Tokenizer, data),
            T2 = erlmd_tokeniser:consume(T1),
            {{next, paragraph_after_line}, T2};

        _Byte ->
            %% Regular character - consume and continue
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, paragraph_inside}, T1}
    end.

%% @doc After a line ending - check if paragraph continues.
%% @private
-spec after_line(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
after_line(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% EOF after line - end paragraph
            T1 = erlmd_tokeniser:exit(Tokenizer, paragraph),
            {ok, T1};

        $\n ->
            %% Another newline (blank line) - end paragraph
            T1 = erlmd_tokeniser:exit(Tokenizer, paragraph),
            {ok, T1};

        _ ->
            %% More content - continue paragraph on next line
            {{retry, paragraph_line_start}, Tokenizer}
    end.
