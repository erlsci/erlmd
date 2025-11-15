%%%-----------------------------------------------------------------------------
%%% @doc Indented code construct - code blocks created by indentation.
%%%
%%% An indented code block is formed by indenting every line by at least
%%% 4 spaces or 1 tab. The indentation is removed and the content is
%%% treated as literal code.
%%%
%%% Examples:
%%%     code line 1
%%%     code line 2
%%%
%%% CommonMark Spec: §4.4 Indented code blocks
%%% Reference: markdown-rs/src/construct/code_indented.rs
%%%
%%% SIMPLIFIED VERSION FOR PHASE 5:
%%% - Requires 4 spaces or 1 tab indentation
%%% - Parses content until indentation drops
%%% - Handles blank lines within code
%%% - Does NOT handle paragraph interruption (deferred to later phase)
%%% - Does NOT handle lazy/pierce (deferred to later phase)
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_code_indented).

-export([start/1, after_prefix/1, begin_line/1, content_inside/1,
         further_start/1, further_end/1]).

-include("types.hrl").
-include("consts.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point for indented code construct.
%%
%% Must have at least 4 spaces or 1 tab indentation.
start(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        $\t ->
            %% Tab counts as 4 spaces
            T1 = erlmd_tokeniser:enter(Tokenizer, code_indented),
            T2 = erlmd_tokeniser:consume(T1),
            {{next, code_indented_begin_line}, T2};

        $\s ->
            %% Try to consume 4 spaces
            T1 = erlmd_tokeniser:enter(Tokenizer, code_indented),
            T2 = erlmd_tokeniser:set_state(T1, code_indent_spaces, 0),
            {{retry, code_indented_after_prefix}, T2};

        _ ->
            %% Not indented code
            {nok, Tokenizer}
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @doc Consume required indentation (4 spaces).
-spec after_prefix(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
after_prefix(Tokenizer) ->
    Spaces = erlmd_tokeniser:get_state(Tokenizer, code_indent_spaces),
    SpaceCount = if Spaces =:= undefined -> 0; true -> Spaces end,

    case erlmd_tokeniser:current(Tokenizer) of
        $\s when SpaceCount < ?CODE_INDENT_SIZE ->
            %% Consume space
            T1 = erlmd_tokeniser:consume(Tokenizer),
            T2 = erlmd_tokeniser:set_state(T1, code_indent_spaces, SpaceCount + 1),

            if
                SpaceCount + 1 >= ?CODE_INDENT_SIZE ->
                    %% Have 4 spaces, start content
                    {{next, code_indented_begin_line}, T2};
                true ->
                    %% Need more spaces
                    {{next, code_indented_after_prefix}, T2}
            end;

        _ when SpaceCount >= ?CODE_INDENT_SIZE ->
            %% Have enough indentation
            {{retry, code_indented_begin_line}, Tokenizer};

        _ ->
            %% Not enough indentation - not code
            T1 = erlmd_tokeniser:exit(Tokenizer, code_indented),
            {nok, T1}
    end.

%% @doc Begin a line of code content.
-spec begin_line(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
begin_line(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% End of code at EOF
            T1 = erlmd_tokeniser:exit(Tokenizer, code_indented),
            {ok, T1};

        $\n ->
            %% Blank line or end - check if code continues
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, code_indented_further_start}, T1};

        _ ->
            %% Content - enter chunk and parse
            T1 = erlmd_tokeniser:enter(Tokenizer, code_flow_chunk),
            {{retry, code_indented_content_inside}, T1}
    end.

%% @doc Inside code content - consume until EOL.
-spec content_inside(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
content_inside(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% End of code at EOF
            T1 = erlmd_tokeniser:exit(Tokenizer, code_flow_chunk),
            T2 = erlmd_tokeniser:exit(T1, code_indented),
            {ok, T2};

        $\n ->
            %% End of line - check if code continues
            T1 = erlmd_tokeniser:exit(Tokenizer, code_flow_chunk),
            T2 = erlmd_tokeniser:consume(T1),
            {{next, code_indented_further_start}, T2};

        _ ->
            %% Regular content
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, code_indented_content_inside}, T1}
    end.

%% @doc Check if code block continues on next line.
-spec further_start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
further_start(Tokenizer) ->
    case erlmd_tokeniser:current(Tokenizer) of
        eof ->
            %% End of code at EOF
            T1 = erlmd_tokeniser:exit(Tokenizer, code_indented),
            {ok, T1};

        $\n ->
            %% Blank line - include it and continue
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, code_indented_further_start}, T1};

        $\t ->
            %% Tab - code continues
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, code_indented_begin_line}, T1};

        $\s ->
            %% Spaces - check if we have 4
            T1 = erlmd_tokeniser:set_state(Tokenizer, code_indent_spaces, 0),
            {{retry, code_indented_further_end}, T1};

        _ ->
            %% No indentation - end code
            T1 = erlmd_tokeniser:exit(Tokenizer, code_indented),
            {ok, T1}
    end.

%% @doc Check if next line has enough indentation.
-spec further_end(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
further_end(Tokenizer) ->
    Spaces = erlmd_tokeniser:get_state(Tokenizer, code_indent_spaces),
    SpaceCount = if Spaces =:= undefined -> 0; true -> Spaces end,

    case erlmd_tokeniser:current(Tokenizer) of
        $\s when SpaceCount < ?CODE_INDENT_SIZE ->
            %% Consume space
            T1 = erlmd_tokeniser:consume(Tokenizer),
            T2 = erlmd_tokeniser:set_state(T1, code_indent_spaces, SpaceCount + 1),

            if
                SpaceCount + 1 >= ?CODE_INDENT_SIZE ->
                    %% Have 4 spaces - continue code
                    {{next, code_indented_begin_line}, T2};
                true ->
                    %% Need more spaces
                    {{next, code_indented_further_end}, T2}
            end;

        $\n when SpaceCount > 0 ->
            %% Blank line with some spaces - include it
            T1 = erlmd_tokeniser:consume(Tokenizer),
            {{next, code_indented_further_start}, T1};

        _ when SpaceCount >= ?CODE_INDENT_SIZE ->
            %% Have enough indentation - continue
            {{retry, code_indented_begin_line}, Tokenizer};

        _ ->
            %% Not enough indentation - end code
            T1 = erlmd_tokeniser:exit(Tokenizer, code_indented),
            {ok, T1}
    end.
