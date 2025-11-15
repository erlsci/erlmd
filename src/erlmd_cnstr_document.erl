%%%-----------------------------------------------------------------------------
%%% @doc Document (top-level) content dispatcher.
%%%
%%% Manages top-level document parsing:
%%% - Optional BOM (Byte Order Mark) at start
%%% - Optional frontmatter (YAML/TOML)
%%% - Containers (block quotes, lists) - Phase 8
%%% - Flow content (block-level constructs)
%%% - Line endings and EOF handling
%%%
%%% This is a SIMPLIFIED implementation for Phase 4.
%%% Full container handling will be added in Phase 8.
%%%
%%% Reference: markdown-rs/src/construct/document.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_document).

-export([start/1]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Entry point for document-level parsing.
%%
%% For Phase 4, this is simplified - no container handling yet.
%% Full implementation in Phase 8 will handle:
%% - Container stack management
%% - Lazy continuation detection
%% - Pierce mode for interrupting containers
%%
%% Current implementation:
%% 1. Enter document token
%% 2. Try optional BOM
%% 3. Loop: parse flow constructs until EOF
%% 4. Exit document token
start(T) ->
    %% Enter document token
    T1 = erlmd_tokenizer:enter(T, document),
    %% Try optional BOM
    case erlmd_tokenizer:attempt_construct(T1, bom, nok) of
        {ok, T2} ->
            parse_flow_loop(T2);
        {nok, T2} ->
            parse_flow_loop(T2)
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec parse_flow_loop(erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Main loop: repeatedly parse flow constructs until EOF.
%%
%% This is tail-recursive and will parse the entire document by:
%% 1. Checking for EOF
%% 2. If not EOF, calling flow dispatcher
%% 3. If flow succeeds, continue loop
%% 4. If flow fails, return error (shouldn't happen with paragraph fallback)
%%
%% The flow dispatcher tries all block-level constructs and should always
%% succeed because paragraph is the fallback.
parse_flow_loop(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            %% End of document - exit document token
            T1 = erlmd_tokenizer:exit(T, document),
            {ok, T1};
        _ ->
            %% Try to parse flow content
            case erlmd_state:call(flow, T) of
                {ok, T1} ->
                    %% Flow construct succeeded, continue
                    parse_flow_loop(T1);
                {nok, T1} ->
                    %% Flow construct failed - shouldn't happen if paragraph is last
                    %% This is an error condition
                    T2 = erlmd_tokenizer:exit(T1, document),
                    {error, {unexpected_content, erlmd_tokenizer:current(T1)}, T2}
            end
    end.
