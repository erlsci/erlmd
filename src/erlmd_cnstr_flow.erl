%%%-----------------------------------------------------------------------------
%%% @doc Flow (block-level) content dispatcher.
%%%
%%% Flow content handles block-level constructs like:
%%% - Headings (# Heading, underline style)
%%% - Paragraphs
%%% - Code blocks (indented, fenced)
%%% - Thematic breaks (---, ***)
%%% - HTML blocks
%%% - Blank lines
%%% - Tables, definitions (GFM/extensions)
%%%
%%% Used in: document body, container content (block quotes, list items).
%%%
%%% IMPORTANT: Paragraph MUST BE LAST - it's the fallback that always succeeds.
%%% Blank line has HIGHEST priority - can interrupt most other constructs.
%%%
%%% Reference: markdown-rs/src/construct/flow.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_flow).

-export([start/1]).

-include("types.hrl").

%%%=============================================================================
%%% Constants
%%%=============================================================================

%% Flow (block-level) content constructs in priority order
%% Based on markdown-rs/src/construct/flow.rs
-define(FLOW_CONSTRUCTS, [
    %% Blank line has highest priority - can interrupt anything
    blank_line,
    %% Code blocks
    code_indented,
    raw_flow,  % Fenced code/math
    %% Headings
    heading_atx,
    heading_setext,
    %% HTML
    html_flow,
    %% MDX (if enabled - will fail gracefully)
    mdx_esm,
    mdx_expression_flow,
    mdx_jsx_flow,
    %% Thematic break
    thematic_break,
    %% GFM table (if enabled)
    gfm_table,
    %% Definitions and content
    definition,
    %% Paragraph MUST BE LAST - it's the default/fallback
    paragraph
]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point for flow (block-level) content parsing.
%%
%% Tries each block construct in priority order. Blank line is first because
%% it can interrupt most constructs. Paragraph is last because it's the fallback.
%%
%% Unlike string/text dispatchers, flow returns after ONE construct (not recursive).
%% The document dispatcher calls flow repeatedly until EOF.
start(T) ->
    try_constructs(T, ?FLOW_CONSTRUCTS).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec try_constructs(erlmd_tokeniser:tokenizer(), [atom()]) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Try each construct in order until one succeeds.
%%
%% Pattern: attempt construct, return immediately on success.
%% This is tail-recursive and preserves binary match context.
%%
%% NOTE: Unlike string/text, flow does NOT recurse on success.
%% It returns after one construct, letting document dispatcher loop.
try_constructs(T, []) ->
    %% Should never happen - paragraph should always succeed as fallback
    {nok, T};

try_constructs(T, [Construct | Rest]) ->
    case erlmd_tokeniser:attempt_construct(T, Construct, nok) of
        {ok, T1} ->
            %% Construct succeeded - return immediately
            {ok, T1};
        {nok, T1} ->
            %% Construct failed, try next in list
            try_constructs(T1, Rest)
    end.
