%%%-----------------------------------------------------------------------------
%%% @doc Block quote construct.
%%%
%%% Implements CommonMark block quotes (Section 5.1):
%%% - Marker: `>` optionally followed by a space
%%% - Can have 0-3 spaces before marker
%%% - Contains flow content
%%% - Supports lazy continuation (subsequent lines without `>`)
%%% - Can be nested
%%%
%%% Reference: markdown-rs/src/construct/block_quote.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_block_quote).

-export([start/1, cont_start/1, cont_before/1, cont_after/1]).

-include("types.hrl").

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Start of block quote.
%% Called by flow dispatcher.
%%
%% ```markdown
%% > | > a
%%     ^
%% ```
start(T) ->
    T1 = erlmd_tokeniser:enter(T, block_quote),
    cont_start(T1).

-spec cont_start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Start of block quote continuation.
%% Also used to parse the first block quote opening.
%%
%% ```markdown
%%   | > a
%% > | > b
%%     ^
%% ```
cont_start(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $\t; C =:= $\s ->
            %% Optional whitespace before marker (0-3 spaces)
            %% Set up attempt - success continues to block_quote_cont_before, failure returns nok
            T1 = erlmd_tokeniser:attempt(T, {next, block_quote_cont_before}, nok),
            %% Set up space_or_tab parameters
            T2 = erlmd_tokeniser:set_state(T1, space_or_tab_size, 0),
            T3 = erlmd_tokeniser:set_state(T2, space_or_tab_max, 3),
            %% Try to parse 1-3 spaces/tabs
            erlmd_cnstr_prtl_space_or_tab:start(T3);
        _ ->
            cont_before(T)
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec cont_before(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc At `>`, after optional whitespace.
%%
%% ```markdown
%%   | > a
%% > | > b
%%     ^
%% ```
cont_before(T) ->
    case erlmd_tokeniser:current(T) of
        $> ->
            T1 = erlmd_tokeniser:enter(T, block_quote_prefix),
            T2 = erlmd_tokeniser:enter(T1, block_quote_marker),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, block_quote_marker),
            %% Return {next, block_quote_cont_after} so feed loop calls prepare_byte
            {{next, block_quote_cont_after}, T4};
        _ ->
            {nok, T}
    end.

-spec cont_after(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc After `>`, before optional whitespace.
%% Consumes a single optional space/tab.
%%
%% ```markdown
%% > | > a
%%      ^
%% > | >b
%%      ^
%% ```
cont_after(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $\t; C =:= $\s ->
            T1 = erlmd_tokeniser:enter(T, space_or_tab),
            T2 = erlmd_tokeniser:consume(T1),
            T3 = erlmd_tokeniser:exit(T2, space_or_tab),
            {ok, erlmd_tokeniser:exit(T3, block_quote_prefix)};
        _ ->
            {ok, erlmd_tokeniser:exit(T, block_quote_prefix)}
    end.
