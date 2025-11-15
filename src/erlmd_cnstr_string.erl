%%%-----------------------------------------------------------------------------
%%% @doc String content dispatcher - simplest content type.
%%%
%%% String content only allows:
%%% - Character escapes (\*, \[, etc.)
%%% - Character references (&amp;, &#35;, etc.)
%%% - Data (literal characters)
%%%
%%% Used in: code fence info strings, link titles, link destinations, etc.
%%%
%%% Reference: markdown-rs/src/construct/string.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_string).

-export([start/1]).

-include("types.hrl").

%%%=============================================================================
%%% Constants
%%%=============================================================================

%% String content constructs in priority order
-define(STRING_CONSTRUCTS, [
    character_escape,
    character_reference
]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point for string content parsing.
%%
%% String is the simplest content type - only escapes, entities, and data.
%% This dispatcher tries character_escape and character_reference constructs,
%% falling back to data for everything else.
start(T) ->
    try_constructs(T, ?STRING_CONSTRUCTS).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec try_constructs(erlmd_tokeniser:tokenizer(), [atom()]) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Try each construct in order until one succeeds.
%%
%% Pattern: attempt construct, on success continue with string, on failure try next.
%% This is tail-recursive and preserves binary match context.
try_constructs(T, []) ->
    %% No constructs succeeded, try data as ultimate fallback
    erlmd_tokeniser:attempt_construct(T, data, nok);

try_constructs(T, [Construct | Rest]) ->
    case erlmd_tokeniser:attempt_construct(T, Construct, nok) of
        {ok, T1} ->
            %% Construct succeeded, continue parsing string content
            start(T1);
        {nok, T1} ->
            %% Construct failed, try next in list
            try_constructs(T1, Rest)
    end.
