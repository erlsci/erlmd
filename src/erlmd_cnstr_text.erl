%%%-----------------------------------------------------------------------------
%%% @doc Text (inline) content dispatcher.
%%%
%%% Text content handles inline constructs like:
%%% - Emphasis and strong (*text*, **text**)
%%% - Links and images ([text](url), ![alt](url))
%%% - Code spans (`code`)
%%% - Escapes and entities
%%% - Autolinks, HTML tags
%%% - GFM and MDX extensions (if enabled)
%%%
%%% Used in: paragraphs, headings, table cells, etc.
%%%
%%% Reference: markdown-rs/src/construct/text.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_text).

-export([start/1]).

-include("types.hrl").

%%%=============================================================================
%%% Constants
%%%=============================================================================

%% Text content constructs in priority order
%% Note: This is the order from markdown-rs/src/construct/text.rs
%% Phase 6 constructs are now implemented: code_text, character_reference, character_escape, hard_break_escape
-define(TEXT_CONSTRUCTS, [
    %% Task list item check is special - only at start of first paragraph
    gfm_task_list_item_check,
    %% Images before links (same opener `[`)
    label_start_image,
    %% Code/math spans (implemented in Phase 6)
    code_text,
    %% Entities (implemented in Phase 6)
    character_reference,
    %% Emphasis/strong
    attention,
    %% Autolinks
    autolink,
    html_text,
    %% MDX (if enabled - will fail gracefully if not implemented)
    mdx_jsx_text,
    %% GFM bare URLs
    gfm_autolink_literal,
    %% GFM footnotes
    gfm_label_start_footnote,
    %% Escapes (implemented in Phase 6)
    character_escape,
    hard_break_escape,
    %% Links (after images)
    label_start_link,
    label_end,
    %% MDX expressions
    mdx_expression_text
]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Entry point for text (inline) content parsing.
%%
%% Tries each inline construct in priority order. GFM and MDX constructs
%% will fail gracefully if not implemented. Falls back to data for literal text.
start(T) ->
    %% Text parsing terminates at EOF or line endings
    case erlmd_tokeniser:current(T) of
        eof ->
            {ok, T};
        $\n ->
            {ok, T};
        _ ->
            %% Add attention markers (* and _) so data knows to stop at them
            Markers = erlmd_tokeniser:get_markers(T),
            HasAsterisk = lists:member($*, Markers),
            HasUnderscore = lists:member($_, Markers),
            T1 = if
                HasAsterisk andalso HasUnderscore ->
                    T;  % Already have both
                HasAsterisk ->
                    erlmd_tokeniser:set_markers(T, [$_ | Markers]);
                HasUnderscore ->
                    erlmd_tokeniser:set_markers(T, [$* | Markers]);
                true ->
                    erlmd_tokeniser:set_markers(T, [$*, $_ | Markers])
            end,
            try_constructs(T1, ?TEXT_CONSTRUCTS)
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec try_constructs(erlmd_tokeniser:tokenizer(), [atom()]) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Try each construct in order until one succeeds.
%%
%% Pattern: attempt construct, on success continue with text, on failure try next.
%% This is tail-recursive and preserves binary match context.
try_constructs(T, []) ->
    %% No constructs succeeded, try data as ultimate fallback
    case erlmd_tokeniser:attempt_construct(T, data, nok) of
        {ok, T1} ->
            %% Data succeeded, continue parsing
            start(T1);
        {nok, T1} ->
            %% Data also failed
            {nok, T1}
    end;

try_constructs(T, [Construct | Rest]) ->
    case erlmd_tokeniser:attempt_construct(T, Construct, nok) of
        {ok, T1} ->
            %% Construct succeeded, continue parsing text content
            start(T1);
        {nok, T1} ->
            %% Construct failed, try next in list
            try_constructs(T1, Rest)
    end.
