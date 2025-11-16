%%%-----------------------------------------------------------------------------
%%% @doc List item construct.
%%%
%%% Implements CommonMark list items (Section 5.2):
%%% - Unordered markers: `*`, `-`, `+`
%%% - Ordered markers: `1.` through `999999999.` or `1)` through `999999999)`
%%% - Indentation: Marker can be preceded by 0-3 spaces
%%% - Content indent: Content after marker determines list item indent
%%% - Blank initial: List item can start with blank line
%%% - Lazy continuation: Subsequent lines can be lazy
%%% - Loose vs tight: Blank lines between items determine list style
%%%
%%% Reference: markdown-rs/src/construct/list_item.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_list_item).

-export([start/1, cont_start/1, before/1, before_unordered/1, value/1, marker/1, after_marker/1]).

-include("types.hrl").

%%%=============================================================================
%%% Constants
%%%=============================================================================

%% Maximum length of ordered list value (9 digits + terminator)
-define(LIST_ITEM_VALUE_SIZE_MAX, 10).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Start of list item.
%%
%% ```markdown
%% > | * a
%%     ^
%% ```
start(T) ->
    T1 = erlmd_tokeniser:enter(T, list_item),

    %% Optional indentation (0-3 spaces)
    case erlmd_tokeniser:current(T1) of
        C when C =:= $\t; C =:= $\s ->
            %% Set up attempt - if indentation succeeds, go to before
            T2 = erlmd_tokeniser:attempt(T1, {next, list_item_before}, nok),
            erlmd_cnstr_prtl_space_or_tab:space_or_tab_min_max(T2, 0, 3);
        _ ->
            before(T1)
    end.

-spec cont_start(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc Start of list item continuation.
%% To be implemented in Day 3.
cont_start(_T) ->
    error(not_implemented).

%%%=============================================================================
%%% Internal Functions - Marker Detection
%%%=============================================================================

-spec before(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc After optional whitespace, at list item prefix.
%%
%% ```markdown
%% > | * a
%%     ^
%% ```
before(T) ->
    case erlmd_tokeniser:current(T) of
        %% Unordered markers - but check for thematic break first
        C when C =:= $*; C =:= $- ->
            %% These could be thematic breaks, so we need to check
            %% If it IS a thematic break, reject (nok)
            %% If it's NOT a thematic break, continue to list_item_before_unordered
            T1 = erlmd_tokeniser:check(T, nok, {next, list_item_before_unordered}),
            erlmd_cnstr_thematic_break:start(T1);
        $+ ->
            %% + cannot be thematic break
            before_unordered(T);
        C when C >= $0, C =< $9 ->
            %% Ordered list marker
            %% TODO (Day 3): Check interrupt rules - lists starting with
            %% digits other than 1 cannot interrupt paragraphs
            before_ordered(T);
        _ ->
            {nok, T}
    end.

-spec before_unordered(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc At unordered list item marker.
%% The line is not a thematic break.
%%
%% ```markdown
%% > | * a
%%     ^
%% ```
before_unordered(T) ->
    T1 = erlmd_tokeniser:enter(T, list_item_prefix),
    marker(T1).

-spec before_ordered(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc At ordered list item value.
%%
%% ```markdown
%% > | 1. a
%%     ^
%% ```
before_ordered(T) ->
    T1 = erlmd_tokeniser:enter(T, list_item_prefix),
    T2 = erlmd_tokeniser:enter(T1, list_item_value),
    T3 = erlmd_tokeniser:set_state(T2, list_item_value_size, 0),
    {{retry, list_item_value}, T3}.

-spec value(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc In ordered list item value.
%% Uses tokenizer state to track size.
%%
%% ```markdown
%% > | 1. a
%%     ^
%% ```
value(T) ->
    Size = case erlmd_tokeniser:get_state(T, list_item_value_size) of
        undefined -> 0;
        N when is_integer(N) -> N
    end,
    case erlmd_tokeniser:current(T) of
        C when C =:= $.; C =:= $) ->
            %% End of value - proceed to marker
            %% TODO (Day 3): Check interrupt rules - empty values (Size < 1)
            %% are not allowed when interrupting paragraphs
            T1 = erlmd_tokeniser:clear_state(T, list_item_value_size),
            T2 = erlmd_tokeniser:exit(T1, list_item_value),
            {{retry, list_item_marker}, T2};
        C when C >= $0, C =< $9 ->
            %% Another digit
            if
                Size + 1 < ?LIST_ITEM_VALUE_SIZE_MAX ->
                    T1 = erlmd_tokeniser:consume(T),
                    T2 = erlmd_tokeniser:set_state(T1, list_item_value_size, Size + 1),
                    {{next, list_item_value}, T2};
                true ->
                    %% Too many digits
                    T1 = erlmd_tokeniser:clear_state(T, list_item_value_size),
                    {nok, T1}
            end;
        _ ->
            T1 = erlmd_tokeniser:clear_state(T, list_item_value_size),
            {nok, T1}
    end.

-spec marker(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc At list item marker.
%%
%% ```markdown
%% > | * a
%%     ^
%% > | 1. b
%%      ^
%% ```
marker(T) ->
    T1 = erlmd_tokeniser:enter(T, list_item_marker),
    T2 = erlmd_tokeniser:consume(T1),
    T3 = erlmd_tokeniser:exit(T2, list_item_marker),
    %% For now, just succeed - we'll implement whitespace parsing in Day 2
    {{next, list_item_after}, T3}.

-spec after_marker(erlmd_tokeniser:tokenizer()) ->
    {erlmd_tokeniser:state_result(), erlmd_tokeniser:tokenizer()}.
%% @doc After list item marker.
%% Simple placeholder - full implementation in Day 2.
after_marker(T) ->
    %% For now, just exit the prefix and succeed
    T1 = erlmd_tokeniser:exit(T, list_item_prefix),
    {ok, T1}.
