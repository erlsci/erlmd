%%%-----------------------------------------------------------------------------
%%% @doc Partial label construct - parses label text `[...]`.
%%%
%%% Used for parsing reference labels in links and images.
%%% Labels have constraints:
%%% - Maximum 999 characters
%%% - No blank lines
%%% - At least 1 non-whitespace character
%%% - Nested `[` not allowed
%%%
%%% Examples:
%%%   [ref]          - Simple label
%%%   [my ref]       - Label with spaces
%%%   [foo\]bar]     - Label with escaped ]
%%%
%%% Reference: markdown-rs src/construct/partial_label.rs
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmd_cnstr_prtl_label).

-export([start/1, at_break/1, label_inside/1, label_escape/1,
         eol_after/1, label_nok/1]).

-include("types.hrl").
-include("tokeniser.hrl").

-define(MAX_LABEL_SIZE, 999).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Entry point for label parsing.
%%
%% Expects to be on `[` and uses token names from tokenizer state.
start(T) ->
    case erlmd_tokeniser:current(T) of
        $[ ->
            %% Get token names (set by caller)
            Token1 = erlmd_tokeniser:get_token_1(T),
            Token2 = erlmd_tokeniser:get_token_2(T),
            Token3 = erlmd_tokeniser:get_token_3(T),

            %% Enter the label structure
            T1 = erlmd_tokeniser:enter(T, Token1),
            T2 = erlmd_tokeniser:enter(T1, Token2),
            T3 = erlmd_tokeniser:consume(T2),
            T4 = erlmd_tokeniser:exit(T3, Token2),
            T5 = erlmd_tokeniser:enter(T4, Token3),

            %% Reset label parsing state
            T6 = erlmd_tokeniser:set_label_size(T5, 0),
            T7 = erlmd_tokeniser:set_label_seen_char(T6, false),

            {{next, prtl_label_at_break}, T7};
        _ ->
            {nok, T}
    end.

%% @doc At a break point - check what to do next.
at_break(T) ->
    Size = erlmd_tokeniser:get_label_size(T),
    SeenChar = erlmd_tokeniser:get_label_seen_char(T),

    %% Check size constraint
    if
        Size > ?MAX_LABEL_SIZE ->
            {{retry, prtl_label_nok}, T};
        true ->
            case erlmd_tokeniser:current(T) of
                $[ ->
                    %% Nested [ not allowed
                    {{retry, prtl_label_nok}, T};
                $] when not SeenChar ->
                    %% Empty label not allowed
                    {{retry, prtl_label_nok}, T};
                $] ->
                    %% Success! Found closing ]
                    Token2 = erlmd_tokeniser:get_token_2(T),
                    Token3 = erlmd_tokeniser:get_token_3(T),
                    Token1 = erlmd_tokeniser:get_token_1(T),

                    T1 = erlmd_tokeniser:exit(T, Token3),
                    T2 = erlmd_tokeniser:enter(T1, Token2),
                    T3 = erlmd_tokeniser:consume(T2),
                    T4 = erlmd_tokeniser:exit(T3, Token2),
                    T5 = erlmd_tokeniser:exit(T4, Token1),

                    %% Clear state
                    T6 = erlmd_tokeniser:set_label_size(T5, 0),
                    T7 = erlmd_tokeniser:set_label_seen_char(T6, false),

                    {ok, T7};
                $\n ->
                    %% Line ending - check if blank line follows
                    T1 = erlmd_tokeniser:attempt(T, {next, prtl_label_eol_after},
                                                    {next, prtl_label_nok}),
                    {{retry, space_or_tab_eol}, T1};
                _ ->
                    %% Parse data
                    {{retry, prtl_label_inside}, T}
            end
    end.

%% @doc Parse label content.
label_inside(T) ->
    Size = erlmd_tokeniser:get_label_size(T),

    if
        Size > ?MAX_LABEL_SIZE ->
            {{retry, prtl_label_at_break}, T};
        true ->
            case erlmd_tokeniser:current(T) of
                C when C =:= $\n; C =:= $[; C =:= $] ->
                    {{retry, prtl_label_at_break}, T};
                $\\ ->
                    T1 = erlmd_tokeniser:consume(T),
                    T2 = erlmd_tokeniser:inc_label_size(T1),
                    {{next, prtl_label_escape}, T2};
                C ->
                    T1 = erlmd_tokeniser:consume(T),
                    T2 = erlmd_tokeniser:inc_label_size(T1),
                    %% Mark if we've seen non-whitespace
                    T3 = if
                        C =/= $\t andalso C =/= $\s ->
                            erlmd_tokeniser:set_label_seen_char(T2, true);
                        true ->
                            T2
                    end,
                    {{next, prtl_label_inside}, T3}
            end
    end.

%% @doc Handle escape sequence in label.
label_escape(T) ->
    case erlmd_tokeniser:current(T) of
        C when C =:= $[; C =:= $\\; C =:= $] ->
            T1 = erlmd_tokeniser:consume(T),
            T2 = erlmd_tokeniser:inc_label_size(T1),
            {{next, prtl_label_inside}, T2};
        _ ->
            {{retry, prtl_label_inside}, T}
    end.

%% @doc After consuming EOL whitespace.
eol_after(T) ->
    {{retry, prtl_label_at_break}, T}.

%% @doc Label parsing failed.
label_nok(T) ->
    T1 = erlmd_tokeniser:set_label_size(T, 0),
    T2 = erlmd_tokeniser:set_label_seen_char(T1, false),
    {nok, T2}.
