%%%-----------------------------------------------------------------------------
%%% @doc State dispatcher - Maps state names to construct functions.
%%%
%%% This module is THE central dispatch point for all state transitions.
%%% Each state name maps to a construct module and function.
%%%
%%% In Phase 2, we only implement stubs. Real constructs come in later phases.
%%%
%%% Based on markdown-rs src/state.rs
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_state).

-include("types.hrl").

-export([call/2]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

-spec call(atom(), erlmd_tokenizer:tokenizer()) ->
    {erlmd_tokenizer:state_result(), erlmd_tokenizer:tokenizer()}.
%% @doc Dispatch to a state function by name.
%%
%% This is THE central dispatch point for all state transitions.
%% Each state name maps to a construct module and function.
%%
%% In Phase 2, we only implement stubs. Real constructs come in later phases.
call(StateName, Tokenizer) ->
    case StateName of
        %% Phase 2: Document start stub
        document_start -> stub(StateName, Tokenizer);

        %% Phase 3: Data construct
        data_start -> erlmd_cnstr_prtl_data:start(Tokenizer);
        data_at_break -> erlmd_cnstr_prtl_data:at_break(Tokenizer);
        data_inside -> erlmd_cnstr_prtl_data:inside(Tokenizer);

        %% Phase 3: Blank line construct
        blank_line_start -> erlmd_cnstr_blank_line:start(Tokenizer);
        parse_whitespace -> erlmd_cnstr_blank_line:parse_whitespace(Tokenizer);
        check_after_whitespace -> erlmd_cnstr_blank_line:check_after_whitespace(Tokenizer);
        blank_line_after -> erlmd_cnstr_blank_line:after_whitespace(Tokenizer);

        %% Phase 3: Space or tab construct
        space_or_tab_start -> erlmd_cnstr_prtl_space_or_tab:start(Tokenizer);
        space_or_tab_inside -> erlmd_cnstr_prtl_space_or_tab:inside(Tokenizer);
        space_or_tab_after -> erlmd_cnstr_prtl_space_or_tab:after_space_or_tab(Tokenizer);

        %% More states will be added in future phases:
        %% flow_start -> erlmd_construct_flow:start(Tokenizer);
        %% paragraph_start -> erlmd_construct_paragraph:start(Tokenizer);
        %% etc.

        _ ->
            %% Unknown state
            error({unknown_state, StateName})
    end.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private
%% Stub for unimplemented states
stub(_StateName, Tokenizer) ->
    %% In Phase 2, just return ok
    %% In later phases, this will error if called
    {ok, Tokenizer}.
