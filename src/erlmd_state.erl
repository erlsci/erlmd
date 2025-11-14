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
        %% Phase 2: Only return 'ok' stub
        %% These will be implemented in later phases
        document_start -> stub(StateName, Tokenizer);

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
