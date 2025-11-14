%%%-----------------------------------------------------------------------------
%%% @doc Internal tokenizer record definitions.
%%%
%%% These records are internal implementation details of the tokenizer.
%%% They are separated into this file to allow testing while keeping them
%%% private from the public API.
%%% @end
%%%-----------------------------------------------------------------------------

-ifndef(ERLMD_TOKENIZER_INTERNAL_HRL).
-define(ERLMD_TOKENIZER_INTERNAL_HRL, true).

-record(tokenizer, {
    %% Input and position
    bytes :: binary(),                    % Complete input
    index = 0 :: non_neg_integer(),       % Current byte position (0-indexed)
    line = 1 :: pos_integer(),            % Current line (1-indexed)
    column = 1 :: pos_integer(),          % Current column (1-indexed)
    vs = 0 :: non_neg_integer(),          % Virtual steps (for tabs)

    %% Byte tracking
    previous = undefined :: byte() | undefined,  % Previous byte
    current = undefined :: byte() | undefined,   % Current byte
    consumed = true :: boolean(),         % Whether current byte is consumed

    %% Output
    events = [] :: [event()],             % Accumulated events (reversed)
    stack = [] :: [atom()],               % Token type stack

    %% Backtracking
    attempts = [] :: [attempt()],         % Attempt stack

    %% Context
    parse_state :: map(),                 % Global parse state
    options :: map()                      % Parse options
}).

-record(attempt, {
    ok :: state_result(),                 % Success continuation
    nok :: state_result(),                % Failure continuation
    kind :: attempt | check,              % Discard on failure or always
    progress = undefined :: progress() | undefined  % Saved state
}).

-record(progress, {
    events_len :: non_neg_integer(),
    stack_len :: non_neg_integer(),
    previous :: byte() | undefined,
    current :: byte() | undefined,
    index :: non_neg_integer(),
    line :: pos_integer(),
    column :: pos_integer(),
    vs :: non_neg_integer()
}).

%% Type declarations (after records are defined)
-type state_result() :: ok | nok | {next, atom()} | {retry, atom()}.
-type attempt() :: #attempt{}.
-type progress() :: #progress{}.

-endif. % ERLMD_TOKENIZER_INTERNAL_HRL
