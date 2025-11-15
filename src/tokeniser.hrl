%%%-----------------------------------------------------------------------------
%%% @doc Internal tokenizer record definitions.
%%%
%%% These records are internal implementation details of the tokenizer.
%%% They are separated into this file to allow testing while keeping them
%%% private from the public API.
%%% @end
%%%-----------------------------------------------------------------------------

-ifndef(ERLMD_TOKENISER_HRL).
-define(ERLMD_TOKENISER_HRL, true).

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
    options :: map(),                     % Parse options

    %% Phase 7: Label tracking (links, images)
    label_starts = [] :: [label_start()],       % Active label starts
    label_starts_loose = [] :: [label_start()], % Failed label starts
    labels = [] :: [label()],                   % Matched labels
    end_index = 0 :: non_neg_integer(),         % Temp: end event index

    %% Phase 7: Label parsing state
    label_size = 0 :: non_neg_integer(),      % Current label size
    label_seen_char = false :: boolean(),     % Seen non-whitespace in label

    %% Phase 7: Destination parsing state
    paren_depth = 0 :: non_neg_integer(),     % Parenthesis nesting depth

    %% Phase 7: Title/marker state
    marker = 0 :: byte() | non_neg_integer(), % Current marker character

    %% Phase 7: Token names for partial constructs
    token_1 = undefined :: atom() | undefined,
    token_2 = undefined :: atom() | undefined,
    token_3 = undefined :: atom() | undefined,
    token_4 = undefined :: atom() | undefined,
    token_5 = undefined :: atom() | undefined,

    %% Phase 7: Resolvers
    resolvers = [] :: [atom()],                 % Pending resolvers
    resolver_before = [] :: [atom()]            % Resolvers to run before others
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
    consumed :: boolean(),
    index :: non_neg_integer(),
    line :: pos_integer(),
    column :: pos_integer(),
    vs :: non_neg_integer(),

    %% Phase 7: Label state
    label_starts_len = 0 :: non_neg_integer(),
    label_starts_loose_len = 0 :: non_neg_integer(),
    labels_len = 0 :: non_neg_integer(),
    end_index = 0 :: non_neg_integer(),
    label_size = 0 :: non_neg_integer(),
    label_seen_char = false :: boolean(),
    paren_depth = 0 :: non_neg_integer(),
    marker = 0 :: byte() | non_neg_integer(),
    token_1 = undefined :: atom() | undefined,
    token_2 = undefined :: atom() | undefined,
    token_3 = undefined :: atom() | undefined,
    token_4 = undefined :: atom() | undefined,
    token_5 = undefined :: atom() | undefined,
    resolvers_len = 0 :: non_neg_integer(),
    resolver_before_len = 0 :: non_neg_integer()
}).

%% Type declarations (after records are defined)
-type state_result() :: ok | nok | {next, atom()} | {retry, atom()}.
-type attempt() :: #attempt{}.
-type progress() :: #progress{}.

-endif. % ERLMD_TOKENISER_HRL
