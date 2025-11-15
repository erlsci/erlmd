# Phase 7: Complex Inline Constructs - Implementation Guide

**Project**: erlmd (Erlang Markdown Parser)  
**Phase**: 7 - Complex Inline (Emphasis, Links, Images)  
**Duration**: 2 weeks  
**Complexity**: HIGH  
**Date**: November 15, 2025

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture Context](#architecture-context)
3. [Module Specifications](#module-specifications)
4. [Implementation Sequence](#implementation-sequence)
5. [Critical Patterns & Pitfalls](#critical-patterns--pitfalls)
6. [Testing Strategy](#testing-strategy)
7. [Acceptance Criteria](#acceptance-criteria)

---

## Overview

### What We're Building

Phase 7 implements the most complex inline constructs in CommonMark:

1. **Attention (emphasis/strong)** - `*emphasis*`, `**strong**`, `_emphasis_`, `__strong__`
2. **Links** - `[text](url)`, `[text][ref]`, `[text][]`, `[text]`
3. **Images** - `![alt](url)`, `![alt][ref]`, `![alt][]`, `![alt]`
4. **Resolution logic** - Two-pass matching algorithm for delimiters

### Why This Is Complex

Unlike previous constructs, these require:

- **Two-pass parsing**: Mark potential delimiters first, resolve matches second
- **Delimiter matching**: Opening/closing pairs with complex rules
- **Nesting constraints**: Links cannot contain other links
- **Backtracking**: Label ends may or may not match
- **Multiple reference types**: Resource, full, collapsed, shortcut
- **State tracking**: Active label starts, balanced brackets

### Dependencies

**Required Modules (from previous phases)**:
- `erlmd_tokenizer.erl` - State machine
- `erlmd_state.erl` - State dispatcher
- `erlmd_event.erl` - Event types
- `erlmd_cnstr_text.erl` - Text content dispatcher
- `erlmd_cnstr_prtl_data.erl` - Data construct
- `erlmd_util_char.erl` - Character classification
- `erlmd_util_normalize_id.erl` - Identifier normalization

**New Utility Needed**:
- `erlmd_util_sanitize_uri.erl` - URI encoding (can stub initially)

---

## Architecture Context

### Event-Driven Two-Pass Architecture

The key innovation in markdown-rs (and what we must replicate) is the two-pass approach:

#### Pass 1: Token Generation
```
Input: "*hello*"
  ↓ Tokenizer
Events: [
  Enter(AttentionSequence),
  Exit(AttentionSequence),    % The opening *
  Enter(Data), Exit(Data),    % hello
  Enter(AttentionSequence),
  Exit(AttentionSequence)     % The closing *
]
```

#### Pass 2: Resolution
```
Events from Pass 1
  ↓ Resolver
Resolved Events: [
  Enter(Emphasis),
  Enter(EmphasisSequence),
  Exit(EmphasisSequence),
  Enter(EmphasisText),
  Enter(Data), Exit(Data),
  Exit(EmphasisText),
  Enter(EmphasisSequence),
  Exit(EmphasisSequence),
  Exit(Emphasis)
]
```

### Link/Image Architecture

Links and images use a different pattern:

1. **Label Start**: Marks opening `[` (or `![` for images)
2. **Content Parsing**: Parse text content between brackets
3. **Label End**: Matches closing `]` and looks for destination
4. **Resolution**: Pairs starts with ends, marks failures as data

### Critical Data Structures

From Rust's `tokenizer.rs`, we need these in our tokenizer state:

```erlang
-record(tokenizer, {
    %% Existing fields...
    
    %% NEW for Phase 7:
    label_starts = [] :: [label_start()],         % Active [ or ![
    label_starts_loose = [] :: [label_start()],   % Failed starts
    labels = [] :: [label()],                     % Matched pairs
    definitions = #{} :: #{binary() => definition()}, % Link refs
    
    %% Temporary state during label end processing:
    end_index = 0 :: non_neg_integer()
}).

-record(label_start, {
    kind :: link | image | gfm_footnote | gfm_undefined_footnote,
    start :: {non_neg_integer(), non_neg_integer()}, % Event indices
    inactive = false :: boolean()  % True if inside another link
}).

-record(label, {
    kind :: link | image | gfm_footnote | gfm_undefined_footnote,
    start :: {non_neg_integer(), non_neg_integer()}, % Event indices  
    end :: {non_neg_integer(), non_neg_integer()}     % Event indices
}).
```

---

## Module Specifications

### Module 1: `erlmd_cnstr_attention.erl`

**Purpose**: Mark potential emphasis/strong delimiters for later resolution.

**Reference**: `markdown-rs/src/construct/attention.rs`

**Key Algorithm**: 
- Count consecutive `*` or `_` characters
- Determine if sequence can open/close based on surrounding context
- Store as `AttentionSequence` event for resolver

#### Implementation

```erlang
-module(erlmd_cnstr_attention).
-export([start/1, inside/1, resolve/1]).

-include("types.hrl").

%% Entry point called by text dispatcher
-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $*; C =:= $_ ->
            %% Store the marker for comparison
            T1 = erlmd_tokenizer:set_marker(T, C),
            T2 = erlmd_tokenizer:enter(T1, attention_sequence),
            {retry, inside, T2};
        _ ->
            {nok, T}
    end.

%% Count consecutive markers
-spec inside(tokenizer()) -> {state_result(), tokenizer()}.
inside(T) ->
    Marker = erlmd_tokenizer:get_marker(T),
    case erlmd_tokenizer:current(T) of
        Marker ->
            T1 = erlmd_tokenizer:consume(T),
            {next, inside, T1};
        _ ->
            %% Sequence complete, register for resolution
            T1 = erlmd_tokenizer:exit(T, attention_sequence),
            T2 = erlmd_tokenizer:register_resolver(T1, attention),
            T3 = erlmd_tokenizer:set_marker(T2, 0),
            {ok, T3}
    end.

%% Resolution algorithm - this is the complex part!
-spec resolve(tokenizer()) -> tokenizer().
resolve(T) ->
    %% This implements the CommonMark emphasis/strong matching algorithm
    %% See detailed implementation below
    Sequences = get_sequences(T),
    T1 = match_sequences(T, Sequences),
    mark_remaining_as_data(T1, Sequences).
```

#### Attention Resolution Algorithm

This is the heart of emphasis/strong parsing. From CommonMark spec §6.2:

**Rules**:
1. A delimiter run can open emphasis/strong if:
   - It is followed by punctuation AND preceded by whitespace/punctuation
   - OR it is followed by any non-whitespace character
   - Special rule for `_`: cannot open if preceded by alphanumeric

2. A delimiter run can close emphasis/strong if:
   - It is preceded by punctuation AND followed by whitespace/punctuation  
   - OR it is preceded by any non-whitespace character
   - Special rule for `_`: cannot close if followed by alphanumeric

3. Matching pairs:
   - Take the first closer
   - Walk backward to find a matching opener
   - Must have same marker (`*` or `_`)
   - Must not have mismatched emphasis (see "rule of 3" below)
   - For emphasis: take 1 marker from each side
   - For strong: take 2 markers from each side

4. **Rule of 3** (most complex):
   - If opener can both open and close, AND closer can both open and close
   - AND the sum of their lengths is divisible by 3
   - BUT the closer's length is NOT divisible by 3
   - THEN don't match (prevents `a***b` from matching)

**Implementation Pattern**:

```erlang
%% Get all attention sequences with metadata
get_sequences(T) ->
    Events = erlmd_tokenizer:events(T),
    Bytes = erlmd_tokenizer:bytes(T),
    get_sequences(Events, Bytes, 0, [], []).

get_sequences([], _Bytes, _Index, _Stack, Acc) ->
    lists:reverse(Acc);
get_sequences([Event | Rest], Bytes, Index, Stack, Acc) ->
    case Event of
        #event{kind = enter, name = attention_sequence} ->
            %% Found a sequence, analyze it
            ExitEvent = lists:nth(2, Rest),  % Next event is exit
            
            %% Get the marker byte
            Marker = binary:at(Bytes, Event#event.point#point.offset),
            
            %% Get character before and after sequence
            BeforeChar = char_before_index(Bytes, Event#event.point#point.offset),
            AfterChar = char_after_index(Bytes, ExitEvent#event.point#point.offset),
            
            %% Classify characters
            Before = erlmd_util_char:classify_opt(BeforeChar),
            After = erlmd_util_char:classify_opt(AfterChar),
            
            %% Determine if can open/close
            {CanOpen, CanClose} = can_open_close(Marker, Before, After),
            
            Sequence = #attention_sequence{
                marker = Marker,
                index = Index,
                start_point = Event#event.point,
                end_point = ExitEvent#event.point,
                size = ExitEvent#event.point#point.offset - 
                       Event#event.point#point.offset,
                open = CanOpen,
                close = CanClose,
                stack = Stack  % Track nesting level
            },
            
            get_sequences(Rest, Bytes, Index + 1, Stack, [Sequence | Acc]);
            
        #event{kind = enter} ->
            %% Track nesting (for balanced brackets, etc.)
            get_sequences(Rest, Bytes, Index + 1, [Index | Stack], Acc);
            
        #event{kind = exit} ->
            [_ | NewStack] = Stack,
            get_sequences(Rest, Bytes, Index + 1, NewStack, Acc);
            
        _ ->
            get_sequences(Rest, Bytes, Index + 1, Stack, Acc)
    end.

%% Determine if sequence can open/close based on surrounding context
can_open_close(Marker, Before, After) ->
    %% After classification
    CanOpenPrelim = 
        After =:= other orelse
        (After =:= punctuation andalso Before =/= other) orelse
        %% Can open if followed by another emphasis marker
        (Marker =/= $~ andalso lists:member(After, [$*, $_])),
    
    CanClosePrelim = 
        Before =:= other orelse
        (Before =:= punctuation andalso After =/= other) orelse
        (Marker =/= $~ andalso lists:member(Before, [$*, $_])),
    
    %% Special rules for underscore
    CanOpen = if
        Marker =:= $_ ->
            CanOpenPrelim andalso (Before =/= other orelse not CanClosePrelim);
        true ->
            CanOpenPrelim
    end,
    
    CanClose = if
        Marker =:= $_ ->
            CanClosePrelim andalso (After =/= other orelse not CanOpenPrelim);
        true ->
            CanClosePrelim
    end,
    
    {CanOpen, CanClose}.

%% Match sequences using inside-out algorithm
match_sequences(T, Sequences) ->
    match_sequences(T, Sequences, 0).

match_sequences(T, Sequences, CloseIdx) when CloseIdx >= length(Sequences) ->
    T;
match_sequences(T, Sequences, CloseIdx) ->
    Closer = lists:nth(CloseIdx + 1, Sequences),
    
    case Closer#attention_sequence.close of
        true ->
            %% Try to find matching opener
            case find_opener(Sequences, CloseIdx) of
                {ok, OpenIdx} ->
                    %% Match found, create emphasis/strong events
                    T1 = create_emphasis_events(T, Sequences, OpenIdx, CloseIdx),
                    %% Update sequences list
                    {NewSequences, NextIdx} = 
                        update_sequences(Sequences, OpenIdx, CloseIdx),
                    match_sequences(T1, NewSequences, NextIdx);
                    
                not_found ->
                    match_sequences(T, Sequences, CloseIdx + 1)
            end;
        false ->
            match_sequences(T, Sequences, CloseIdx + 1)
    end.

%% Find matching opener for a closer
find_opener(Sequences, CloseIdx) ->
    find_opener(Sequences, CloseIdx, CloseIdx - 1).

find_opener(_Sequences, _CloseIdx, OpenIdx) when OpenIdx < 0 ->
    not_found;
find_opener(Sequences, CloseIdx, OpenIdx) ->
    Opener = lists:nth(OpenIdx + 1, Sequences),
    Closer = lists:nth(CloseIdx + 1, Sequences),
    
    %% Check if this opener matches
    if
        Opener#attention_sequence.open andalso
        Opener#attention_sequence.marker =:= Closer#attention_sequence.marker andalso
        Opener#attention_sequence.stack =:= Closer#attention_sequence.stack ->
            
            %% Check rule of 3
            if
                (Opener#attention_sequence.close orelse 
                 Closer#attention_sequence.open) andalso
                (Closer#attention_sequence.size rem 3 =/= 0) andalso
                ((Opener#attention_sequence.size + 
                  Closer#attention_sequence.size) rem 3 =:= 0) ->
                    %% Rule of 3 violation, keep searching
                    find_opener(Sequences, CloseIdx, OpenIdx - 1);
                true ->
                    {ok, OpenIdx}
            end;
        true ->
            find_opener(Sequences, CloseIdx, OpenIdx - 1)
    end.
```

### Module 2: `erlmd_cnstr_label_start_link.erl`

**Purpose**: Mark opening `[` as potential link start.

**Reference**: `markdown-rs/src/construct/label_start_link.rs`

**Key Points**:
- Very simple construct - just marks the `[`
- Adds to `label_starts` list
- Actual matching happens in `label_end`

```erlang
-module(erlmd_cnstr_label_start_link).
-export([start/1]).

-include("types.hrl").

-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        $[ ->
            StartIdx = erlmd_tokenizer:event_count(T),
            T1 = erlmd_tokenizer:enter(T, label_link),
            T2 = erlmd_tokenizer:enter(T1, label_marker),
            T3 = erlmd_tokenizer:consume(T2),
            T4 = erlmd_tokenizer:exit(T3, label_marker),
            T5 = erlmd_tokenizer:exit(T4, label_link),
            
            %% Add to label starts list
            LabelStart = #label_start{
                kind = link,
                start = {StartIdx, erlmd_tokenizer:event_count(T5) - 1},
                inactive = false
            },
            T6 = erlmd_tokenizer:add_label_start(T5, LabelStart),
            
            %% Register resolver
            T7 = erlmd_tokenizer:register_resolver_before(T6, label),
            
            {ok, T7};
        _ ->
            {nok, T}
    end.
```

### Module 3: `erlmd_cnstr_label_start_image.erl`

**Purpose**: Mark opening `![` as potential image start.

**Reference**: `markdown-rs/src/construct/label_start_image.rs`

**Key Points**:
- Similar to link start but with `!` prefix
- Must check that `^` doesn't follow (GFM footnote conflict)

```erlang
-module(erlmd_cnstr_label_start_image).
-export([start/1, open/1, after_open/1]).

-include("types.hrl").

-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        $! ->
            T1 = erlmd_tokenizer:enter(T, label_image),
            T2 = erlmd_tokenizer:enter(T1, label_image_marker),
            T3 = erlmd_tokenizer:consume(T2),
            T4 = erlmd_tokenizer:exit(T3, label_image_marker),
            {next, open, T4};
        _ ->
            {nok, T}
    end.

-spec open(tokenizer()) -> {state_result(), tokenizer()}.
open(T) ->
    case erlmd_tokenizer:current(T) of
        $[ ->
            T1 = erlmd_tokenizer:enter(T, label_marker),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2, label_marker),
            T4 = erlmd_tokenizer:exit(T3, label_image),
            {next, after_open, T4};
        _ ->
            {nok, T}
    end.

-spec after_open(tokenizer()) -> {state_result(), tokenizer()}.
after_open(T) ->
    %% Don't allow ![^...] (conflicts with GFM footnotes)
    case erlmd_tokenizer:current(T) of
        $^ ->
            {nok, T};
        _ ->
            %% Add to label starts
            StartIdx = erlmd_tokenizer:event_count(T) - 6,
            EndIdx = erlmd_tokenizer:event_count(T) - 1,
            
            LabelStart = #label_start{
                kind = image,
                start = {StartIdx, EndIdx},
                inactive = false
            },
            T1 = erlmd_tokenizer:add_label_start(T, LabelStart),
            T2 = erlmd_tokenizer:register_resolver_before(T1, label),
            
            {ok, T2}
    end.
```

### Module 4: `erlmd_cnstr_label_end.erl`

**Purpose**: Match closing `]` and look for destination/reference.

**Reference**: `markdown-rs/src/construct/label_end.rs`

**This is the MOST COMPLEX module in Phase 7.**

**Key Algorithm**:
1. Check if there's a matching `[` or `![` 
2. If found, check if it's inactive (inside another link)
3. Try to parse resource: `](url)` or `](url "title")`
4. Try to parse full reference: `][ref]`
5. Try to parse collapsed reference: `][]`
6. Try shortcut reference (just `]`)
7. Match against definitions in parse state
8. Mark unmatched starts as data

```erlang
-module(erlmd_cnstr_label_end).
-export([start/1, after_marker/1, ok/1, nok/1, resolve/1]).
-export([
    resource_start/1, resource_before/1, resource_open/1,
    resource_destination_after/1, resource_destination_missing/1,
    resource_between/1, resource_title_after/1, resource_end/1,
    reference_full/1, reference_full_after/1, reference_full_missing/1,
    reference_not_full/1, reference_collapsed/1, reference_collapsed_open/1
]).

-include("types.hrl").

-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        $] ->
            %% Check if there's a matching opening
            case erlmd_tokenizer:has_label_starts(T) of
                true ->
                    LabelStart = erlmd_tokenizer:peek_label_start(T),
                    
                    %% Store end index for later use
                    T1 = erlmd_tokenizer:set_end_index(T, 
                        erlmd_tokenizer:event_count(T)),
                    
                    %% Check if inactive (inside another link)
                    case LabelStart#label_start.inactive of
                        true ->
                            {retry, nok, T1};
                        false ->
                            %% Valid label end, consume the ]
                            T2 = erlmd_tokenizer:enter(T1, label_end),
                            T3 = erlmd_tokenizer:enter(T2, label_marker),
                            T4 = erlmd_tokenizer:consume(T3),
                            T5 = erlmd_tokenizer:exit(T4, label_marker),
                            T6 = erlmd_tokenizer:exit(T5, label_end),
                            {next, after_marker, T6}
                    end;
                false ->
                    {nok, T}
            end;
        _ ->
            {nok, T}
    end.

-spec after_marker(tokenizer()) -> {state_result(), tokenizer()}.
after_marker(T) ->
    LabelStart = erlmd_tokenizer:peek_label_start(T),
    
    %% Get label text for matching against definitions
    {StartOffset, EndOffset} = get_label_offsets(T, LabelStart),
    LabelText = get_label_text(T, StartOffset, EndOffset),
    NormalizedId = erlmd_util_normalize_id:normalize(LabelText),
    
    %% Check if this is defined
    Defined = erlmd_tokenizer:has_definition(T, NormalizedId),
    
    %% Try different reference types
    case erlmd_tokenizer:current(T) of
        $( ->
            %% Try resource: [text](url) or [text](url "title")
            {attempt, {next, ok}, 
                     if Defined -> {next, ok}; true -> {next, nok} end,
                     {retry, resource_start}, T};
        $[ ->
            %% Try full reference: [text][ref]
            {attempt, {next, ok},
                     if Defined -> {next, reference_not_full}; 
                        true -> {next, nok} end,
                     {retry, reference_full}, T};
        _ ->
            %% Try shortcut reference: [text]
            if Defined -> {retry, ok, T};
               true -> {retry, nok, T}
            end
    end.

%% Resource parsing: [text](url) or [text](url "title")
-spec resource_start(tokenizer()) -> {state_result(), tokenizer()}.
resource_start(T) ->
    %% Must be on (
    $( = erlmd_tokenizer:current(T),
    
    T1 = erlmd_tokenizer:enter(T, resource),
    T2 = erlmd_tokenizer:enter(T1, resource_marker),
    T3 = erlmd_tokenizer:consume(T2),
    T4 = erlmd_tokenizer:exit(T3, resource_marker),
    {next, resource_before, T4}.

-spec resource_before(tokenizer()) -> {state_result(), tokenizer()}.
resource_before(T) ->
    %% Optional whitespace before destination
    case erlmd_tokenizer:current(T) of
        C when C =:= $\t; C =:= $\n; C =:= $\s ->
            {attempt, {next, resource_open}, {next, resource_open},
                     {retry, space_or_tab_eol}, T};
        _ ->
            {retry, resource_open, T}
    end.

-spec resource_open(tokenizer()) -> {state_result(), tokenizer()}.
resource_open(T) ->
    case erlmd_tokenizer:current(T) of
        $) ->
            %% Empty resource: [text]()
            {retry, resource_end, T};
        _ ->
            %% Parse destination
            T1 = erlmd_tokenizer:set_token_names(T, 
                resource_destination,
                resource_destination_literal,
                resource_destination_literal_marker,
                resource_destination_raw,
                resource_destination_string),
            
            {attempt, {next, resource_destination_after},
                     {next, resource_destination_missing},
                     {retry, destination_start}, T1}
    end.

%% ... (continue with other resource states)

%% Reference parsing: [text][ref] or [text][]
-spec reference_full(tokenizer()) -> {state_result(), tokenizer()}.
reference_full(T) ->
    %% Must be on [
    $[ = erlmd_tokenizer:current(T),
    
    T1 = erlmd_tokenizer:set_token_names(T,
        reference, reference_marker, reference_string, undefined, undefined),
    
    {attempt, {next, reference_full_after},
             {next, reference_full_missing},
             {retry, label_start}, T1}.

%% ... (continue with other reference states)

%% Success - we matched!
-spec ok(tokenizer()) -> {state_result(), tokenizer()}.
ok(T) ->
    %% Remove the label start from the stack
    {LabelStart, T1} = erlmd_tokenizer:pop_label_start(T),
    
    %% If this is a link (not image), mark earlier link starts as inactive
    %% (prevents nested links)
    T2 = case LabelStart#label_start.kind of
        image ->
            T1;
        _ ->
            erlmd_tokenizer:mark_link_starts_inactive(T1)
    end,
    
    %% Add matched label
    Label = #label{
        kind = LabelStart#label_start.kind,
        start = LabelStart#label_start.start,
        end_index = {
            erlmd_tokenizer:get_end_index(T2),
            erlmd_tokenizer:event_count(T2) - 1
        }
    },
    T3 = erlmd_tokenizer:add_label(T2, Label),
    T4 = erlmd_tokenizer:set_end_index(T3, 0),
    T5 = erlmd_tokenizer:register_resolver_before(T4, label),
    
    {ok, T5}.

%% Failure - didn't match
-spec nok(tokenizer()) -> {state_result(), tokenizer()}.
nok(T) ->
    %% Move start to loose list (will be marked as data later)
    {LabelStart, T1} = erlmd_tokenizer:pop_label_start(T),
    T2 = erlmd_tokenizer:add_loose_label_start(T1, LabelStart),
    T3 = erlmd_tokenizer:set_end_index(T2, 0),
    {nok, T3}.

%% Resolution - convert matched labels to link/image events
-spec resolve(tokenizer()) -> tokenizer().
resolve(T) ->
    %% Get all matched labels
    Labels = erlmd_tokenizer:get_labels(T),
    T1 = inject_labels(T, Labels),
    
    %% Mark unmatched starts as data
    Starts = erlmd_tokenizer:get_label_starts(T),
    T2 = mark_as_data(T1, Starts),
    
    LooseStarts = erlmd_tokenizer:get_loose_label_starts(T),
    T3 = mark_as_data(T2, LooseStarts),
    
    %% Clear state
    T4 = erlmd_tokenizer:clear_labels(T3),
    T5 = erlmd_tokenizer:clear_label_starts(T4),
    erlmd_tokenizer:clear_loose_label_starts(T5).
```

### Module 5: `erlmd_cnstr_prtl_label.erl`

**Purpose**: Parse label text `[...]` for references.

**Reference**: `markdown-rs/src/construct/partial_label.rs`

**Constraints**:
- Maximum 999 characters
- No blank lines
- At least 1 non-whitespace character

```erlang
-module(erlmd_cnstr_prtl_label).
-export([start/1, at_break/1, inside/1, escape/1, eol_after/1, nok/1]).

-include("types.hrl").
-define(MAX_LABEL_SIZE, 999).

-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    %% Must be on [
    $[ = erlmd_tokenizer:current(T),
    
    T1 = erlmd_tokenizer:enter(T, erlmd_tokenizer:get_token_1(T)),
    T2 = erlmd_tokenizer:enter(T1, erlmd_tokenizer:get_token_2(T)),
    T3 = erlmd_tokenizer:consume(T2),
    T4 = erlmd_tokenizer:exit(T3, erlmd_tokenizer:get_token_2(T)),
    T5 = erlmd_tokenizer:enter(T4, erlmd_tokenizer:get_token_3(T)),
    
    %% Reset label parsing state
    T6 = erlmd_tokenizer:set_label_size(T5, 0),
    T7 = erlmd_tokenizer:set_label_seen_char(T6, false),
    
    {next, at_break, T7}.

-spec at_break(tokenizer()) -> {state_result(), tokenizer()}.
at_break(T) ->
    Size = erlmd_tokenizer:get_label_size(T),
    SeenChar = erlmd_tokenizer:get_label_seen_char(T),
    
    %% Check constraints
    if
        Size > ?MAX_LABEL_SIZE ->
            {retry, nok, T};
        true ->
            case erlmd_tokenizer:current(T) of
                $[ ->
                    %% Nested [ not allowed
                    {retry, nok, T};
                $] when not SeenChar ->
                    %% Empty label not allowed
                    {retry, nok, T};
                $] ->
                    %% Success!
                    T1 = erlmd_tokenizer:exit(T, erlmd_tokenizer:get_token_3(T)),
                    T2 = erlmd_tokenizer:enter(T1, erlmd_tokenizer:get_token_2(T)),
                    T3 = erlmd_tokenizer:consume(T2),
                    T4 = erlmd_tokenizer:exit(T3, erlmd_tokenizer:get_token_2(T)),
                    T5 = erlmd_tokenizer:exit(T4, erlmd_tokenizer:get_token_1(T)),
                    
                    %% Clear state
                    T6 = erlmd_tokenizer:set_label_size(T5, 0),
                    T7 = erlmd_tokenizer:set_label_seen_char(T6, false),
                    {ok, T7};
                $\n ->
                    %% Line ending - check for blank line
                    {attempt, {next, eol_after}, {next, nok},
                             {retry, space_or_tab_eol}, T};
                _ ->
                    %% Parse data
                    {retry, inside, T}
            end
    end.

-spec inside(tokenizer()) -> {state_result(), tokenizer()}.
inside(T) ->
    Size = erlmd_tokenizer:get_label_size(T),
    
    if
        Size > ?MAX_LABEL_SIZE ->
            {retry, at_break, T};
        true ->
            case erlmd_tokenizer:current(T) of
                C when C =:= $\n; C =:= $[; C =:= $] ->
                    {retry, at_break, T};
                $\\ ->
                    T1 = erlmd_tokenizer:consume(T),
                    T2 = erlmd_tokenizer:inc_label_size(T1),
                    {next, escape, T2};
                C ->
                    T1 = erlmd_tokenizer:consume(T),
                    T2 = erlmd_tokenizer:inc_label_size(T1),
                    %% Mark if we've seen non-whitespace
                    T3 = if
                        C =/= $\t andalso C =/= $\s ->
                            erlmd_tokenizer:set_label_seen_char(T2, true);
                        true ->
                            T2
                    end,
                    {next, inside, T3}
            end
    end.

-spec escape(tokenizer()) -> {state_result(), tokenizer()}.
escape(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $[; C =:= $\\; C =:= $] ->
            T1 = erlmd_tokenizer:consume(T),
            T2 = erlmd_tokenizer:inc_label_size(T1),
            {next, inside, T2};
        _ ->
            {retry, inside, T}
    end.

-spec eol_after(tokenizer()) -> {state_result(), tokenizer()}.
eol_after(T) ->
    {retry, at_break, T}.

-spec nok(tokenizer()) -> {state_result(), tokenizer()}.
nok(T) ->
    T1 = erlmd_tokenizer:set_label_size(T, 0),
    T2 = erlmd_tokenizer:set_label_seen_char(T1, false),
    {nok, T2}.
```

### Module 6: `erlmd_cnstr_prtl_destination.erl`

**Purpose**: Parse URL destinations in resources.

**Reference**: `markdown-rs/src/construct/partial_destination.rs`

**Two Forms**:
1. Enclosed: `<https://example.com>`
2. Raw: `https://example.com` (with balanced parens)

```erlang
-module(erlmd_cnstr_prtl_destination).
-export([start/1, enclosed_before/1, enclosed/1, enclosed_escape/1,
         raw/1, raw_escape/1]).

-include("types.hrl").
-define(MAX_PAREN_DEPTH, 32).  % CommonMark limit

-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        $< ->
            %% Enclosed destination
            T1 = erlmd_tokenizer:enter(T, erlmd_tokenizer:get_token_1(T)),
            T2 = erlmd_tokenizer:enter(T1, erlmd_tokenizer:get_token_2(T)),
            T3 = erlmd_tokenizer:enter(T2, erlmd_tokenizer:get_token_3(T)),
            T4 = erlmd_tokenizer:consume(T3),
            T5 = erlmd_tokenizer:exit(T4, erlmd_tokenizer:get_token_3(T)),
            {next, enclosed_before, T5};
            
        C when C >= 16#01 andalso C =< 16#1F; C =:= $\s; C =:= $); C =:= 16#7F ->
            %% ASCII control, space, or closing paren - not allowed
            {nok, T};
            
        undefined ->
            {nok, T};
            
        _ ->
            %% Raw destination
            T1 = erlmd_tokenizer:enter(T, erlmd_tokenizer:get_token_1(T)),
            T2 = erlmd_tokenizer:enter(T1, erlmd_tokenizer:get_token_4(T)),
            T3 = erlmd_tokenizer:enter(T2, erlmd_tokenizer:get_token_5(T)),
            T4 = erlmd_tokenizer:set_paren_depth(T3, 0),
            {retry, raw, T4}
    end.

-spec raw(tokenizer()) -> {state_result(), tokenizer()}.
raw(T) ->
    Depth = erlmd_tokenizer:get_paren_depth(T),
    
    if
        Depth =:= 0 ->
            %% Check for end conditions
            case erlmd_tokenizer:current(T) of
                C when C =:= $\t; C =:= $\n; C =:= $\s; C =:= $); C =:= undefined ->
                    %% End of destination
                    T1 = erlmd_tokenizer:exit(T, erlmd_tokenizer:get_token_5(T)),
                    T2 = erlmd_tokenizer:exit(T1, erlmd_tokenizer:get_token_4(T)),
                    T3 = erlmd_tokenizer:exit(T2, erlmd_tokenizer:get_token_1(T)),
                    T4 = erlmd_tokenizer:set_paren_depth(T3, 0),
                    {ok, T4};
                $( ->
                    %% Opening paren
                    T1 = erlmd_tokenizer:consume(T),
                    T2 = erlmd_tokenizer:inc_paren_depth(T1),
                    {next, raw, T2};
                $\\ ->
                    T1 = erlmd_tokenizer:consume(T),
                    {next, raw_escape, T1};
                _ ->
                    T1 = erlmd_tokenizer:consume(T),
                    {next, raw, T1}
            end;
        Depth > ?MAX_PAREN_DEPTH ->
            %% Too deep!
            {nok, T};
        true ->
            %% Have open parens, continue
            case erlmd_tokenizer:current(T) of
                $( ->
                    T1 = erlmd_tokenizer:consume(T),
                    T2 = erlmd_tokenizer:inc_paren_depth(T1),
                    {next, raw, T2};
                $) ->
                    T1 = erlmd_tokenizer:consume(T),
                    T2 = erlmd_tokenizer:dec_paren_depth(T1),
                    {next, raw, T2};
                $\\ ->
                    T1 = erlmd_tokenizer:consume(T),
                    {next, raw_escape, T1};
                C when C >= 16#01 andalso C =< 16#1F; C =:= $\s; C =:= $); C =:= 16#7F ->
                    {nok, T};
                _ ->
                    T1 = erlmd_tokenizer:consume(T),
                    {next, raw, T1}
            end
    end.

%% ... (other state functions)
```

### Module 7: `erlmd_cnstr_prtl_title.erl`

**Purpose**: Parse optional title in resources.

**Reference**: `markdown-rs/src/construct/partial_title.rs`

**Three Forms**:
1. Double quoted: `"title"`
2. Single quoted: `'title'`
3. Parenthesized: `(title)`

```erlang
-module(erlmd_cnstr_prtl_title).
-export([start/1, begin/1, at_break/1, inside/1, escape/1, after_eol/1, nok/1]).

-include("types.hrl").

-spec start(tokenizer()) -> {state_result(), tokenizer()}.
start(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $"; C =:= $'; C =:= $( ->
            %% Store the marker (convert ( to ))
            Marker = if C =:= $( -> $); true -> C end,
            T1 = erlmd_tokenizer:set_marker(T, Marker),
            
            T2 = erlmd_tokenizer:enter(T1, erlmd_tokenizer:get_token_1(T)),
            T3 = erlmd_tokenizer:enter(T2, erlmd_tokenizer:get_token_2(T)),
            T4 = erlmd_tokenizer:consume(T3),
            T5 = erlmd_tokenizer:exit(T4, erlmd_tokenizer:get_token_2(T)),
            {next, begin, T5};
        _ ->
            {nok, T}
    end.

%% ... (rest similar to label)
```

---

## Implementation Sequence

### Week 1: Foundation & Simple Constructs

**Day 1-2: Tokenizer State Extensions**
```erlang
%% Add to erlmd_tokenizer.erl
-record(tokenizer, {
    %% ... existing fields ...
    
    %% Phase 7 additions:
    label_starts = [] :: [label_start()],
    label_starts_loose = [] :: [label_start()],
    labels = [] :: [label()],
    end_index = 0 :: non_neg_integer(),
    
    %% For label parsing:
    label_size = 0 :: non_neg_integer(),
    label_seen_char = false :: boolean(),
    
    %% For destination parsing:
    paren_depth = 0 :: non_neg_integer(),
    
    %% For title parsing:
    title_marker = 0 :: byte()
}).

%% Add accessor functions:
add_label_start(T, Start) -> ...
pop_label_start(T) -> ...
has_label_starts(T) -> ...
peek_label_start(T) -> ...
% ... etc
```

**Day 3-4: Label Start Constructs**
1. Implement `erlmd_cnstr_label_start_link.erl`
2. Implement `erlmd_cnstr_label_start_image.erl`
3. Add to text dispatcher
4. Write unit tests

**Day 5: Partial Constructs**
1. Implement `erlmd_cnstr_prtl_label.erl`
2. Implement `erlmd_cnstr_prtl_destination.erl`
3. Implement `erlmd_cnstr_prtl_title.erl`
4. Test in isolation

### Week 2: Complex Constructs & Resolution

**Day 6-7: Label End**
1. Implement `erlmd_cnstr_label_end.erl` (resource handling)
2. Test resource links: `[text](url)`
3. Test with titles: `[text](url "title")`

**Day 8: References**
1. Add definition support to parser
2. Implement full references: `[text][ref]`
3. Implement collapsed: `[text][]`
4. Implement shortcut: `[text]`

**Day 9-10: Attention**
1. Implement `erlmd_cnstr_attention.erl` (marking)
2. Implement resolution algorithm
3. Test emphasis: `*text*`, `_text_`
4. Test strong: `**text**`, `__text__`
5. Test nested: `***text***`

**Day 11: Integration & Testing**
1. Add all constructs to text dispatcher
2. Run CommonMark tests
3. Debug failures
4. Test edge cases

**Day 12-14: Polish**
1. Optimize hot paths
2. Add comprehensive tests
3. Document algorithms
4. Code review

---

## Critical Patterns & Pitfalls

### Pattern 1: Two-Pass Parsing

**DO**:
```erlang
%% First pass: mark sequences
start(T) ->
    T1 = erlmd_tokenizer:enter(T, attention_sequence),
    T2 = consume_markers(T1),
    T3 = erlmd_tokenizer:exit(T2, attention_sequence),
    T4 = erlmd_tokenizer:register_resolver(T3, attention),
    {ok, T4}.

%% Second pass: match and create final events
resolve(T) ->
    Sequences = get_sequences(T),
    match_and_create_events(T, Sequences).
```

**DON'T**:
```erlang
%% Wrong: trying to create emphasis events during parsing
start(T) ->
    T1 = erlmd_tokenizer:enter(T, emphasis),  % TOO EARLY!
    ...
```

### Pattern 2: Backtracking with Attempt

**DO**:
```erlang
after_bracket(T) ->
    %% Try resource first, fall back to reference
    {attempt, {next, ok}, {next, try_reference},
             {retry, resource_start}, T}.
```

**DON'T**:
```erlang
%% Wrong: no backtracking, will fail incorrectly
after_bracket(T) ->
    case parse_resource(T) of
        {ok, T1} -> {ok, T1};
        {nok, _} -> {nok, T}  % Lost state!
    end.
```

### Pattern 3: State Management

**DO**:
```erlang
%% Always clear temporary state when done
ok(T) ->
    {_, T1} = pop_label_start(T),
    T2 = set_end_index(T1, 0),
    T3 = set_label_size(T2, 0),
    {ok, T3}.
```

**DON'T**:
```erlang
%% Wrong: leaking state to next parse
ok(T) ->
    {_, T1} = pop_label_start(T),
    {ok, T1}.  % Still has end_index, label_size!
```

### Pattern 4: Event Manipulation

**DO**:
```erlang
%% Use edit map for modifying event stream
inject_events(T, Index, Events) ->
    EditMap = erlmd_tokenizer:get_edit_map(T),
    EditMap1 = edit_map:add(EditMap, Index, 0, Events),
    erlmd_tokenizer:set_edit_map(T, EditMap1).
```

**DON'T**:
```erlang
%% Wrong: directly modifying events list
inject_events(T, Index, Events) ->
    OldEvents = erlmd_tokenizer:events(T),
    NewEvents = insert_at(OldEvents, Index, Events),
    erlmd_tokenizer:set_events(T, NewEvents).  % Breaks indices!
```

### Pitfall 1: Nested Links

**Problem**: `[a [b](c) d](e)` - inner link should win

**Solution**:
```erlang
%% When link matches, mark earlier link starts as inactive
ok(T) ->
    case LabelStart#label_start.kind of
        link ->
            mark_earlier_links_inactive(T);
        image ->
            T  % Images can nest in links
    end.
```

### Pitfall 2: Rule of 3

**Problem**: `a***b` should NOT parse as emphasis

**Solution**:
```erlang
can_match(Opener, Closer) ->
    BothFlank = Opener#seq.close orelse Closer#seq.open,
    CloserMod3 = Closer#seq.size rem 3,
    SumMod3 = (Opener#seq.size + Closer#seq.size) rem 3,
    
    not (BothFlank andalso CloserMod3 =/= 0 andalso SumMod3 =:= 0).
```

### Pitfall 3: Balance Tracking

**Problem**: Must ensure brackets are balanced at same nesting level

**Solution**:
```erlang
%% Track stack depth when creating sequence
get_sequences(Events, Index, Stack, Acc) ->
    case Event of
        #event{kind = enter} ->
            get_sequences(Rest, Index + 1, [Index | Stack], Acc);
        _ ->
            Seq = #attention_sequence{stack = Stack, ...},
            get_sequences(Rest, Index + 1, Stack, [Seq | Acc])
    end.

%% Only match if stacks are equal
can_match(Opener, Closer) ->
    Opener#attention_sequence.stack =:= Closer#attention_sequence.stack.
```

---

## Testing Strategy

### Unit Tests

```erlang
-module(erlmd_cnstr_attention_test).
-include_lib("eunit/include/eunit.hrl").

simple_emphasis_test() ->
    Input = <<"*hello*">>,
    Events = test_helpers:parse_text(Input),
    ?assertMatch([
        #event{kind = enter, name = emphasis},
        #event{kind = enter, name = emphasis_sequence},
        #event{kind = exit, name = emphasis_sequence},
        #event{kind = enter, name = emphasis_text},
        #event{kind = enter, name = data},
        #event{kind = exit, name = data},
        #event{kind = exit, name = emphasis_text},
        #event{kind = enter, name = emphasis_sequence},
        #event{kind = exit, name = emphasis_sequence},
        #event{kind = exit, name = emphasis}
    ], Events).

strong_test() ->
    Input = <<"**hello**">>,
    Events = test_helpers:parse_text(Input),
    %% Should create Strong, not Emphasis
    Strong = [E || E <- Events, E#event.name =:= strong],
    ?assertEqual(2, length(Strong)).  % Enter + Exit

nested_emphasis_test() ->
    Input = <<"***hello***">>,
    Events = test_helpers:parse_text(Input),
    %% Should create Strong containing Emphasis
    ?assert(has_strong(Events)),
    ?assert(has_emphasis(Events)).

rule_of_3_test() ->
    Input = <<"a***b">>,
    Events = test_helpers:parse_text(Input),
    %% Should NOT create emphasis (rule of 3)
    ?assertEqual([], [E || E <- Events, E#event.name =:= emphasis]),
    ?assertEqual([], [E || E <- Events, E#event.name =:= strong]).

underscore_in_word_test() ->
    Input = <<"foo_bar_baz">>,
    Events = test_helpers:parse_text(Input),
    %% Underscores in words don't create emphasis
    ?assertEqual([], [E || E <- Events, E#event.name =:= emphasis]).
```

### Link Tests

```erlang
-module(erlmd_cnstr_label_end_test).
-include_lib("eunit/include/eunit.hrl").

simple_link_test() ->
    Input = <<"[text](url)">>,
    Events = test_helpers:parse_text(Input),
    ?assertMatch([
        #event{kind = enter, name = link},
        #event{kind = enter, name = label},
        #event{kind = enter, name = label_marker},
        #event{kind = exit, name = label_marker},
        #event{kind = enter, name = label_text},
        #event{kind = enter, name = data},
        #event{kind = exit, name = data},
        #event{kind = exit, name = label_text},
        #event{kind = enter, name = label_marker},
        #event{kind = exit, name = label_marker},
        #event{kind = exit, name = label},
        #event{kind = enter, name = resource},
        %% ... resource events
        #event{kind = exit, name = resource},
        #event{kind = exit, name = link}
    ], Events).

link_with_title_test() ->
    Input = <<"[text](url \"title\")">>,
    Events = test_helpers:parse_text(Input),
    ?assert(has_resource_title(Events)).

image_test() ->
    Input = <<"![alt](url)">>,
    Events = test_helpers:parse_text(Input),
    Images = [E || E <- Events, E#event.name =:= image],
    ?assertEqual(2, length(Images)).  % Enter + Exit

nested_link_test() ->
    Input = <<"[a [b](c) d](e)">>,
    Events = test_helpers:parse_text(Input),
    %% Inner link should win
    Links = [E || E <- Events, E#event.name =:= link, E#event.kind =:= enter],
    ?assertEqual(1, length(Links)),
    %% Outer [ should become data
    ?assert(has_unmatched_bracket(Events)).

reference_full_test() ->
    Input = <<"[text][ref]">>,
    Defs = #{"ref" => {"http://example.com", undefined}},
    Events = test_helpers:parse_text_with_defs(Input, Defs),
    ?assert(has_link(Events)),
    ?assert(has_reference(Events)).

reference_collapsed_test() ->
    Input = <<"[ref][]">>,
    Defs = #{"ref" => {"http://example.com", undefined}},
    Events = test_helpers:parse_text_with_defs(Input, Defs),
    ?assert(has_link(Events)).

reference_shortcut_test() ->
    Input = <<"[ref]">>,
    Defs = #{"ref" => {"http://example.com", undefined}},
    Events = test_helpers:parse_text_with_defs(Input, Defs),
    ?assert(has_link(Events)).

undefined_reference_test() ->
    Input = <<"[text][undefined]">>,
    Events = test_helpers:parse_text(Input),
    %% Should become data, not a link
    ?assertEqual([], [E || E <- Events, E#event.name =:= link]).
```

### CommonMark Spec Tests

From CommonMark spec sections:
- §6.2 - Emphasis and strong (Examples 350-489)
- §6.3 - Links (Examples 481-549)
- §6.4 - Images (Examples 550-567)

```erlang
%% Example 350
commonmark_350_test() ->
    Input = <<"*foo bar*">>,
    Expected = <<"<p><em>foo bar</em></p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Example 360 - strong
commonmark_360_test() ->
    Input = <<"**foo bar**">>,
    Expected = <<"<p><strong>foo bar</strong></p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Example 481 - link
commonmark_481_test() ->
    Input = <<"[link](/uri)">>,
    Expected = <<"<p><a href=\"/uri\">link</a></p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% Run all 230+ examples
run_all_inline_tests() ->
    load_commonmark_spec("spec.json"),
    Examples = filter_examples([350..567]),
    Results = [run_example(E) || E <- Examples],
    PassCount = length([R || R <- Results, R =:= pass]),
    io:format("Passed ~p/~p tests~n", [PassCount, length(Examples)]).
```

### Edge Case Tests

```erlang
%% Maximum label size
max_label_test() ->
    %% 999 characters - should work
    Input = iolist_to_binary(["[", lists:duplicate(999, $a), "]"]),
    Events = test_helpers:parse_text_with_defs(Input, #{"a..." => {"#", undefined}}),
    ?assert(has_link(Events)),
    
    %% 1000 characters - should fail
    Input2 = iolist_to_binary(["[", lists:duplicate(1000, $a), "]"]),
    Events2 = test_helpers:parse_text_with_defs(Input2, #{"a..." => {"#", undefined}}),
    ?assertNot(has_link(Events2)).

%% Deeply nested parens in destination
nested_parens_test() ->
    Input = <<"[link](url(with(nested)parens))">>,
    Events = test_helpers:parse_text(Input),
    ?assert(has_link(Events)).

%% Too many nested parens
too_many_parens_test() ->
    Parens = lists:duplicate(33, "(("),
    Input = iolist_to_binary(["[link](", Parens, "]"]),
    Events = test_helpers:parse_text(Input),
    ?assertNot(has_link(Events)).

%% Blank line in label
blank_line_in_label_test() ->
    Input = <<"[foo\n\nbar]">>,
    Defs = #{"foo\n\nbar" => {"#", undefined}},
    Events = test_helpers:parse_text_with_defs(Input, Defs),
    %% Should fail - blank lines not allowed
    ?assertNot(has_link(Events)).
```

---

## Acceptance Criteria

### Functional Requirements

- [ ] **Emphasis parsing**
  - [ ] Single asterisk creates emphasis: `*text*`
  - [ ] Single underscore creates emphasis: `_text_`
  - [ ] Underscore doesn't work inside words: `foo_bar_baz`
  - [ ] Nested emphasis works: `*a _b_ c*`
  - [ ] Rule of 3 enforced: `a***b` doesn't match

- [ ] **Strong parsing**
  - [ ] Double asterisk creates strong: `**text**`
  - [ ] Double underscore creates strong: `__text__`
  - [ ] Can nest emphasis in strong: `**a *b* c**`
  - [ ] Can nest strong in emphasis: `*a **b** c*`

- [ ] **Links parsing**
  - [ ] Resource links work: `[text](url)`
  - [ ] Links with titles work: `[text](url "title")`
  - [ ] Full references work: `[text][ref]`
  - [ ] Collapsed references work: `[text][]`
  - [ ] Shortcut references work: `[text]`
  - [ ] Undefined references fail gracefully
  - [ ] Nested links are prevented: `[a [b](c) d](e)`

- [ ] **Images parsing**
  - [ ] Resource images work: `![alt](url)`
  - [ ] Images with titles work: `![alt](url "title")`
  - [ ] Reference images work: `![alt][ref]`
  - [ ] Images can nest in links: `[![img](i.png)](url)`

### Technical Requirements

- [ ] **Resolution**
  - [ ] Attention resolver runs on all attention sequences
  - [ ] Label resolver runs on all label starts/ends
  - [ ] Unmatched delimiters become data
  - [ ] Event stream properly nested (enter/exit paired)

- [ ] **Performance**
  - [ ] No infinite loops on any input
  - [ ] Handles 10KB of inline content in <100ms
  - [ ] Memory usage reasonable (no major leaks)

- [ ] **Integration**
  - [ ] All constructs added to text dispatcher
  - [ ] Constructs work together (e.g., emphasis in links)
  - [ ] Proper event generation for HTML output

### Test Requirements

- [ ] **Unit tests**
  - [ ] All modules have unit tests
  - [ ] Edge cases covered
  - [ ] Resolution algorithms tested

- [ ] **CommonMark compliance**
  - [ ] Examples 350-489 (emphasis/strong) pass
  - [ ] Examples 481-549 (links) pass
  - [ ] Examples 550-567 (images) pass
  - [ ] At least 90% of relevant examples pass

- [ ] **Code quality**
  - [ ] No compiler warnings
  - [ ] No binary optimization warnings
  - [ ] Dialyzer passes
  - [ ] All tail recursion verified

---

## Implementation Checklist

### Phase 7.1: Setup (Day 1-2)

- [ ] Extend tokenizer record with Phase 7 fields
- [ ] Add label_start, label, definitions types to types.hrl
- [ ] Implement tokenizer accessor functions
- [ ] Create test helpers for inline content
- [ ] Set up CommonMark inline test runner

### Phase 7.2: Label Starts (Day 3-4)

- [ ] Implement erlmd_cnstr_label_start_link
- [ ] Implement erlmd_cnstr_label_start_image
- [ ] Add to text dispatcher (test with simple `[text]`)
- [ ] Write unit tests
- [ ] Verify events generated correctly

### Phase 7.3: Partial Constructs (Day 5)

- [ ] Implement erlmd_cnstr_prtl_label
  - [ ] Maximum size checking
  - [ ] No blank lines
  - [ ] Escape handling
- [ ] Implement erlmd_cnstr_prtl_destination
  - [ ] Enclosed form `<url>`
  - [ ] Raw form with balanced parens
- [ ] Implement erlmd_cnstr_prtl_title
  - [ ] Double quotes
  - [ ] Single quotes
  - [ ] Parentheses
- [ ] Test each in isolation

### Phase 7.4: Label End - Resources (Day 6-7)

- [ ] Implement basic label end matching
- [ ] Implement resource parsing `](url)`
- [ ] Implement resource with title `](url "title")`
- [ ] Test simple resource links
- [ ] Test resource images

### Phase 7.5: Label End - References (Day 8)

- [ ] Add definition support to parser state
- [ ] Implement full reference matching `][ref]`
- [ ] Implement collapsed reference `][]`
- [ ] Implement shortcut reference `]`
- [ ] Test all reference forms
- [ ] Test undefined references

### Phase 7.6: Label End - Resolution (Day 8)

- [ ] Implement label resolver
- [ ] Inject link/image events
- [ ] Mark unmatched starts as data
- [ ] Test nested links prevention
- [ ] Test images in links

### Phase 7.7: Attention - Marking (Day 9)

- [ ] Implement attention construct
- [ ] Count consecutive markers
- [ ] Store attention sequences
- [ ] Register resolver
- [ ] Test simple emphasis marking

### Phase 7.8: Attention - Resolution (Day 10)

- [ ] Implement get_sequences
  - [ ] Extract all attention sequences
  - [ ] Analyze before/after characters
  - [ ] Determine can_open/can_close
  - [ ] Track stack depth
- [ ] Implement matching algorithm
  - [ ] Find closers
  - [ ] Walk back to find openers
  - [ ] Check rule of 3
  - [ ] Match pairs
- [ ] Implement event creation
  - [ ] Take markers from sequences
  - [ ] Create Emphasis/Strong events
  - [ ] Update sequence sizes
- [ ] Test emphasis
- [ ] Test strong
- [ ] Test nested
- [ ] Test rule of 3

### Phase 7.9: Integration (Day 11)

- [ ] Add all constructs to text dispatcher
- [ ] Test constructs work together
- [ ] Test emphasis in links
- [ ] Test links in emphasis (nested links fail)
- [ ] Run CommonMark tests
- [ ] Debug failures

### Phase 7.10: Polish & Documentation (Day 12-14)

- [ ] Optimize hot paths (character classification, event manipulation)
- [ ] Add comprehensive edge case tests
- [ ] Document resolution algorithms
- [ ] Add code comments
- [ ] Write usage examples
- [ ] Performance profiling
- [ ] Final CommonMark test run
- [ ] Code review

---

## Additional Resources

### Rust Reference Code Mapping

| Erlang Module | Rust File |
|---------------|-----------|
| erlmd_cnstr_attention | construct/attention.rs |
| erlmd_cnstr_label_start_link | construct/label_start_link.rs |
| erlmd_cnstr_label_start_image | construct/label_start_image.rs |
| erlmd_cnstr_label_end | construct/label_end.rs |
| erlmd_cnstr_prtl_label | construct/partial_label.rs |
| erlmd_cnstr_prtl_destination | construct/partial_destination.rs |
| erlmd_cnstr_prtl_title | construct/partial_title.rs |
| erlmd_resolve (attention & label) | resolve.rs |

### CommonMark Spec Sections

- [§6.2 Emphasis and strong emphasis](https://spec.commonmark.org/0.31/#emphasis-and-strong-emphasis)
- [§6.3 Links](https://spec.commonmark.org/0.31/#links)
- [§6.4 Images](https://spec.commonmark.org/0.31/#images)

### Key Algorithms

1. **Attention matching** - CommonMark spec §6.2, markdown-rs attention.rs lines 140-350
2. **Label end matching** - CommonMark spec §6.3-6.4, markdown-rs label_end.rs lines 100-600
3. **Delimiter run classification** - markdown-rs attention.rs lines 200-250

---

## Success Metrics

After Phase 7 completion:

- **CommonMark tests**: 450+ passing (from ~250 after Phase 6)
- **Coverage**: All §6.2, §6.3, §6.4 examples pass
- **Performance**: Can parse 10KB of inline markdown in <100ms
- **Code quality**: No warnings, dialyzer clean
- **Documentation**: All algorithms documented

---

## Notes for Claude Code

### When Implementing

1. **Start with tests** - Write failing tests first, then implement
2. **Reference Rust often** - The algorithms are complex, follow the reference
3. **Debug with events** - Print event streams to understand what's happening
4. **Test incrementally** - Don't wait until everything is done
5. **Use attempt wisely** - Backtracking is key for label ends

### Common Issues

1. **Infinite loops** - Usually missing EOF check in state function
2. **Stack overflow** - Check tail recursion, especially in resolution
3. **Wrong events** - Verify enter/exit pairing
4. **Failed matches** - Debug by printing sequences and their can_open/close flags
5. **Memory issues** - Make sure to clear temporary state

### Debug Commands

```erlang
%% Print current tokenizer state
io:format("Pos: ~p, Char: ~p~n", [T#tokenizer.index, erlmd_tokenizer:current(T)]).

%% Print all events
Events = erlmd_tokenizer:events(T),
[io:format("~p: ~p ~p~n", [I, E#event.kind, E#event.name]) || 
    {I, E} <- lists:enumerate(Events)].

%% Print attention sequences
Seqs = get_sequences(T),
[io:format("Seq ~p: marker=~p size=~p open=~p close=~p~n",
    [I, S#attention_sequence.marker, S#attention_sequence.size,
     S#attention_sequence.open, S#attention_sequence.close]) ||
    {I, S} <- lists:enumerate(Seqs)].
```

---

**Good luck! Phase 7 is the most complex phase, but it's also the most rewarding. The attention and label matching algorithms are elegant once you understand them. Take your time, test thoroughly, and reference the Rust code when stuck.**
