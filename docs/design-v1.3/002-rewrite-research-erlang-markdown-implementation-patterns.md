# Erlang Implementation Patterns for High-Performance Markdown Parsing

A comprehensive guide to implementing a production-quality Markdown parser in Erlang, focusing on the specific patterns, optimizations, and architectural decisions that leverage Erlang's strengths while achieving performance competitive with compiled languages.

## Table of Contents

1. [Why Erlang Can Compete with Rust for Parsing](#why-erlang-can-compete-with-rust-for-parsing)
2. [Binary Pattern Matching: The Foundation](#binary-pattern-matching-the-foundation)
3. [State Machine Implementation Without gen_statem](#state-machine-implementation-without-gen_statem)
4. [Data Structure Choices: Records vs Maps](#data-structure-choices-records-vs-maps)
5. [Performance Optimization Patterns](#performance-optimization-patterns)
6. [Memory Management and Allocation](#memory-management-and-allocation)
7. [The Event Stream Architecture](#the-event-stream-architecture)
8. [Module Organization and Code Structure](#module-organization-and-code-structure)
9. [Configuration and Variant Support](#configuration-and-variant-support)
10. [Testing Infrastructure](#testing-infrastructure)
11. [Common Pitfalls and Anti-Patterns](#common-pitfalls-and-anti-patterns)

---

## Why Erlang Can Compete with Rust for Parsing

### The Modern Erlang Advantage

Erlang OTP 26-28 introduced transformative improvements that make it surprisingly competitive for parsing workloads:

**OTP 26 Binary Construction Optimizations:**
- The JIT now emits single-instruction code for binary construction
- Binary contents are constructed in CPU registers before writing to memory
- Type-based optimizations use BEAM file type information for better native code
- Result: 2-3x performance improvement for binary-heavy operations

**OTP 27 Binary/Bitstring Rewrite:**
- Fundamental improvements to binary handling at the VM level
- Better memory layout and cache locality for binary operations
- Enhanced match context optimization across function boundaries

**Real-World Performance Expectations:**
- Well-written Erlang binary parsers: **100-150 MB/s** on commodity hardware
- Rust parsers (like markdown-rs): **200-300 MB/s** on the same hardware
- The 2-3x gap is acceptable for most use cases and far better than the 10-20x gap of older Erlang versions

### When to Choose Erlang for a Parser

**Choose Erlang when:**
- You need to parse documents as part of a larger Erlang/OTP system
- Concurrent document processing is important (Erlang's process model shines)
- You value maintainability and hot code reloading over raw speed
- 100 MB/s is "fast enough" (processes typical web documents in milliseconds)
- You want strong error isolation (parser crashes don't kill the VM)

**Consider Rust/Go when:**
- Parsing is the primary workload (not part of a larger system)
- You need absolute maximum throughput (500+ MB/s)
- You're building a command-line tool with minimal dependencies
- Sub-millisecond latency is critical for your use case

---

## Binary Pattern Matching: The Foundation

### The Match Context Optimization

The single most important pattern for Erlang parser performance is **match context preservation**. When you pattern match on a binary in a function head and pass the remaining binary as a tail call, the compiler avoids creating a sub-binary and maintains a "match context" - essentially a pointer into the original binary.

**Optimal Pattern (Match Context Preserved):**

```erlang
%% Match in function head, tail-recursive call
parse(<<$#, Rest/binary>>, State) ->
    parse_heading(Rest, State);
parse(<<$*, Rest/binary>>, State) ->
    parse_emphasis(Rest, State);
parse(<<Char, Rest/binary>>, State) ->
    parse_char(Rest, State#state{buffer = [Char | State#state.buffer]});
parse(<<>>, State) ->
    finalize(State).
```

**Suboptimal Pattern (Creates Sub-binaries):**

```erlang
%% Matching inside function body - creates sub-binary!
parse(Binary, State) ->
    case Binary of
        <<$#, Rest/binary>> -> parse_heading(Rest, State);
        <<$*, Rest/binary>> -> parse_emphasis(Rest, State);
        _ -> parse(Binary, State)
    end.
```

The difference: In the first version, `Rest` is a match context (lightweight pointer). In the second, `Rest` is a new sub-binary (heap allocation and copy). For a 1MB document, this difference means 1 million allocations vs. 0 allocations.

### Enabling Binary Optimization Info

Always compile with binary optimization information during development:

```erlang
-compile([bin_opt_info]).
```

Or in `rebar.config`:

```erlang
{erl_opts, [debug_info, bin_opt_info]}.
```

The compiler will emit warnings like:

```
src/markdown_parser.erl:42: Warning: NOT OPTIMIZED: the binary matching compiler generated code that will produce a sub-binary
```

### Multi-Byte Pattern Matching

Erlang excels at matching multiple bytes simultaneously:

```erlang
%% Efficient: matches entire sequence in one operation
parse_code_fence(<<"```", Rest/binary>>, State) ->
    {fence, Rest, State};

%% Also efficient: matches newline patterns
parse_line_ending(<<"\r\n", Rest/binary>>, State) ->
    {line_end, Rest, State#state{line = State#state.line + 1}};
parse_line_ending(<<"\n", Rest/binary>>, State) ->
    {line_end, Rest, State#state{line = State#state.line + 1}};
parse_line_ending(<<"\r", Rest/binary>>, State) ->
    {line_end, Rest, State#state{line = State#state.line + 1}}.
```

### Binary Comprehensions for Transformation

For character-level transformations (like normalizing tabs), binary comprehensions are both elegant and efficient:

```erlang
%% Normalize tabs to spaces (4-space tab stops)
normalize_tabs(Binary) ->
    normalize_tabs(Binary, 0, <<>>).

normalize_tabs(<<$\t, Rest/binary>>, Column, Acc) ->
    SpacesNeeded = 4 - (Column rem 4),
    Spaces = binary:copy(<<$ >>, SpacesNeeded),
    normalize_tabs(Rest, Column + SpacesNeeded, <<Acc/binary, Spaces/binary>>);
normalize_tabs(<<$\n, Rest/binary>>, _Column, Acc) ->
    normalize_tabs(Rest, 0, <<Acc/binary, $\n>>);
normalize_tabs(<<Char, Rest/binary>>, Column, Acc) ->
    normalize_tabs(Rest, Column + 1, <<Acc/binary, Char>>);
normalize_tabs(<<>>, _Column, Acc) ->
    Acc.
```

### Avoiding Binary Concatenation

**Never do this in a loop:**

```erlang
%% BAD: Copies the entire Acc binary on every iteration
collect_chars(<<Char, Rest/binary>>, Acc) ->
    collect_chars(Rest, <<Acc/binary, Char>>).
```

**Do this instead:**

```erlang
%% GOOD: Build a list, convert once at the end
collect_chars(<<Char, Rest/binary>>, Acc) ->
    collect_chars(Rest, [Char | Acc]);
collect_chars(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc)).
```

Or even better, use iolists (covered later).

---

## State Machine Implementation Without gen_statem

### Why Hand-Rolled for the Parser Core

While `gen_statem` is excellent for protocol handlers and stateful services, it's **overkill and slower** for character-by-character parsing:

**gen_statem overhead:**
- Message passing between behavior and callback module
- Generic event handling machinery
- State data copying on transitions
- ~10-20% performance penalty for tight loops

**Hand-rolled advantages:**
- Direct function calls (no message passing)
- Compiler can inline small state functions
- Zero overhead state transitions
- Full control over tail recursion

### The Core Parser Loop Pattern

```erlang
-module(markdown_parser).
-export([parse/1]).

-record(state, {
    input :: binary(),
    position = 0 :: non_neg_integer(),
    line = 1 :: pos_integer(),
    column = 1 :: pos_integer(),
    events = [] :: list(),
    stack = [] :: list(),
    mode :: atom()
}).

%% Public API
parse(Binary) when is_binary(Binary) ->
    State = #state{input = Binary, mode = document},
    case parse_loop(Binary, State) of
        {ok, FinalState} ->
            {ok, lists:reverse(FinalState#state.events)};
        {error, Reason} ->
            {error, Reason}
    end.

%% Main parsing loop - delegates to mode-specific parsers
parse_loop(<<>>, State) ->
    {ok, State};
parse_loop(Binary, #state{mode = document} = State) ->
    parse_document(Binary, State);
parse_loop(Binary, #state{mode = paragraph} = State) ->
    parse_paragraph(Binary, State);
parse_loop(Binary, #state{mode = inline} = State) ->
    parse_inline(Binary, State).

%% Document-level parsing
parse_document(<<$#, Rest/binary>>, State) ->
    parse_heading(Rest, State);
parse_document(<<"```", Rest/binary>>, State) ->
    parse_code_fence(Rest, State);
parse_document(<<$>, Rest/binary>>, State) ->
    parse_blockquote(Rest, State);
parse_document(<<Char, Rest/binary>>, State) when Char =:= $\n; Char =:= $\r ->
    parse_document(Rest, advance_line(State));
parse_document(Binary, State) ->
    %% Start paragraph
    NewState = emit_event({enter, paragraph}, State),
    parse_paragraph(Binary, NewState#state{mode = paragraph}).
```

### State Transitions with Events

The key insight from markdown-rs is that **events are your output, state transitions are your mechanism**:

```erlang
%% Emit an event and update state
emit_event(Event, State) ->
    State#state{
        events = [Event | State#state.events]
    }.

%% Common pattern: enter construct, parse content, exit construct
parse_emphasis(Binary, State) ->
    State1 = emit_event({enter, emphasis, position(State)}, State),
    State2 = emit_event({enter, emphasis_marker, position(State1)}, State1),
    
    %% Skip the opening * or _
    <<_Marker, Rest/binary>> = Binary,
    State3 = emit_event({exit, emphasis_marker, position(State2)}, advance_column(State2)),
    
    %% Parse until we find the closing marker
    {InnerBinary, CloseBinary} = find_emphasis_close(Rest),
    State4 = parse_inline(InnerBinary, State3),
    
    %% Emit closing marker
    State5 = emit_event({enter, emphasis_marker, position(State4)}, State4),
    <<_Close, AfterEmphasis/binary>> = CloseBinary,
    State6 = emit_event({exit, emphasis_marker, position(State5)}, advance_column(State5)),
    
    State7 = emit_event({exit, emphasis, position(State6)}, State6),
    
    %% Continue parsing
    parse_inline(AfterEmphasis, State7).
```

### Position Tracking

Accurate position tracking is critical for error messages and source maps:

```erlang
%% Extract current position
position(#state{line = Line, column = Col, position = Pos}) ->
    #{line => Line, column => Col, offset => Pos}.

%% Advance position after consuming a character
advance_column(State) ->
    State#state{
        position = State#state.position + 1,
        column = State#state.column + 1
    }.

advance_line(State) ->
    State#state{
        position = State#state.position + 1,
        line = State#state.line + 1,
        column = 1
    }.

%% Advance by N bytes (for multi-byte matches)
advance_by(N, State) ->
    State#state{
        position = State#state.position + N,
        column = State#state.column + N
    }.
```

### Mode-Based Parsing

Different contexts require different parsing rules. Use mode switching:

```erlang
%% Switch from document mode to inline mode when entering a paragraph
enter_paragraph(State) ->
    State#state{mode = inline}.

%% Switch back when exiting
exit_paragraph(State) ->
    State#state{mode = document}.

%% Inline mode has different character interpretation
parse_inline(<<$*, Rest/binary>>, #state{mode = inline} = State) ->
    %% In inline mode, * starts emphasis
    parse_emphasis(Rest, State);
parse_inline(<<$*, Rest/binary>>, #state{mode = document} = State) ->
    %% In document mode, * at start of line is a list item
    parse_list_item(Rest, State).
```

---

## Data Structure Choices: Records vs Maps

### The Hybrid Approach

The optimal strategy uses **records internally for speed, maps externally for flexibility**:

```erlang
%% Internal AST representation (fast field access)
-record(ast_node, {
    type :: atom(),
    children = [] :: list(),
    position :: #{line := pos_integer(), 
                  column := pos_integer(), 
                  offset := non_neg_integer()},
    meta = #{} :: map()
}).

%% Public API returns maps (flexible, JSON-compatible)
to_mdast(#ast_node{type = Type, children = Children, position = Pos, meta = Meta}) ->
    #{
        type => Type,
        children => [to_mdast(C) || C <- Children],
        position => Pos
    } ++ Meta.  %% Merge meta fields into top level
```

### Why Records Are Faster

Records are compile-time tuples with named field access:

```erlang
%% Record definition
-record(node, {type, value, pos}).

%% Compiles to tuple operations:
Node = #node{type = text, value = "hello"},
%% Becomes: {node, text, "hello", undefined}

Type = Node#node.type,
%% Becomes: element(2, Node)  -- O(1) array index
```

Performance characteristics:
- Field access: **O(1)** via `element/2` - a single BEAM instruction
- Field update: **O(n)** where n = number of fields (creates new tuple)
- Memory: Minimal - just tuple overhead (1 word for tag)
- Match performance: Excellent - pattern matching is highly optimized

### Why Maps Are Better for APIs

Maps provide flexibility at the boundary:

```erlang
%% Maps are self-describing and extensible
Node = #{
    type => heading,
    depth => 1,
    children => [],
    position => #{line => 5, column => 1}
}.

%% JSON serialization is trivial
json:encode(Node).

%% Adding fields doesn't break existing code
Node#{id => "heading-1"}.
```

### Small Map Optimization (< 32 keys)

Maps under 32 keys use FLATMAP representation:

```erlang
%% FLATMAP: sorted key tuple shared between instances
%% Memory: 5 words overhead + (2 * num_keys) words
Map1 = #{type => text, value => "a"},
Map2 = #{type => text, value => "b"},
%% Map1 and Map2 share the key tuple {type, value}
```

For small maps (typical for AST nodes), this is very efficient. Use maps for:
- Fewer than 32 keys
- Keys known at compile time
- Structures returned to user code

### Record-to-Map Conversion Pattern

```erlang
%% Convert record tree to map tree recursively
-spec to_map(#ast_node{}) -> map().
to_map(#ast_node{type = Type, children = Children, position = Pos, meta = Meta}) ->
    Base = #{
        type => Type,
        position => Pos,
        children => [to_map(C) || C <- Children]
    },
    maps:merge(Base, Meta).

%% Utility for common AST nodes
text_node(Value, Pos) ->
    #ast_node{
        type = text,
        children = [],
        position = Pos,
        meta = #{value => Value}
    }.

emphasis_node(Children, Pos) ->
    #ast_node{
        type = emphasis,
        children = Children,
        position = Pos
    }.
```

### When to Use Each

**Use records for:**
- Internal parser state (`#state{}`)
- AST nodes during construction
- Tokenizer context
- Performance-critical data accessed frequently
- Structures with fixed fields

**Use maps for:**
- Configuration options
- Public API return values
- JSON-serializable data
- Extensible structures
- Event payloads

**Use proplists for:**
- Option arguments (backwards compatibility)
- Small, infrequently accessed data

---

## Performance Optimization Patterns

### 1. IOLists for Accumulation

IOLists are the secret weapon for building output without copying:

```erlang
%% BAD: Binary concatenation (quadratic complexity)
build_html(Events) ->
    lists:foldl(fun(Event, Acc) ->
        Html = event_to_html(Event),
        <<Acc/binary, Html/binary>>  %% Copies entire Acc every time!
    end, <<>>, Events).

%% GOOD: IOList accumulation (linear complexity)
build_html(Events) ->
    IOList = lists:map(fun event_to_html/1, Events),
    iolist_to_binary(IOList).

%% BETTER: Fold with iolist building
build_html(Events) ->
    IOList = lists:foldl(fun(Event, Acc) ->
        [event_to_html(Event) | Acc]  %% Just cons, no copying
    end, [], Events),
    iolist_to_binary(lists:reverse(IOList)).

%% BEST: No reverse needed if order doesn't matter or use foldr
build_html(Events) ->
    IOList = lists:foldr(fun(Event, Acc) ->
        [event_to_html(Event) | Acc]
    end, [], Events),
    iolist_to_binary(IOList).
```

What's an iolist? A deeply nested list of binaries and bytes:

```erlang
IOList = [<<"<p>">>, [<<"Hello">>, <<" ">>, <<"World">>], <<"</p>">>],
Binary = iolist_to_binary(IOList),
%% Binary = <<"<p>Hello World</p>">>
```

The runtime flattens this efficiently in a single pass without intermediate allocations.

### 2. Selective Map Updates

Map updates have two syntaxes with different semantics:

```erlang
%% := requires key to exist (fails if missing)
%% Enables key-sharing optimization
Map2 = Map1#{type := heading}.

%% => adds key if missing, updates if present
%% May prevent key-sharing
Map2 = Map1#{id => "new-id"}.
```

**For performance:** Use `:=` for updates to known keys. This allows the runtime to share key tuples between map instances.

### 3. Avoid Premature List Reversal

Many operations naturally produce reversed lists. Don't reverse until necessary:

```erlang
%% Pattern: collect in reverse, process in reverse, output forward

%% Step 1: Collect (naturally reversed due to cons)
collect_tokens(Binary) ->
    collect_tokens(Binary, []).

collect_tokens(<<Char, Rest/binary>>, Acc) ->
    collect_tokens(Rest, [Char | Acc]);
collect_tokens(<<>>, Acc) ->
    Acc.  %% Still reversed

%% Step 2: Process in reverse (foldr processes right-to-left = reverse)
process_tokens(Tokens) ->
    lists:foldr(fun process_token/2, initial_state(), Tokens).

%% Only reverse if user explicitly needs forward order
tokens_forward(Binary) ->
    lists:reverse(collect_tokens(Binary)).
```

### 4. Pre-allocate ETS Tables for References

Link reference resolution requires O(1) lookup. Use ETS:

```erlang
%% During initialization
init_parser() ->
    RefsTable = ets:new(link_refs, [set, private, {keypos, 1}]),
    #state{refs = RefsTable}.

%% First pass: collect definitions
store_definition(Label, Url, Title, State) ->
    NormalizedLabel = string:lowercase(Label),
    ets:insert(State#state.refs, {NormalizedLabel, Url, Title}),
    State.

%% Second pass: resolve references (O(1) lookup)
resolve_reference(Label, State) ->
    NormalizedLabel = string:lowercase(Label),
    case ets:lookup(State#state.refs, NormalizedLabel) of
        [{_, Url, Title}] -> {ok, Url, Title};
        [] -> {error, undefined_reference}
    end.

%% Cleanup
finalize_parser(State) ->
    ets:delete(State#state.refs),
    State#state{refs = undefined}.
```

### 5. Tail Recursion is Mandatory

Non-tail-recursive functions will overflow the stack on large documents:

```erlang
%% BAD: Non-tail-recursive (stack grows with document size)
parse_all(<<Char, Rest/binary>>) ->
    Node = parse_char(Char),
    [Node | parse_all(Rest)];  %% Builds list after recursive call
parse_all(<<>>) ->
    [].

%% GOOD: Tail-recursive with accumulator
parse_all(Binary) ->
    parse_all(Binary, []).

parse_all(<<Char, Rest/binary>>, Acc) ->
    Node = parse_char(Char),
    parse_all(Rest, [Node | Acc]);  %% Recursive call in tail position
parse_all(<<>>, Acc) ->
    lists:reverse(Acc).
```

The compiler optimizes tail calls to loops - zero stack consumption.

### 6. Measure Before Optimizing

Use `eprof` for function-level profiling:

```erlang
%% In the shell
eprof:start().
eprof:profile(fun() -> markdown:parse(LargeDoc) end).
eprof:analyze(total).
```

Use `fprof` for detailed call graphs:

```erlang
fprof:apply(markdown, parse, [LargeDoc]).
fprof:profile().
fprof:analyse([{dest, "markdown.fprof"}]).
```

Focus optimization on functions that appear in the top 10 by time. Optimizing rare code paths yields no benefit.

---

## Memory Management and Allocation

### Understanding Erlang's Memory Model

Erlang processes have private heaps:
- Each process allocates from its own heap
- No shared mutable state means no garbage collection coordination
- Minor GC runs when process heap fills
- Major GC runs when heap grows significantly

For parsing (single-process):
- Parser state lives on process heap
- Output (events, AST) lives on process heap
- When parsing completes, caller receives result (copies to its heap)
- Original parser process can die, reclaiming all memory instantly

### Avoiding Memory Bloat

**1. Don't accumulate unnecessary data**

```erlang
%% BAD: Keeps entire input in state
parse(Binary, State) ->
    NewState = State#state{original_input = Binary},
    %% ... parsing ...
    
%% GOOD: Only keep what's needed
parse(Binary, State) ->
    %% State doesn't hold Binary
    %% ... parsing ...
```

**2. Stream events to caller instead of buffering**

```erlang
%% Instead of collecting all events in memory:
parse_streaming(Binary, Callback) ->
    parse_stream(Binary, #state{callback = Callback}).

parse_stream(Binary, State) ->
    %% When event occurs, call back immediately
    emit_event_streaming(Event, State),
    %% Event is not stored in State
```

**3. Break large documents into chunks**

```erlang
parse_large_file(Filename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    parse_chunks(File, #state{}).

parse_chunks(File, State) ->
    case file:read(File, 65536) of  %% 64KB chunks
        {ok, Data} ->
            NewState = parse(Data, State),
            parse_chunks(File, NewState);
        eof ->
            file:close(File),
            {ok, State}
    end.
```

### Binary Reference Counting

Binaries larger than 64 bytes are reference-counted and stored off-heap:
- Multiple processes can reference the same binary
- Binary is only freed when all references are dropped
- Sub-binaries hold references to original

This means:
- Passing large binaries between processes is cheap (just increment refcount)
- Keep original binary in scope as long as sub-binaries are needed
- Be aware that small sub-binaries keep entire large binary alive

```erlang
%% If you extract a small piece, consider copying it
extract_small_piece(LargeBinary) ->
    <<Small:10/binary, _Rest/binary>> = LargeBinary,
    binary:copy(Small).  %% Breaks reference to LargeBinary
```

---

## The Event Stream Architecture

### Event Structure

Events are the core abstraction, representing token boundaries:

```erlang
-record(event, {
    kind :: enter | exit,
    name :: atom(),  %% paragraph, heading, emphasis, etc.
    position :: #{line := pos_integer(), 
                  column := pos_integer(), 
                  offset := non_neg_integer()}
}).

%% Example event stream for: *hello*
Events = [
    #event{kind = enter, name = emphasis, position = #{line => 1, column => 1, offset => 0}},
    #event{kind = enter, name = emphasis_sequence, position = #{line => 1, column => 1, offset => 0}},
    #event{kind = exit, name = emphasis_sequence, position = #{line => 1, column => 2, offset => 1}},
    #event{kind = enter, name = emphasis_text, position = #{line => 1, column => 2, offset => 1}},
    #event{kind = exit, name = emphasis_text, position = #{line => 1, column => 7, offset => 6}},
    #event{kind = enter, name = emphasis_sequence, position = #{line => 1, column => 7, offset => 6}},
    #event{kind = exit, name = emphasis_sequence, position = #{line => 1, column => 8, offset => 7}},
    #event{kind = exit, name = emphasis, position = #{line => 1, column => 8, offset => 7}}
].
```

### Building AST from Events

Stack-based conversion matches Enter/Exit pairs:

```erlang
events_to_ast(Events) ->
    events_to_ast(Events, [], []).

events_to_ast([#event{kind = enter, name = Name, position = Pos} | Rest], Stack, Nodes) ->
    %% Push new node onto stack
    Node = #ast_node{type = Name, position = Pos, children = []},
    events_to_ast(Rest, [Node | Stack], Nodes);

events_to_ast([#event{kind = exit, name = Name} | Rest], [Current | Stack], Nodes) ->
    true = Current#ast_node.type =:= Name,  %% Assert matching
    case Stack of
        [] ->
            %% Top-level node completed
            events_to_ast(Rest, [], [Current | Nodes]);
        [Parent | RestStack] ->
            %% Add current as child of parent
            UpdatedParent = Parent#ast_node{
                children = [Current | Parent#ast_node.children]
            },
            events_to_ast(Rest, [UpdatedParent | RestStack], Nodes)
    end;

events_to_ast([], [], Nodes) ->
    #ast_node{type = root, children = lists:reverse(Nodes)}.
```

### Compiling Events to HTML

Direct traversal without intermediate AST:

```erlang
events_to_html(Events) ->
    iolist_to_binary(compile_events(Events, [])).

compile_events([#event{kind = enter, name = Name} | Rest], Acc) ->
    OpenTag = open_tag(Name),
    compile_events(Rest, [OpenTag | Acc]);

compile_events([#event{kind = exit, name = Name} | Rest], Acc) ->
    CloseTag = close_tag(Name),
    compile_events(Rest, [CloseTag | Acc]);

compile_events([], Acc) ->
    lists:reverse(Acc).

open_tag(paragraph) -> <<"<p>">>;
open_tag(heading) -> <<"<h1>">>;  %% Simplified - real version uses depth
open_tag(emphasis) -> <<"<em>">>;
open_tag(strong) -> <<"<strong>">>;
open_tag(_) -> <<>>.

close_tag(paragraph) -> <<"</p>">>;
close_tag(heading) -> <<"</h1>">>;
close_tag(emphasis) -> <<"</em>">>;
close_tag(strong) -> <<"</strong>">>;
close_tag(_) -> <<>>.
```

---

## Module Organization and Code Structure

### Recommended Directory Structure

```
erlmd/
├── src/
│   ├── erlmd.app.src
│   ├── erlmd.erl                 % Public API
│   ├── erlmd_parser.erl          % Main parser orchestration
│   ├── erlmd_tokenizer.erl       % Core state machine
│   ├── erlmd_ast.erl             % AST utilities
│   ├── erlmd_event.erl           % Event generation/handling
│   │
│   │   % Individual construct parsers
│   ├── erlmd_cnstr_heading.erl
│   ├── erlmd_cnstr_list.erl
│   ├── erlmd_cnstr_emphasis.erl
│   ├── erlmd_cnstr_code_block.erl
│   ├── ...
│   │
│   │   % Variant-specific modules
│   ├── erlmd_vnt_commonmark.erl
│   ├── erlmd_vnt_gfm.erl
│   ├── erlmd_vnt_pandoc.erl
│   │
│   │   % GFM/Pandoc extensions
│   ├── erlmd_ext_tables.erl
│   ├── erlmd_ext_strikethrough.erl
│   ├── erlmd_ext_footnotes.erl
│   ├── ...
│   │
│   │   % Output formats
│   ├── erlmd_out_html.erl
│   ├── erlmd_out_text.erl
│   ├── erlmd_out_manpage.erl
│   └── erlmd_out_ast.erl
│
├── test/
│   ├── erlmd_SUITE.erl           % Common Test suite
│   ├── spec_tests/               % CommonMark spec tests
│   ├── prop_markdown.erl         % PropEr properties
│   └── fixtures/                 % Test documents
│
├── priv/
│   └── spec_tests.json           % Parsed spec tests
│
└── rebar.config
```

### Public API Module

```erlang
-module(erlmd).
-export([
    parse/1, parse/2,
    to_html/1, to_html/2,
    to_ast/1, to_ast/2
]).

-type option() :: {variant, commonmark | gfm | pandoc}
                | {extensions, [atom()]}
                | {features, #{atom() => boolean()}}.

-type options() :: [option()].

%% Parse markdown to events
-spec parse(binary()) -> {ok, [event()]} | {error, term()}.
parse(Markdown) ->
    parse(Markdown, []).

-spec parse(binary(), options()) -> {ok, [event()]} | {error, term()}.
parse(Markdown, Opts) ->
    erlmd_parser:parse(Markdown, normalize_options(Opts)).

%% Parse and convert to HTML
-spec to_html(binary()) -> {ok, binary()} | {error, term()}.
to_html(Markdown) ->
    to_html(Markdown, []).

-spec to_html(binary(), options()) -> {ok, binary()} | {error, term()}.
to_html(Markdown, Opts) ->
    case parse(Markdown, Opts) of
        {ok, Events} ->
            Html = erlmd_out_html:render(Events),
            {ok, Html};
        {error, _} = Error ->
            Error
    end.

%% Parse and convert to AST
-spec to_ast(binary()) -> {ok, map()} | {error, term()}.
to_ast(Markdown) ->
    to_ast(Markdown, []).

-spec to_ast(binary(), options()) -> {ok, map()} | {error, term()}.
to_ast(Markdown, Opts) ->
    case parse(Markdown, Opts) of
        {ok, Events} ->
            Ast = erlmd_ast:from_events(Events),
            {ok, Ast};
        {error, _} = Error ->
            Error
    end.

normalize_options(Opts) ->
    Defaults = #{
        variant => commonmark,
        extensions => [],
        features => #{}
    },
    lists:foldl(fun normalize_option/2, Defaults, Opts).

normalize_option({variant, V}, Acc) -> Acc#{variant => V};
normalize_option({extensions, E}, Acc) -> Acc#{extensions => E};
normalize_option({features, F}, Acc) -> Acc#{features => F}.
```

### Construct Module Pattern

Each construct is a separate module with a standard interface:

```erlang
-module(erlmd_cnstr_heading).
-export([attempt/2]).

-include("erlmd_internal.hrl").

%% Attempt to parse an ATX heading at current position
-spec attempt(binary(), state()) -> {ok, binary(), state()} | {error, nomatch}.
attempt(<<"#", Rest/binary>>, State) ->
    attempt_heading(Rest, State, 1);
attempt(_, _State) ->
    {error, nomatch}.

attempt_heading(<<"#", Rest/binary>>, State, Level) when Level < 6 ->
    attempt_heading(Rest, State, Level + 1);
attempt_heading(<<C, _/binary>> = Binary, State, Level) when C =:= $  ; C =:= $\t ->
    %% Valid heading - emit events and parse content
    State1 = emit_event({enter, heading, level(Level)}, State),
    State2 = emit_event({enter, heading_sequence}, State1),
    State3 = skip_chars(Level + 1, State2),  %% Skip the #'s and space
    State4 = emit_event({exit, heading_sequence}, State3),
    
    {Content, After} = take_until_eol(Binary),
    State5 = parse_inline(Content, State4),
    State6 = emit_event({exit, heading}, State5),
    
    {ok, After, State6};
attempt_heading(_, _State, _Level) ->
    {error, nomatch}.
```

---

## Configuration and Variant Support

### Variant Behavior

Define a behavior for Markdown variants:

```erlang
-module(erlmd_vnt).

-callback name() -> atom().
-callback constructs() -> #{atom() => module()}.
-callback extensions() -> [atom()].
-callback default_options() -> #{atom() => term()}.
```

### CommonMark Implementation

```erlang
-module(erlmd_vnt_commonmark).
-behaviour(erlmd_vnt).

-export([name/0, constructs/0, extensions/0, default_options/0]).

name() -> commonmark.

constructs() ->
    #{
        heading => erlmd_cnstr_heading,
        thematic_break => erlmd_cnstr_thematic_break,
        code_block => erlmd_cnstr_code_block,
        block_quote => erlmd_cnstr_blockquote,
        list => erlmd_cnstr_list,
        paragraph => erlmd_cnstr_paragraph,
        
        emphasis => erlmd_cnstr_emphasis,
        link => erlmd_cnstr_link,
        image => erlmd_cnstr_image,
        code_span => erlmd_cnstr_code_span,
        hard_break => erlmd_cnstr_hard_break
    }.

extensions() ->
    [].  %% No extensions for base CommonMark

default_options() ->
    #{
        allow_html => true,
        smart_punctuation => false
    }.
```

### GFM Implementation

```erlang
-module(erlmd_vnt_gfm).
-behaviour(erlmd_variant).

-export([name/0, constructs/0, extensions/0, default_options/0]).

name() -> gfm.

constructs() ->
    %% Start with CommonMark constructs
    Base = erlmd_vnt_commonmark:constructs(),
    
    %% Add GFM extensions
    Base#{
        table => erlmd_ext_tables,
        strikethrough => erlmd_ext_strikethrough,
        task_list => erlmd_ext_task_lists,
        autolink_extended => erlmd_ext_autolink
    }.

extensions() ->
    [tables, strikethrough, task_lists, autolinks].

default_options() ->
    Base = erlmd_vnt_commonmark:default_options(),
    Base#{
        tagfilter => true  %% GFM-specific option
    }.
```

### Runtime Variant Selection

```erlang
-module(erlmd_parser).

parse(Markdown, Options) ->
    Variant = maps:get(variant, Options, commonmark),
    VariantMod = variant_module(Variant),
    
    Constructs = VariantMod:constructs(),
    State = init_state(Markdown, Constructs, Options),
    
    parse_document(Markdown, State).

variant_module(commonmark) -> erlmd_vnt_commonmark;
variant_module(gfm) -> erlmd_vnt_gfm;
variant_module(pandoc) -> erlmd_vnt_pandoc.

init_state(Binary, Constructs, Options) ->
    #state{
        input = Binary,
        constructs = Constructs,
        options = Options,
        variant = maps:get(variant, Options, commonmark)
    }.
```

---

## Testing Infrastructure

### Common Test Suite Structure

```erlang
-module(erlmd_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([
    basic_parse_test/1,
    heading_test/1,
    emphasis_test/1,
    list_test/1
]).

all() ->
    [
        {group, basic},
        {group, constructs},
        {group, spec_compliance}
    ].

groups() ->
    [
        {basic, [parallel], [basic_parse_test]},
        {constructs, [parallel], [heading_test, emphasis_test, list_test]},
        {spec_compliance, [sequential], spec_test_cases()}
    ].

spec_test_cases() ->
    %% Load CommonMark spec tests dynamically
    {ok, Spec} = file:consult("priv/spec_tests.json"),
    [test_name(T) || T <- Spec].
```

### PropEr Properties

```erlang
-module(prop_markdown).
-include_lib("proper/include/proper.hrl").

%% Property: Parsing never crashes
prop_never_crashes() ->
    ?FORALL(Input, binary(),
        begin
            Result = erlmd:parse(Input),
            is_tuple(Result) andalso (element(1, Result) =:= ok orelse element(1, Result) =:= error)
        end).

%% Property: Roundtrip preservation
prop_roundtrip() ->
    ?FORALL(Ast, ast(),
        begin
            Markdown = erlmd:to_markdown(Ast),
            {ok, Ast2} = erlmd:to_ast(Markdown),
            ast_equivalent(Ast, Ast2)
        end).

%% Generator for valid AST
ast() ->
    ?SIZED(Size, ast(Size)).

ast(0) ->
    %% Base case: leaf nodes
    oneof([
        text_node(),
        code_node(),
        break_node()
    ]);
ast(N) ->
    %% Recursive case: container nodes
    frequency([
        {10, text_node()},
        {5, paragraph([ast(N div 2)])},
        {3, heading(choose(1, 6), [ast(N div 2)])},
        {2, list([ast(N div 2)])}
    ]).

text_node() ->
    #{type => text, value => non_empty(binary())}.

paragraph(Children) ->
    #{type => paragraph, children => Children}.

heading(Level, Children) ->
    #{type => heading, depth => Level, children => Children}.
```

### Running the Full Test Suite

```erlang
%% In rebar.config
{profiles, [
    {test, [
        {deps, [
            {proper, "~> 1.5"}
        ]},
        {erl_opts, [debug_info, export_all]}
    ]}
]}.

%% Run all tests
%% $ rebar3 ct
%% $ rebar3 proper
```

---

## Common Pitfalls and Anti-Patterns

### Pitfall 1: Breaking Match Context

```erlang
%% WRONG: Matching in case breaks match context
parse(Binary, State) ->
    case Binary of
        <<$#, Rest/binary>> -> ...
    end.

%% RIGHT: Match in function head
parse(<<$#, Rest/binary>>, State) -> ...
```

### Pitfall 2: Using `++` in Loops

```erlang
%% WRONG: Quadratic complexity
collect([], Acc) -> Acc;
collect([H|T], Acc) -> collect(T, Acc ++ [H]).

%% RIGHT: Linear complexity
collect([], Acc) -> lists:reverse(Acc);
collect([H|T], Acc) -> collect(T, [H|Acc]).
```

### Pitfall 3: Binary Concatenation in Loops

```erlang
%% WRONG: Copies entire binary every iteration
build([], Acc) -> Acc;
build([H|T], Acc) -> build(T, <<Acc/binary, H/binary>>).

%% RIGHT: Use iolists
build(List) -> iolist_to_binary(List).
```

### Pitfall 4: Forgetting Tail Recursion

```erlang
%% WRONG: Not tail recursive (builds stack)
process([]) -> [];
process([H|T]) -> [transform(H) | process(T)].

%% RIGHT: Tail recursive with accumulator
process(List) -> process(List, []).
process([], Acc) -> lists:reverse(Acc);
process([H|T], Acc) -> process(T, [transform(H)|Acc]).
```

### Pitfall 5: Using Maps Like Records

```erlang
%% WRONG: Using maps for hot-path internal state
parse(Binary, #{events := Events, pos := Pos} = State) ->
    ...

%% RIGHT: Use records for internal state
parse(Binary, #state{events = Events, pos = Pos} = State) ->
    ...
```

### Pitfall 6: Premature gen_statem

```erlang
%% WRONG: Using gen_statem for tight parsing loop
-module(markdown_parser).
-behaviour(gen_statem).

%% RIGHT: Hand-rolled state machine for hot path
-module(markdown_parser).
parse(Binary, State) -> ...
```

### Pitfall 7: Creating Too Many Processes

```erlang
%% WRONG: Spawning process per character
parse_parallel(Binary) ->
    [spawn(fun() -> parse_char(C) end) || <<C>> <= Binary].

%% RIGHT: Single-process parsing (Markdown is inherently sequential)
parse(Binary) -> parse_loop(Binary, init_state()).
```

### Pitfall 8: Not Measuring Performance

```erlang
%% WRONG: Optimizing blindly
"I think this map lookup might be slow, let me change it to a record..."

%% RIGHT: Profile first
eprof:start(),
eprof:profile(fun() -> markdown:parse(Doc) end),
eprof:analyze(total),
%% Only optimize functions that show up in top 10
```

---

## Conclusion

Building a high-performance Markdown parser in Erlang requires understanding and leveraging Erlang's unique strengths:

1. **Binary pattern matching** is your primary tool - use it directly in function heads
2. **Match context optimization** is critical - test with `bin_opt_info`
3. **Records for internal state, maps for APIs** - hybrid approach wins
4. **IOLists everywhere** - never concatenate binaries in loops
5. **Hand-rolled state machines** - gen_statem is overkill for parsing
6. **Tail recursion is mandatory** - stack overflows on large documents are unacceptable
7. **Measure, don't guess** - use eprof and fprof to find actual bottlenecks

Modern Erlang OTP 26-28 has closed the performance gap significantly. Well-written Erlang parsers can achieve 100-150 MB/s throughput - sufficient for processing typical web documents in single-digit milliseconds.

The event-based architecture from markdown-rs translates beautifully to Erlang, where events can be efficiently accumulated in lists and processed functionally. The lack of mutable state actually simplifies implementation compared to imperative languages.

Focus on correctness first (pass all 650+ CommonMark tests), then optimize the hot paths that profiling identifies. The patterns in this guide will get you 90% of the way to optimal performance - the final 10% comes from measurement-driven optimization of your specific bottlenecks.

---

## References and Further Reading

### Erlang Performance

- [Erlang Efficiency Guide - Binary Handling](https://www.erlang.org/doc/efficiency_guide/binaryhandling.html)
- "Optimizing Erlang Binary Matching" - rhye.org blog series
- OTP 26 Release Notes - Binary construction improvements

### Markdown Specifications

- [CommonMark Spec 0.31.2](https://spec.commonmark.org/0.31.2/)
- [GitHub Flavored Markdown Spec](https://github.github.com/gfm/)
- [markdown-rs Source Code](https://github.com/wooorm/markdown-rs)

### Testing

- [PropEr Documentation](https://proper-testing.github.io/)
- [Common Test User's Guide](https://www.erlang.org/doc/apps/common_test/users_guide.html)

### Related Projects

- [earmark](https://github.com/pragdave/earmark) - Existing Elixir Markdown parser
- [markdown](https://github.com/erlange/markdown) - Existing Erlang Markdown implementation
- [cmark](https://github.com/commonmark/cmark) - Reference C implementation

---

**Document Version:** 1.0  
**Last Updated:** November 2025  
**Target OTP Versions:** 26, 27, 28
