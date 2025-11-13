# Building a Bulletproof Markdown Parser Test Suite in Erlang: Property-Based Testing, Spec Conformance, and Production-Ready Quality Assurance

**PropEr property-based testing combined with Common Test provides the most robust foundation for parser development, delivering 100% code coverage when paired with spec conformance tests, fuzzing, and differential testing against reference implementations.** The key insight from production parsers like markdown-rs and cmark is that successful testing requires multiple complementary strategies: property-based testing catches logical errors and edge cases through randomized inputs, spec conformance ensures standard compliance with 500+ CommonMark tests, fuzzing discovers crashes and hangs missed by coverage analysis, and golden testing validates complex outputs. This comprehensive approach has enabled parsers like markdown-rs to achieve 100% coverage and cmark to serve as the reference implementation trusted across the ecosystem.

Building a Markdown parser in Erlang demands rigorous testing because parsers sit at trust boundaries—they process untrusted input and must never crash, hang, or produce incorrect output. The testing strategies outlined here draw from production systems including Erlang/OTP itself, RabbitMQ, and successful parsers across multiple languages. The report provides concrete, implementable patterns with real code examples rather than abstract concepts, enabling developers to actually write these tests immediately.

## Table of Contents

1. [Property-Based Testing Foundations with PropEr](#property-based-testing-foundations-with-proper)
2. [Integrating PropEr with Common Test](#integrating-proper-with-common-test)
3. [Markdown-Specific Testing Strategies](#markdown-specific-testing-strategies)
4. [Building Robust Test Infrastructure](#building-robust-test-infrastructure)
5. [Lessons from Reference Implementations](#lessons-from-reference-implementations)
6. [Putting It All Together: A Practical Test Suite Blueprint](#putting-it-all-together-a-practical-test-suite-blueprint)

---

## Property-Based Testing Foundations with PropEr

Property-based testing represents the cornerstone of parser validation, generating thousands of randomized test cases to explore the input space systematically. PropEr integrates seamlessly with Erlang's testing ecosystem and excels at finding edge cases that manual testing misses. The fundamental approach involves defining generators that create valid and invalid Markdown, writing properties that must hold for all inputs, and leveraging PropEr's shrinking to minimize failing cases.

### Generators for Structured Markdown

Creating effective generators requires understanding Markdown's recursive structure. A basic text generator uses frequency distributions to create realistic-looking content with appropriate character ratios. The pattern uses `frequency/1` to weight common characters more heavily than rare ones, producing text that exercises the parser naturally. For Markdown-specific constructs, generators compose smaller pieces into larger structures—headings combine level indicators with inline content, emphasis wraps text in delimiters, and links pair display text with URLs.

The recursive document generator demonstrates the core pattern. Start with a simple base case returning minimal content, then build complexity using `?LAZY` to prevent infinite recursion. The frequency list weights different constructs by their importance—paragraphs appear most often since they're the most common Markdown element, while code blocks appear less frequently. Each recursive call reduces the size parameter, ensuring termination. This approach generates realistic documents with varied structure that thoroughly exercise the parser.

```erlang
-module(erlmd_generators).
-include_lib("proper/include/proper.hrl").
-export([md_document/1, md_heading/0, md_list/0]).

%% Basic text generator with realistic character distribution
simple_text() ->
    ?LET(Text, non_empty(list(frequency([
        {50, range($a, $z)},
        {50, range($A, $Z)},
        {20, $\s},
        {5, oneof([$., $,, $!, $?])},
        {2, oneof([$-, $_, $*, $~])}
    ]))),
    list_to_binary(Text)).

%% Recursive document generator
md_document(N) when N =< 1 ->
    simple_paragraph();
md_document(N) ->
    PerBranch = N div 2,
    frequency([
        {10, ?LAZY(paragraph())},
        {5, ?LAZY(markdown_heading())},
        {3, ?LAZY(markdown_list())},
        {2, ?LAZY(markdown_code_block())}
    ]).

%% Heading generator
md_heading() ->
    ?LET({Level, Text}, 
         {range(1, 6), simple_text()},
         begin
             Hashes = list_to_binary(lists:duplicate(Level, $#)),
             <<Hashes/binary, " ", Text/binary, "\n">>
         end).

%% List generator
md_list() ->
    ?LET({Type, Items},
         {oneof([ordered, unordered]), non_empty(list(simple_text()))},
         format_list(Type, Items)).

format_list(ordered, Items) ->
    Lines = [iolist_to_binary([integer_to_list(N), ". ", Item, "\n"]) 
             || {N, Item} <- lists:zip(lists:seq(1, length(Items)), Items)],
    iolist_to_binary(Lines);
format_list(unordered, Items) ->
    Lines = [<<"- ", Item/binary, "\n">> || Item <- Items],
    iolist_to_binary(Lines).

%% Complex nested list generator
nested_list(0) ->
    simple_text();
nested_list(N) ->
    ?LET({Items, HasNested},
         {non_empty(list(simple_text())), boolean()},
         case HasNested of
             true ->
                 ?LET(NestedPos, range(1, length(Items)),
                      insert_nested(Items, NestedPos, nested_list(N-1)));
             false ->
                 format_list(unoreof([ordered, unordered]), Items)
         end).
```

For complex nested structures like lists, the generator must maintain structural validity. A list generator creates both ordered and unordered variants, generates a non-empty list of items, then formats them with appropriate markers. The key technique is **generating valid by construction** rather than generating arbitrary strings and filtering. This approach runs faster because every generated value passes validity checks, and PropEr doesn't waste time generating invalid inputs that get rejected.

### Writing Properties That Catch Parser Bugs

The roundtrip property forms the foundation of parser testing. Parse input to an AST, serialize that AST back to Markdown, parse again—the two ASTs must match. This property catches rendering bugs, parser inconsistencies, and data loss issues. A more sophisticated version uses normalization, checking that after one round-trip the output stabilizes. The second parse of the rendered output must produce identical output when rendered again, proving the parser has reached a normalized form.

```erlang
-module(parser_properties).
-include_lib("proper/include/proper.hrl").

%% Basic roundtrip property
prop_roundtrip() ->
    ?FORALL(Markdown, markdown_generators:markdown_document(3),
            begin
                case markdown_parser:parse(Markdown) of
                    {ok, AST} ->
                        Rendered = markdown_renderer:render(AST),
                        case markdown_parser:parse(Rendered) of
                            {ok, AST2} ->
                                ast_equal(AST, AST2);
                            {error, _} ->
                                false
                        end;
                    {error, _} ->
                        %% Parser rejected input, that's OK
                        true
                end
            end).

%% Normalized roundtrip property
prop_roundtrip_normalized() ->
    ?FORALL(Markdown, markdown_generators:markdown_document(3),
            begin
                case markdown_parser:parse(Markdown) of
                    {ok, AST} ->
                        Rendered1 = markdown_renderer:render(AST),
                        {ok, AST2} = markdown_parser:parse(Rendered1),
                        Rendered2 = markdown_renderer:render(AST2),
                        Rendered1 =:= Rendered2;
                    {error, _} ->
                        true
                end
            end).

%% Structural invariants property
prop_ast_well_formed() ->
    ?FORALL(Markdown, markdown_generators:markdown_document(4),
            case markdown_parser:parse(Markdown) of
                {ok, AST} ->
                    check_ast_invariants(AST);
                {error, _} ->
                    true
            end).

check_ast_invariants({document, Children}) ->
    lists:all(fun is_block_element/1, Children) andalso
    lists:all(fun check_ast_invariants/1, Children);
check_ast_invariants({heading, Level, Children}) ->
    Level >= 1 andalso Level =< 6 andalso
    lists:all(fun is_inline_element/1, Children) andalso
    lists:all(fun check_ast_invariants/1, Children);
check_ast_invariants({paragraph, Children}) ->
    lists:all(fun is_inline_element/1, Children) andalso
    lists:all(fun check_ast_invariants/1, Children);
check_ast_invariants({text, Text}) when is_binary(Text) ->
    true;
check_ast_invariants(_) ->
    false.

is_block_element({heading, _, _}) -> true;
is_block_element({paragraph, _}) -> true;
is_block_element({code_block, _, _}) -> true;
is_block_element({list, _, _}) -> true;
is_block_element(_) -> false.

is_inline_element({text, _}) -> true;
is_inline_element({emphasis, _}) -> true;
is_inline_element({strong, _}) -> true;
is_inline_element({code, _}) -> true;
is_inline_element({link, _, _}) -> true;
is_inline_element(_) -> false.
```

Structural invariants verify that the AST maintains correct properties regardless of input. Every heading must have a level between 1 and 6, block elements can only contain other blocks or inline elements appropriately, and the tree structure must be well-formed. These invariants catch bugs where the parser creates malformed trees that might crash downstream code. The verification functions walk the AST recursively, checking types and structure at each level.

Differential testing compares your parser against reference implementations like cmark. For every input, both parsers should produce equivalent output. This requires normalizing HTML output to ignore insignificant differences like whitespace variations. The powerful technique involves calling external reference implementations via ports or NIFs, parsing the same input, and asserting equivalence. **This approach catches subtle semantic bugs that other tests miss**—cases where your parser produces valid but incorrect output.

```erlang
%% Differential testing against cmark
prop_matches_cmark() ->
    ?FORALL(Markdown, markdown_generators:valid_markdown(),
            begin
                MyOutput = my_parser:parse_to_html(Markdown),
                CmarkOutput = call_cmark(Markdown),
                normalize_html(MyOutput) =:= normalize_html(CmarkOutput)
            end).

call_cmark(Markdown) ->
    TempFile = "/tmp/markdown_" ++ integer_to_list(erlang:unique_integer()),
    ok = file:write_file(TempFile, Markdown),
    Result = os:cmd("cmark " ++ TempFile),
    file:delete(TempFile),
    list_to_binary(Result).

normalize_html(Html) ->
    %% Remove insignificant whitespace
    Re1 = re:replace(Html, "\\s+", " ", [global, {return, binary}]),
    %% Trim leading/trailing whitespace
    Re2 = re:replace(Re1, "^\\s+|\\s+$", "", [global, {return, binary}]),
    %% Normalize tag spacing
    re:replace(Re2, "> <", "><", [global, {return, binary}]).
```

The crash-resistance property is critical for production systems. The parser must never crash on any input, no matter how malformed. This property generates completely arbitrary byte sequences and verifies the parser either succeeds or returns an error cleanly. Production parsers face hostile inputs from untrusted sources, making this property essential for security and reliability.

```erlang
%% Crash resistance property
prop_never_crashes() ->
    ?FORALL(Input, binary(),
            begin
                try
                    _ = markdown_parser:parse(Input),
                    true
                catch
                    _:_ -> false
                end
            end).

%% Performance property - no pathological slowdowns
prop_linear_time() ->
    ?FORALL(Size, range(100, 10000),
            begin
                Input = binary:copy(<<"# heading\n">>, Size),
                {Time, _} = timer:tc(markdown_parser, parse, [Input]),
                MicrosecondsPerByte = Time / byte_size(Input),
                %% Should be roughly O(n) - less than 10 microseconds per byte
                MicrosecondsPerByte < 10
            end).
```

### Shrinking Strategies for Complex Markdown

When PropEr finds a failing case, shrinking reduces it to a minimal reproducing example. Default shrinking works well for simple types, but complex Markdown structures need custom shrinking. The `?SHRINK` macro specifies simpler cases to try when a test fails. For Markdown documents, shrink toward the simplest valid structure—a single paragraph, then a single heading, then empty document. This guides PropEr toward minimal failing examples.

```erlang
%% Custom shrinking for Markdown documents
markdown_document_with_shrink(N) ->
    ?SHRINK(markdown_document(N), [
        %% Simpler alternatives to try when shrinking
        <<"">>,  %% Empty document
        <<"paragraph\n">>,  %% Single paragraph
        <<"# heading\n">>,  %% Single heading
        <<"- item\n">>  %% Single list item
    ]).

%% Using LETSHRINK for better control
complex_document() ->
    ?LETSHRINK(Blocks, list(markdown_block()),
               iolist_to_binary(Blocks)).

markdown_block() ->
    oneof([
        markdown_heading(),
        markdown_paragraph(),
        markdown_list(),
        markdown_code_block()
    ]).
```

The `?LETSHRINK` macro provides more control for recursive structures. When generating a document with multiple blocks, `?LETSHRINK` allows PropEr to try each block independently during shrinking. If a document with heading and paragraph fails, PropEr tries just the heading, just the paragraph, then simpler versions of each. This dramatically improves shrinking effectiveness for complex nested structures.

For stateful parser testing, symbolic calls improve shrinking visibility. Instead of generating opaque parser states, generate sequences of operations like "add heading," "add paragraph," "parse buffer." When PropEr shrinks these symbolic sequences, you see exactly which operations cause failures. This proves invaluable for debugging incremental or streaming parsers where state evolution matters.

### Stateful Testing with PropEr statem

The PropEr statem behavior enables testing stateful systems like incremental parsers. Define a state record tracking the parser's logical state, generate commands that modify that state, specify preconditions determining when commands are valid, and write postconditions checking results. The framework executes command sequences, verifying the parser maintains invariants throughout.

```erlang
-module(incremental_parser_statem).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").

-export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).

-record(state, {
    buffer = <<>> :: binary(),
    parsed_blocks = [] :: list()
}).

initial_state() ->
    #state{}.

%% Generate commands based on current state
command(#state{buffer = Buffer}) ->
    HasContent = Buffer =/= <<>>,
    frequency([
        {5, {call, incremental_parser, append_text, [text_fragment()]}},
        {3, {call, incremental_parser, append_newline, []}},
        {2, {call, incremental_parser, parse_buffer, []}} || HasContent
    ] ++ [{2, {call, incremental_parser, parse_buffer, []}} || HasContent]).

text_fragment() ->
    ?SUCHTHAT(Text, non_empty(binary()),
              not binary:match(Text, <<"\n">>) =/= nomatch).

%% Preconditions
precondition(#state{buffer = Buffer}, {call, _, parse_buffer, []}) ->
    Buffer =/= <<>>;
precondition(_State, _Call) ->
    true.

%% Postconditions
postcondition(_State, {call, _, append_text, [_]}, Result) ->
    Result =:= ok;
postcondition(_State, {call, _, append_newline, []}, Result) ->
    Result =:= ok;
postcondition(_State, {call, _, parse_buffer, []}, Result) ->
    case Result of
        {ok, _Blocks} -> true;
        {error, _} -> true;
        _ -> false
    end.

%% State transitions
next_state(State, _Result, {call, _, append_text, [Text]}) ->
    State#state{buffer = <<(State#state.buffer)/binary, Text/binary>>};
next_state(State, _Result, {call, _, append_newline, []}) ->
    State#state{buffer = <<(State#state.buffer)/binary, "\n">>};
next_state(State, Result, {call, _, parse_buffer, []}) ->
    case Result of
        {ok, Blocks} ->
            State#state{
                buffer = <<>>,
                parsed_blocks = State#state.parsed_blocks ++ Blocks
            };
        {error, _} ->
            State
    end.

%% Property to run the state machine
prop_incremental_parser() ->
    ?FORALL(Cmds, commands(?MODULE),
            begin
                incremental_parser:start_link(),
                {History, State, Result} = run_commands(?MODULE, Cmds),
                incremental_parser:stop(),
                
                ?WHENFAIL(
                    io:format("History: ~p\nState: ~p\nResult: ~p\n",
                              [History, State, Result]),
                    aggregate(command_names(Cmds), Result =:= ok)
                )
            end).
```

An incremental parser test models a parser that accepts text fragments and parses when ready. The state tracks buffered text and parsed blocks. Commands include appending text, appending newlines, and parsing the buffer. Preconditions ensure you don't parse an empty buffer. Postconditions verify successful parsing returns valid AST nodes. PropEr generates random command sequences and checks that executing them maintains correctness.

Parallel statem testing catches concurrency bugs. The `parallel_commands/1` function generates two concurrent command sequences and runs them in parallel against the parser. This reveals race conditions, missing locks, and other concurrency issues. For a Markdown parser, this tests whether multiple processes can safely parse concurrently, validating thread-safety for production use.

```erlang
%% Parallel testing property
prop_concurrent_parsing() ->
    ?FORALL(Cmds, parallel_commands(?MODULE),
            begin
                markdown_parser:start_link(),
                {Sequential, Parallel, Result} = run_parallel_commands(?MODULE, Cmds),
                markdown_parser:stop(),
                
                ?WHENFAIL(
                    io:format("Sequential: ~p\nParallel: ~p\nResult: ~p\n",
                              [Sequential, Parallel, Result]),
                    Result =:= ok
                )
            end).
```

---

## Integrating PropEr with Common Test

Common Test provides the standard Erlang testing framework, and integrating PropEr enables property-based testing within CT's structure. The `ct_property_test` module auto-detects PropEr and provides seamless integration. Initialize it in `init_per_suite/1`, then call `ct_property_test:quickcheck/2` from test cases. This combines PropEr's generative power with CT's organizational features, reporting, and tooling.

### Test Suite Organization for Parser Projects

A well-organized test suite separates concerns clearly. Unit tests verify individual parser components like the lexer and tokenizer. Integration tests check the full parsing pipeline. Spec conformance tests validate against CommonMark and GFM specifications. Performance tests catch regressions in speed and memory usage. Property-based tests live in a dedicated subdirectory with generators and properties separated from test suites.

The recommended directory structure mirrors this organization. Place test suites in `test/`, with `_SUITE.erl` files for each major area. Each suite has a corresponding `_SUITE_data/` directory containing fixtures and golden files. Property tests live under `test/property_test/` with separate modules for generators and properties. Spec tests go in `test/spec_tests/` with the CommonMark JSON test file. This structure scales from small parsers to large projects with thousands of tests.

```
erlmd/
├── src/
│   ├── erlmd.app.src
│   ├── erlmd.erl                 % Public API
│   ├── erlmd_parser.erl          % Main parser orchestration
│   ├── erlmd_tokenizer.erl       % Core state machine
│   ├── erlmd_ast.erl             % AST utilities
│   ├── erlmd_event.erl           % Event generation/handling
│   └── ...
├── test/
│   ├── erlmd_lexer_SUITE.erl
│   ├── erlmd_parser_SUITE.erl
│   ├── erlmd_integration_SUITE.erl
│   ├── erlmd_spec_SUITE.erl
│   ├── erlmd_perf_SUITE.erl
│   ├── property_test/
│   │   ├── parser_properties.erl
│   │   └── generators.erl
│   ├── test_data/
│   │   ├── valid/
│   │   └── invalid/
│   └── spec_tests/
│       └── commonmark_spec.json
├── rebar.config
└── .github/
    └── workflows/
        └── ci.yml
```

Group organization within suites enables fine-grained control. Use `{parallel}` groups for independent tests that can run concurrently, dramatically reducing test execution time. Use `{sequence}` groups for tests with dependencies. The `{shuffle}` option randomizes test order, catching hidden dependencies between tests. The `{repeat_until_any_fail, N}` option stress-tests flaky behavior by running tests repeatedly until failure.

```erlang
-module(erlmd_parser_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1]).
-export([test_headings/1, test_lists/1, test_emphasis/1, test_code_blocks/1]).

all() ->
    [{group, basic_parsing}, {group, complex_structures}].

groups() ->
    [
        {basic_parsing, [parallel], [
            test_headings,
            test_lists,
            test_emphasis
        ]},
        {complex_structures, [sequence], [
            test_nested_lists,
            test_code_in_lists,
            test_complex_emphasis
        ]}
    ].

init_per_suite(Config) ->
    %% Initialize PropEr integration
    ct_property_test:init_per_suite(Config).

end_per_suite(Config) ->
    Config.

test_headings(Config) ->
    true = ct_property_test:quickcheck(
        parser_properties:prop_heading_levels(),
        Config
    ).
```

### CT Hooks for Parser-Specific Concerns

Custom CT hooks inject cross-cutting concerns into test execution. A timing hook measures test duration automatically, logging slow tests and building performance history. Implement the hook's lifecycle callbacks: `init/2` initializes state, `pre_init_per_testcase/4` records start time, `post_end_per_testcase/5` calculates duration and logs results, and `terminate/1` writes a final timing report. This provides automatic performance monitoring without modifying individual tests.

```erlang
-module(timing_cth).
-export([init/2, pre_init_per_testcase/4, post_end_per_testcase/5, terminate/1]).

-record(state, {
    timings = [] :: list({atom(), atom(), integer()})
}).

init(_Id, _Opts) ->
    {ok, #state{}}.

pre_init_per_testcase(_Suite, _TC, Config, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    {[{start_time, StartTime} | Config], State}.

post_end_per_testcase(Suite, TC, Config, _Result, State) ->
    StartTime = ?config(start_time, Config),
    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,
    
    %% Log slow tests
    case Duration > 1000 of
        true ->
            ct:pal("Slow test: ~p:~p took ~pms", [Suite, TC, Duration]);
        false ->
            ok
    end,
    
    NewState = State#state{
        timings = [{Suite, TC, Duration} | State#state.timings]
    },
    {ok, NewState}.

terminate(State) ->
    %% Write timing report
    Timings = lists:reverse(State#state.timings),
    File = "test_timings.txt",
    Content = [io_lib:format("~p:~p ~pms\n", [S, TC, D]) 
               || {S, TC, D} <- Timings],
    file:write_file(File, Content),
    ok.
```

An error formatting hook improves debugging for parser failures. When a test fails with a parse error, the hook formats error messages with context—line number, column, surrounding text, and a clear visual indicator of the error location. This transforms cryptic failure messages into actionable debugging information. The hook examines the failure reason in `on_tc_fail/4` and uses `ct:pal/2` to output formatted diagnostics.

```erlang
-module(parser_error_cth).
-export([init/2, on_tc_fail/4]).

init(_Id, _Opts) ->
    {ok, #{}}.

on_tc_fail(_Suite, _TC, {failed, {error, {parse_error, Line, Col, Input}}}, State) ->
    %% Format error with context
    Lines = binary:split(Input, <<"\n">>, [global]),
    ErrorLine = lists:nth(Line, Lines),
    
    ct:pal("Parse error at line ~p, column ~p:~n~n~s~n~s^~n",
           [Line, Col, ErrorLine, lists:duplicate(Col-1, $\s)]),
    State;
on_tc_fail(_Suite, _TC, _Reason, State) ->
    State.
```

The `cth_readable` hook enhances terminal output readability with colored, well-formatted test results. It shows progress clearly, highlights failures, and provides compact success summaries. **Using `cth_readable_failonly` improves signal-to-noise by only showing output for failing tests**, keeping successful test runs clean while providing full details for failures. Install hooks in `suite/0` or via command line for different environments.

```erlang
suite() ->
    [{ct_hooks, [timing_cth, parser_error_cth, cth_readable_failonly]}].
```

### Running Spec Conformance Tests in CT

CommonMark defines over 500 test examples embedded in its specification. These tests form the baseline for parser correctness—your parser must pass all of them to claim CommonMark compliance. The spec test suite dynamically generates test cases from the CommonMark JSON file, creating one CT test case per example. This approach ensures comprehensive coverage of the specification.

```erlang
-module(spec_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([run_spec_test/1]).

all() ->
    %% Dynamically generate test case names
    {ok, Bin} = file:read_file("test/spec_tests/commonmark_spec.json"),
    Tests = jsx:decode(Bin),
    [list_to_atom("spec_" ++ integer_to_list(N)) 
     || #{<<"example">> := N} <- Tests].

init_per_suite(Config) ->
    %% Load and parse spec tests
    {ok, Bin} = file:read_file("test/spec_tests/commonmark_spec.json"),
    Tests = jsx:decode(Bin),
    [{spec_tests, Tests} | Config].

end_per_suite(Config) ->
    Config.

run_spec_test(Config) ->
    %% Extract test number from test case name
    TC = proplists:get_value(tc_name, Config),
    "spec_" ++ NumStr = atom_to_list(TC),
    N = list_to_integer(NumStr),
    
    %% Get test data
    Tests = ?config(spec_tests, Config),
    Test = lists:keyfind(N, <<"example">>, Tests),
    #{<<"markdown">> := Markdown, 
      <<"html">> := ExpectedHtml,
      <<"section">> := Section} = Test,
    
    %% Run the test
    {ok, AST} = my_parser:parse(Markdown),
    ActualHtml = my_parser:render_html(AST),
    
    %% Normalize and compare
    Expected = normalize_html(ExpectedHtml),
    Actual = normalize_html(ActualHtml),
    
    case Expected =:= Actual of
        true -> 
            ok;
        false -> 
            ct:fail({spec_mismatch, 
                     [{example, N}, 
                      {section, Section},
                      {markdown, Markdown},
                      {expected, Expected},
                      {actual, Actual}]})
    end.

normalize_html(Html) ->
    %% Remove insignificant whitespace
    Re1 = re:replace(Html, "\\s+", " ", [global, {return, binary}]),
    %% Trim leading/trailing
    Re2 = re:replace(Re1, "^\\s+|\\s+$", "", [global, {return, binary}]),
    %% Normalize tag spacing
    re:replace(Re2, "> <", "><", [global, {return, binary}]).
```

The implementation loads the spec file in `init_per_suite/1`, parses the JSON, and stores tests in the Config. The `all/0` function dynamically generates test case names for each spec example. Each test case calls a shared runner function that extracts the example, parses the Markdown input, renders to HTML, normalizes both expected and actual output, and asserts equality. When tests fail, CT logs the example number, input, expected output, and actual output for debugging.

Normalization proves crucial for HTML comparison. The normalizer removes insignificant whitespace, standardizes attribute order, and handles minor variations. Without normalization, tests fail on differences that don't affect rendering. A robust normalizer uses regular expressions to collapse whitespace runs, then trims leading and trailing space. More sophisticated versions parse HTML into a canonical form before comparison.

### Performance Benchmarking in CT

Performance regression testing catches speed degradations before they reach production. A dedicated performance suite runs longer benchmarks measuring throughput, latency percentiles, and memory usage. The suite includes warmup runs to stabilize performance before measurement, then executes operations multiple times and calculates statistics. Tests assert SLAs—average parse time must stay below thresholds, preventing silent performance degradation.

```erlang
-module(perf_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([bench_small_documents/1, bench_large_documents/1, bench_memory_usage/1]).

all() ->
    [bench_small_documents, bench_large_documents, bench_memory_usage].

init_per_suite(Config) ->
    %% Load test documents
    {ok, SmallDoc} = file:read_file("test/perf_data/small.md"),
    {ok, LargeDoc} = file:read_file("test/perf_data/large.md"),
    [{small_doc, SmallDoc}, {large_doc, LargeDoc} | Config].

end_per_suite(_Config) ->
    ok.

bench_small_documents(Config) ->
    Doc = ?config(small_doc, Config),
    
    %% Warmup
    [my_parser:parse(Doc) || _ <- lists:seq(1, 100)],
    
    %% Measure
    Iterations = 10000,
    {TotalTime, _} = timer:tc(fun() ->
        [my_parser:parse(Doc) || _ <- lists:seq(1, Iterations)]
    end),
    
    AvgTimeMicros = TotalTime / Iterations,
    ct:pal("Small document: ~.2f μs/parse", [AvgTimeMicros]),
    
    %% Assert SLA: must be under 100 microseconds
    true = AvgTimeMicros < 100.0.

bench_large_documents(Config) ->
    Doc = ?config(large_doc, Config),
    
    %% Collect latency samples
    Samples = [begin
        {Time, {ok, _}} = timer:tc(my_parser, parse, [Doc]),
        Time
    end || _ <- lists:seq(1, 1000)],
    
    %% Calculate percentiles
    Sorted = lists:sort(Samples),
    P50 = lists:nth(500, Sorted),
    P95 = lists:nth(950, Sorted),
    P99 = lists:nth(990, Sorted),
    
    ct:pal("Large document latency:~nP50: ~pμs~nP95: ~pμs~nP99: ~pμs",
           [P50, P95, P99]),
    
    %% Assert SLAs
    true = P50 < 5000,  %% 5ms
    true = P95 < 10000, %% 10ms
    true = P99 < 20000. %% 20ms

bench_memory_usage(Config) ->
    Doc = ?config(large_doc, Config),
    
    %% Baseline memory
    erlang:garbage_collect(),
    Baseline = erlang:memory(total),
    
    %% Parse many times
    [my_parser:parse(Doc) || _ <- lists:seq(1, 1000)],
    
    %% Force GC and measure
    [erlang:garbage_collect(P) || P <- erlang:processes()],
    Final = erlang:memory(total),
    
    Growth = Final - Baseline,
    GrowthMB = Growth / (1024 * 1024),
    
    ct:pal("Memory growth: ~.2f MB", [GrowthMB]),
    
    %% Assert memory stays bounded (< 10% growth)
    true = GrowthMB < (Baseline * 0.1 / (1024 * 1024)).
```

Using `timer:tc/3` measures wall-clock time accurately. Run the parser many times in a loop, measuring total time, then calculate average time per operation. Collect all timing samples to compute percentiles—P50 (median), P95, and P99. These percentiles reveal tail latency that averages might hide. A parser might average 1ms but occasionally spike to 100ms, causing user-visible delays. Percentile tracking catches these issues.

Memory usage tests verify the parser doesn't leak memory or use excessive space. Record baseline memory before parsing, execute many parse operations, force garbage collection across all processes, then measure final memory. The delta reveals memory growth. Production parsers must maintain bounded memory usage—**memory growth beyond 10% suggests leaks or inefficient allocation patterns**. This test catches accumulating memory issues before they crash production systems.

Continuous performance monitoring integrates benchmarks into CI. Save baseline performance metrics to version control, run benchmarks on every commit, and compare against baselines. A script analyzes results and fails CI if performance degrades beyond thresholds (typically 10-20%). This prevents performance regressions from accumulating, where many small slowdowns compound into major issues.

```bash
#!/bin/bash
# performance_check.sh

# Run benchmarks and extract metrics
rebar3 ct --suite perf_SUITE > perf_output.txt
CURRENT_P50=$(grep "P50:" perf_output.txt | awk '{print $2}' | tr -d 'μs')

# Load baseline
BASELINE_P50=$(cat baseline_metrics.txt | grep P50 | awk '{print $2}')

# Compare (allow 15% degradation)
THRESHOLD=$(echo "$BASELINE_P50 * 1.15" | bc)

if (( $(echo "$CURRENT_P50 > $THRESHOLD" | bc -l) )); then
    echo "Performance regression detected!"
    echo "Current P50: $CURRENT_P50μs"
    echo "Baseline P50: $BASELINE_P50μs"
    echo "Threshold: $THRESHOLD μs"
    exit 1
else
    echo "Performance OK"
    exit 0
fi
```

---

## Markdown-Specific Testing Strategies

Markdown presents unique testing challenges due to its complex syntax, ambiguous edge cases, and multiple competing specifications. Successful testing requires spec-driven validation, differential testing against reference implementations, comprehensive edge case coverage, and structured handling of pathological inputs. These strategies ensure your parser handles both common documents and edge cases correctly.

### CommonMark and GFM Specification Testing

The CommonMark specification embeds test cases directly in the spec document using a distinctive format. Each example includes Markdown input, expected HTML output, section name, and example number. The Python test runner `spec_tests.py` extracts these examples and runs them against any parser. This approach ensures the specification itself is executable—examples in the spec are actual test cases, preventing drift between documentation and reality.

The JSON format exports these tests as structured data: each object contains markdown string, html string, section identifier, and example number. Language-agnostic JSON enables any language to consume the tests easily. The `commonmark-spec` npm package provides these tests for JavaScript, while Python projects use `spec_tests.py --dump-tests` to extract JSON. For Erlang, parse the JSON in `init_per_suite/1` and create CT test cases dynamically.

GitHub Flavored Markdown extends CommonMark with tables, strikethrough, autolinks, and task lists. The GFM specification at github.github.com/gfm uses the same test format as CommonMark, clearly annotating extensions. Testing GFM requires running both the base CommonMark tests plus GFM extension tests. **Reference implementations like cmark-gfm serve as ground truth**—when the spec is ambiguous, the reference implementation's behavior defines correctness.

Running spec tests involves extracting test data, iterating through examples, parsing each input, rendering to HTML, normalizing output, and comparing results. The critical step is normalization—HTML can vary in whitespace and formatting while remaining semantically identical. A robust normalizer handles these variations, ensuring tests catch real semantic differences rather than formatting variations.

```erlang
%% GFM extension testing
-module(gfm_SUITE).
-include_lib("common_test/include/ct.hrl").

init_per_suite(Config) ->
    %% Load both CommonMark and GFM tests
    {ok, CMBin} = file:read_file("test/spec_tests/commonmark_spec.json"),
    {ok, GFMBin} = file:read_file("test/spec_tests/gfm_spec.json"),
    CMTests = jsx:decode(CMBin),
    GFMTests = jsx:decode(GFMBin),
    [{commonmark_tests, CMTests}, {gfm_tests, GFMTests} | Config].

test_gfm_tables(Config) ->
    GFMTests = ?config(gfm_tests, Config),
    TableTests = [T || T <- GFMTests, 
                       maps:get(<<"section">>, T) =:= <<"Tables">>],
    
    Results = [run_single_test(T) || T <- TableTests],
    Passed = length([ok || ok <- Results]),
    Total = length(Results),
    
    ct:pal("GFM Tables: ~p/~p passed", [Passed, Total]),
    true = Passed =:= Total.
```

### Differential Testing Against Reference Implementations

Differential testing compares your parser's output against established reference implementations for the same input. This technique catches subtle semantic bugs that spec tests might miss—cases where your parser produces valid HTML but with incorrect meaning. The CommonMark reference implementation cmark provides the gold standard, while cmark-gfm adds GFM extensions.

```erlang
-module(differential_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, init_per_suite/1]).
-export([test_against_cmark/1, test_against_cmark_gfm/1]).

all() ->
    [test_against_cmark, test_against_cmark_gfm].

init_per_suite(Config) ->
    %% Verify reference implementations are available
    case os:cmd("which cmark") of
        [] -> ct:fail("cmark not found in PATH");
        _ -> ok
    end,
    Config.

test_against_cmark(Config) ->
    %% Use PropEr to generate test cases
    true = ct_property_test:quickcheck(
        differential_properties:prop_matches_cmark(),
        [{numtests, 1000} | Config]
    ).

%% In differential_properties.erl:
prop_matches_cmark() ->
    ?FORALL(Markdown, markdown_generators:valid_markdown(),
            begin
                MyHTML = my_parser:to_html(Markdown),
                CmarkHTML = call_cmark(Markdown),
                
                Normalized1 = normalize_html(MyHTML),
                Normalized2 = normalize_html(CmarkHTML),
                
                ?WHENFAIL(
                    begin
                        ct:pal("Input: ~s", [Markdown]),
                        ct:pal("Mine: ~s", [Normalized1]),
                        ct:pal("Cmark: ~s", [Normalized2])
                    end,
                    Normalized1 =:= Normalized2
                )
            end).

call_cmark(Markdown) ->
    Port = open_port({spawn, "cmark"}, [binary, stream, use_stdio]),
    port_command(Port, Markdown),
    port_command(Port, eof),
    receive_output(Port, <<>>).

receive_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            receive_output(Port, <<Acc/binary, Data/binary>>);
        {Port, eof} ->
            port_close(Port),
            Acc
    after 5000 ->
        port_close(Port),
        error(timeout)
    end.
```

The implementation calls the reference parser as an external process. Write Markdown to a port running `cmark`, read the output, and compare against your parser's output. For performance, use a long-lived port that accepts multiple inputs. The comparison requires normalization since reference implementations might format output differently—focus on semantic equivalence rather than exact string matching.

Babelmark 3 enables crowd-sourced differential testing by comparing over 20 Markdown implementations simultaneously. Visit the website with test input to see how different parsers handle it. This reveals community consensus—when 18 of 20 parsers agree on output, they're probably correct. Use Babelmark during development to resolve ambiguous spec cases and verify edge case handling matches community expectations.

The markdown-testsuite project provides systematic cross-implementation testing with 103 modular tests. It runs the same tests against multiple parsers and reports compliance rates. This identifies compatibility gaps and common failure modes. **Use this knowledge to prioritize which edge cases matter most for your use case**.

### Fuzzing for Security and Robustness

Fuzzing discovers crashes, hangs, and security issues by feeding random or mutated inputs to the parser. For Erlang projects with NIFs, compile the NIF code with AFL instrumentation and run the fuzzer with CommonMark spec tests as initial corpus. The fuzzer mutates these inputs, tracking which mutations increase coverage, and runs continuously looking for crashes. **Fuzzing should run continuously in CI for hours or days**, not just minutes—many bugs require millions of executions to discover.

```erlang
%% Fuzz testing harness
-module(fuzz_SUITE).
-include_lib("common_test/include/ct.hrl").

test_random_inputs(Config) ->
    %% Generate and test random inputs
    Iterations = 100000,
    Results = [test_one_random_input() || _ <- lists:seq(1, Iterations)],
    
    Crashes = [R || R <- Results, element(1, R) =:= crash],
    ct:pal("Tested ~p inputs, found ~p crashes", [Iterations, length(Crashes)]),
    
    case Crashes of
        [] -> ok;
        _ -> ct:fail({crashes_found, Crashes})
    end.

test_one_random_input() ->
    %% Generate random binary
    Size = rand:uniform(10000),
    Input = crypto:strong_rand_bytes(Size),
    
    try
        _ = my_parser:parse(Input),
        {ok, Input}
    catch
        Class:Reason:Stack ->
            {crash, {Input, Class, Reason, Stack}}
    end.
```

The Google fuzzing dictionary for Markdown provides seed tokens that fuzzers can insert or mutate. The dictionary includes Markdown syntax elements like `**`, `~~`, ` ``` `, `[`, `](`, and GFM extensions. Using this dictionary dramatically improves fuzzing effectiveness because fuzzers don't waste time discovering basic syntax—they start with valid tokens and focus on combining them in interesting ways.

```
# markdown.dict - Fuzzing dictionary for Markdown
"#"
"##"
"###"
"**"
"__"
"*"
"_"
"```"
"~~~"
"["
"]"
"("
")"
"<"
">"
"!"
"|"
"-"
"+"
"1."
"`"
"\n"
"\r\n"
"  \n"
```

MdPerfFuzz represents advanced fuzzing targeting performance bugs rather than crashes. It generates syntax-tree-based mutations that exercise parser complexity, monitors execution time, and identifies inputs causing exponential slowdowns. These pathological inputs cause denial-of-service in production when users submit carefully crafted documents. Performance fuzzing catches these issues before attackers discover them.

### Golden File Testing for Complex Outputs

Golden file testing (snapshot testing) verifies complex outputs by comparing against reference files. Store expected output for each test input in a "golden" file, run the parser, and diff actual output against the golden file. This approach works well for AST structures, HTML rendering, and other outputs too complex for manual verification.

```erlang
-module(golden_SUITE).
-include_lib("common_test/include/ct.hrl").

test_ast_output(Config) ->
    DataDir = ?config(data_dir, Config),
    InputFile = filename:join(DataDir, "complex_document.md"),
    GoldenFile = filename:join(DataDir, "complex_document.ast"),
    
    {ok, Input} = file:read_file(InputFile),
    {ok, AST} = my_parser:parse(Input),
    
    %% Serialize AST to comparable format
    ActualOutput = io_lib:format("~p", [AST]),
    
    case file:read_file(GoldenFile) of
        {ok, Expected} ->
            %% Compare with golden file
            case iolist_to_binary(ActualOutput) =:= Expected of
                true -> ok;
                false -> ct:fail({golden_mismatch, GoldenFile})
            end;
        {error, enoent} ->
            %% Golden file doesn't exist - create it (if --update flag)
            case os:getenv("UPDATE_GOLDEN") of
                "true" ->
                    ok = file:write_file(GoldenFile, ActualOutput),
                    ct:pal("Created golden file: ~s", [GoldenFile]);
                _ ->
                    ct:fail({missing_golden, GoldenFile})
            end
    end.
```

The implementation stores test inputs in `test_data/input/` and golden outputs in `test_data/golden/`. For each test, load input, parse it, serialize the result, load the corresponding golden file, and assert equality. When developing new features or fixing bugs, run tests with `UPDATE_GOLDEN=true` to regenerate golden files, review changes with git diff, and commit updated files if correct.

For AST testing, serialize the AST to a human-readable format before comparison. Erlang's `io_lib:format("~p", [AST])` produces readable output that diffs well. Alternatively, convert to JSON for a more standard format. The key requirement is deterministic serialization—the same AST must always serialize identically, or false failures will plague the test suite.

Git integration provides version control for golden files naturally. Commits capture intentional output changes with explanatory messages. Code reviews show exactly how parser output evolved. CI fails if golden files change without being committed, catching unintended output modifications.

### Edge Cases and Pathological Inputs

Certain Markdown constructs trigger quadratic or exponential complexity in naive parsers. **Deeply nested brackets** like `[[[[[[...thousands deep...]]]]]]` can cause exponential time if the parser tries all possible bracket matches. Deeply nested block quotes with thousands of `>` prefixes can cause stack overflow or extreme memory usage. Many unclosed emphasis markers force backtracking across the entire document. These pathological cases must have explicit tests.

```erlang
-module(pathological_SUITE).
-include_lib("common_test/include/ct.hrl").

test_deeply_nested_brackets(Config) ->
    %% Generate deeply nested structure
    Depth = 3000,
    Input = lists:duplicate(Depth, $[) ++ 
            lists:duplicate(Depth, $]),
    
    %% Should complete in reasonable time
    {Time, Result} = timer:tc(my_parser, parse, [Input]),
    
    ct:pal("Parsed ~p-deep brackets in ~pμs", [Depth, Time]),
    
    %% Must complete in under 1 second
    true = Time < 1000000,
    %% Must return valid result
    {ok, _AST} = Result.

test_nested_emphasis(Config) ->
    %% Many unclosed emphasis markers
    Input = binary:copy(<<"*">>, 10000) ++ <<"text">>,
    
    {Time, Result} = timer:tc(my_parser, parse, [Input]),
    ct:pal("Parsed pathological emphasis in ~pμs", [Time]),
    
    %% Should handle gracefully
    true = Time < 5000000,
    case Result of
        {ok, _} -> ok;
        {error, _} -> ok %% Error is acceptable, crash is not
    end.

test_unicode_edge_cases(Config) ->
    TestCases = [
        %% Combining characters
        <<"# Foo χρῆν\n">>,
        %% Emoji
        <<"## Hi, *Saturn*! 🪐\n">>,
        %% Zero-width characters
        <<"Test​invisible​spaces\n">>,
        %% RTL text
        <<"# שלום עולם\n">>,
        %% Mixed CRLF/LF
        <<"Line 1\r\nLine 2\nLine 3\r\n">>
    ],
    
    Results = [{TC, test_unicode_case(TC)} || TC <- TestCases],
    
    Failures = [R || {_TC, Result} = R <- Results, Result =/= ok],
    case Failures of
        [] -> ok;
        _ -> ct:fail({unicode_failures, Failures})
    end.

test_unicode_case(Input) ->
    try
        {ok, _AST} = my_parser:parse(Input),
        ok
    catch
        _:_ -> {error, Input}
    end.
```

The cmark project includes `pathological_tests.py` specifically for these cases. It generates deeply nested structures, times parsing, and asserts completion within reasonable bounds. After optimization, cmark parses 3000-level nesting in 0.7 seconds where naive implementations take minutes or fail. Your parser must handle these cases efficiently—attackers can submit pathological documents to overload servers.

Unicode edge cases require dedicated testing. Markdown must handle combining characters, right-to-left text, emoji, zero-width characters, and various line ending styles (CRLF vs LF). These cases expose encoding bugs, character boundary issues, and rendering problems.

Complex nesting scenarios test structural handling. Block quotes containing headings containing lists containing code blocks creates deeply nested context requiring careful state management. Different parsers historically handled these cases inconsistently. CommonMark resolves ambiguities explicitly, but testing complex nesting ensures your parser matches the spec.

### AST Structural Testing

Testing the abstract syntax tree directly verifies parser internals rather than just rendered output. AST testing catches structural bugs—cases where output happens to be correct despite wrong internal representation.

```erlang
-module(ast_SUITE).
-include_lib("common_test/include/ct.hrl").

test_ast_invariants(Config) ->
    true = ct_property_test:quickcheck(
        ast_properties:prop_well_formed_ast(),
        Config
    ).

%% In ast_properties.erl:
prop_well_formed_ast() ->
    ?FORALL(Markdown, markdown_generators:markdown_document(4),
            case my_parser:parse(Markdown) of
                {ok, AST} ->
                    validate_ast(AST);
                {error, _} ->
                    true %% Parse errors are acceptable
            end).

validate_ast({document, Children}) ->
    lists:all(fun is_block_element/1, Children) andalso
    lists:all(fun validate_ast/1, Children);
validate_ast({heading, Level, Children}) ->
    Level >= 1 andalso Level =< 6 andalso
    lists:all(fun is_inline_element/1, Children) andalso
    lists:all(fun validate_ast/1, Children);
validate_ast({paragraph, Children}) ->
    lists:all(fun is_inline_element/1, Children) andalso
    lists:all(fun validate_ast/1, Children);
validate_ast({list, Type, Items}) ->
    lists:member(Type, [ordered, unordered]) andalso
    lists:all(fun validate_list_item/1, Items);
validate_ast({text, Text}) when is_binary(Text) ->
    true;
validate_ast({emphasis, Children}) ->
    lists:all(fun is_inline_element/1, Children) andalso
    lists:all(fun validate_ast/1, Children);
validate_ast({strong, Children}) ->
    lists:all(fun is_inline_element/1, Children) andalso
    lists:all(fun validate_ast/1, Children);
validate_ast({code, Text}) when is_binary(Text) ->
    true;
validate_ast(_) ->
    false.

is_block_element({heading, _, _}) -> true;
is_block_element({paragraph, _}) -> true;
is_block_element({code_block, _, _}) -> true;
is_block_element({list, _, _}) -> true;
is_block_element({blockquote, _}) -> true;
is_block_element(_) -> false.

is_inline_element({text, _}) -> true;
is_inline_element({emphasis, _}) -> true;
is_inline_element({strong, _}) -> true;
is_inline_element({code, _}) -> true;
is_inline_element({link, _, _}) -> true;
is_inline_element(_) -> false.
```

AST validation tests verify structural invariants. Every node must have a valid type, block nodes must only contain appropriate children, inline nodes must not contain blocks, heading depth must be 1-6, and position information must be consistent. Walk the AST recursively, checking types and structure at each node. This catches bugs where the parser creates malformed trees that might crash transformers or renderers.

Round-trip testing parses Markdown to AST, serializes AST back to Markdown, and parses again. The two ASTs should match structurally even if string representation differs. This verifies the parser and serializer are inverses.

Positional information enables source maps, syntax highlighting, and accurate error messages. Each AST node includes start and end positions (line, column) in the source. Test that positions are correct—generate Markdown with known structure, parse to AST, verify each node's position matches the source.

```erlang
test_position_tracking(Config) ->
    Input = <<"# Heading\n\nParagraph text.\n\n- Item 1\n- Item 2\n">>,
    {ok, AST} = my_parser:parse_with_positions(Input),
    
    %% Verify heading position
    {document, [{heading, 1, _Text, #{start := {1, 1}, end := {1, 9}}} | _]} = AST,
    
    %% Verify paragraph position
    {document, [_, {paragraph, _Text, #{start := {3, 1}, end := {3, 16}}} | _]} = AST,
    
    ok.
```

---

## Building Robust Test Infrastructure

Test infrastructure provides the foundation for all testing activities. Well-organized test data, comprehensive coverage analysis, performance regression tracking, memory leak detection, automated CI/CD, and effective debugging tools enable productive parser development. These systems must be in place before starting serious parser work.

### Test Data Management and Organization

Common Test uses `data_dir` for test fixtures and `priv_dir` for generated outputs. Access these via `?config(data_dir, Config)` and `?config(priv_dir, Config)`. Store static test files, spec tests, and golden files in `_SUITE_data/` directories. The framework automatically makes these available to tests. For generated data or test outputs, write to `priv_dir`—CT provides a clean directory for each test run.

Organize test data hierarchically by type. Place valid test cases under `valid/`, invalid under `invalid/`, pathological cases under `pathological/`, and real-world documents under `real_world/`. Within each category, group by feature—`valid/emphasis/`, `valid/lists/`, etc. This structure makes tests easy to find and maintain.

```
test/
└── parser_SUITE_data/
    ├── valid/
    │   ├── emphasis/
    │   │   ├── basic_emphasis.md
    │   │   ├── nested_emphasis.md
    │   │   └── mixed_emphasis.md
    │   ├── lists/
    │   │   ├── ordered_list.md
    │   │   ├── unordered_list.md
    │   │   └── nested_list.md
    │   └── headings/
    │       ├── atx_headings.md
    │       └── setext_headings.md
    ├── invalid/
    │   ├── unclosed_emphasis.md
    │   ├── malformed_list.md
    │   └── invalid_heading.md
    ├── pathological/
    │   ├── deep_nesting.md
    │   ├── many_emphasis.md
    │   └── huge_document.md
    ├── golden/
    │   ├── basic_emphasis.ast
    │   ├── nested_list.ast
    │   └── complex_document.ast
    └── real_world/
        ├── readme.md
        ├── spec.md
        └── tutorial.md
```

For spec conformance, store the CommonMark JSON file in `test/spec_tests/commonmark_spec.json`. Version this file and update it when new CommonMark versions release. Track the spec version in test suite comments or configuration. When updating the spec file, run tests and carefully review any new failures—they indicate behavior changes in the standard.

Managing large test suites requires automation. Generate test cases programmatically from data files rather than hand-writing each test function. Use CT's dynamic test case generation to create hundreds of tests from JSON or CSV data. This scales to thousands of tests without code explosion and makes adding new tests trivial—just add data.

### Coverage Analysis for Parsers

The Erlang `cover` tool provides line and branch coverage analysis. Enable it in `rebar.config` with `{cover_enabled, true}` and run tests with coverage via `rebar3 ct --cover`. Generate HTML reports showing exactly which lines executed and which branches were taken. **Target 90% coverage for parser core logic** and 80% for error handling paths—comprehensive coverage catches bugs before production.

```erlang
%% rebar.config
{cover_enabled, true}.
{cover_opts, [verbose]}.
{cover_excl_mods, [
    test_helper,
    mock_parser,
    generated_lexer
]}.
```

Coverage analysis reveals untested code paths. Red lines in HTML reports show code that never executed during tests. Examine these lines—they might indicate missing tests, dead code, or impossible-to-reach defensive checks. Add tests to cover these lines or remove dead code. Pay special attention to error handling paths—these often have low coverage but high importance.

For Erlang projects, `rebar3_codecov` generates coverage reports in Codecov's JSON format for integration with codecov.io. Configure it in `rebar.config`, run `rebar3 as test codecov analyze`, and upload to Codecov in CI. This provides coverage tracking over time, showing whether coverage improves or degrades with each commit.

```bash
# In CI
rebar3 do ct --cover, cover
rebar3 as test codecov analyze
bash <(curl -s https://codecov.io/bash)
```

Exclude test helper modules from coverage using `{cover_excl_mods, [test_helper, mock_module]}`. These modules support testing but aren't application code. Including them in coverage metrics creates false signals. Similarly, exclude third-party dependencies. Focus coverage metrics on your parser code exclusively.

### Performance Regression Testing

Performance regressions are bugs—they impact user experience just like correctness bugs. Track parser performance over time and alert when it degrades significantly. The `erlperf` tool provides microbenchmarking for Erlang functions, measuring throughput and latency.

```erlang
%% Store baseline metrics
-module(perf_baseline).
-export([load/0, save/1, compare/2]).

-record(baseline, {
    small_doc_p50 = 0,
    small_doc_p95 = 0,
    large_doc_p50 = 0,
    large_doc_p95 = 0,
    memory_mb = 0.0
}).

load() ->
    case file:consult("test/perf_baseline.erl") of
        {ok, [Baseline]} -> Baseline;
        _ -> #baseline{}
    end.

save(Baseline) ->
    Content = io_lib:format("~p.~n", [Baseline]),
    file:write_file("test/perf_baseline.erl", Content).

compare(Current, Baseline) ->
    Threshold = 1.15, %% 15% degradation allowed
    
    Checks = [
        {small_p50, Current#baseline.small_doc_p50, 
                    Baseline#baseline.small_doc_p50},
        {large_p50, Current#baseline.large_doc_p50,
                    Baseline#baseline.large_doc_p50}
    ],
    
    [{Name, Curr, Base, Curr / Base} || {Name, Curr, Base} <- Checks,
                                         Curr / Base > Threshold].
```

Design benchmarks with realistic inputs. Use the actual markdown files users will parse—README files, documentation, specifications. Include small inputs (paragraphs), medium inputs (full articles), and large inputs (books). Measure throughput (parses per second) and latency (time per parse) separately—they reveal different performance characteristics.

Store baseline performance metrics in version control as text files. After each benchmark run, compare current results against baselines using a tolerance threshold (typically 10-15%). If performance degrades beyond the threshold, fail CI and require investigation. This prevents the "death by a thousand cuts" where many small regressions accumulate into major slowdowns.

### Memory Leak Detection Techniques

The `recon` library provides production-grade memory analysis. Use `recon:proc_count(memory, 10)` to find processes consuming the most memory, `recon:bin_leak/10` to detect binary leaks by forcing GC and examining surviving binaries, and `recon_alloc:memory/1` to check for memory fragmentation.

```erlang
test_binary_leaks(Config) ->
    Doc = ?config(large_doc, Config),
    
    %% Initial state
    InitialBins = recon:bin_leak(10),
    
    %% Parse many times
    [my_parser:parse(Doc) || _ <- lists:seq(1, 1000)],
    
    %% Force GC everywhere
    [erlang:garbage_collect(P) || P <- erlang:processes()],
    
    %% Check for leaked binaries
    FinalBins = recon:bin_leak(10),
    
    %% Compare
    case length(FinalBins) > length(InitialBins) * 1.5 of
        true ->
            ct:pal("Possible binary leak detected:~n~p", [FinalBins]),
            ct:fail(binary_leak);
        false ->
            ok
    end.
```

For NIFs written in C, AddressSanitizer catches memory errors that Erlang tools miss. Build the emulator with ASan enabled, run tests, and examine asan logs for use-after-free, buffer overflows, and leaks. ASan adds overhead but catches serious memory corruption bugs.

```bash
# Build with AddressSanitizer
export ASAN_OPTIONS=detect_leaks=1
kerl build git https://github.com/erlang/otp OTP-27.0 27.0-asan --enable-address-sanitizer
kerl install 27.0-asan ~/kerl/27.0-asan
. ~/kerl/27.0-asan/activate

# Run tests
rebar3 ct
```

Valgrind provides another layer of memory debugging for NIF code. Run the Erlang emulator under Valgrind with `valgrind --leak-check=full erl`, execute tests, and review leak reports. **The combination of ASan and Valgrind provides comprehensive memory safety validation for NIF code**.

Continuous memory monitoring in test suites involves before/after measurements. Record baseline memory, execute operations, force GC, measure final memory. Assert delta stays within bounds. Run these tests repeatedly to catch intermittent leaks.

### CI/CD Integration Patterns

GitHub Actions provides robust CI for Erlang projects. The erlef/setup-beam action installs Erlang/OTP and rebar3, supporting matrix builds across multiple OTP versions. Cache dependencies and Dialyzer PLTs to speed up builds.

```yaml
# .github/workflows/ci.yml
name: CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: ['25', '26', '27']
      fail-fast: false
    
    steps:
      - uses: actions/checkout@v3
      
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: '3.22'
      
      - name: Cache Hex packages
        uses: actions/cache@v3
        with:
          path: ~/.cache/rebar3/hex
          key: ${{ runner.os }}-hex-${{ hashFiles('**/rebar.lock') }}
      
      - name: Cache Dialyzer PLTs
        uses: actions/cache@v3
        with:
          path: ~/.cache/rebar3/rebar3_*_plt
          key: ${{ runner.os }}-plt-${{ matrix.otp }}-${{ hashFiles('**/rebar.config') }}
      
      - name: Compile
        run: rebar3 compile
      
      - name: Dialyzer
        run: rebar3 dialyzer
      
      - name: Run tests
        run: rebar3 do eunit, ct --cover
      
      - name: Generate coverage
        run: rebar3 cover
      
      - name: Upload coverage
        if: matrix.otp == '27'
        uses: codecov/codecov-action@v3
        with:
          files: _build/test/cover/eunit.coverdata,_build/test/cover/ct.coverdata
      
      - name: Performance tests
        run: rebar3 ct --suite perf_SUITE
      
      - name: Check performance regression
        run: ./scripts/check_performance.sh
```

The recommended workflow tests against multiple OTP versions (25, 26, 27) to ensure compatibility. Use a matrix strategy with `fail-fast: false` to run all versions even if one fails. Upload test artifacts (logs, coverage reports) for failed runs to enable debugging. Upload coverage to Codecov only for the latest OTP version to avoid redundant uploads.

Cache configuration proves critical for performance. Cache Hex packages, Dialyzer PLTs, and use cache keys based on OTP version and lock file hashes. This ensures caches invalidate when dependencies change but persist across runs with stable dependencies. **Effective caching reduces CI runtime by 60-80%**.

For performance testing in CI, store baseline metrics and run abbreviated benchmarks. Full benchmarks might take hours; CI runs should complete in minutes. Run abbreviated benchmarks on every commit and full benchmarks nightly.

### Debugging Tools and Workflows

The Erlang debugger provides step-through debugging for failing tests. Call `debugger:start()`, interpret the module with `int:i(parser)`, set breakpoints with `int:break(parser, 42)`, and run tests. The GUI debugger lets you step through code, examine variables, and understand control flow.

```erlang
%% Debugging helper in test suite
debug_test_case(Module, Function, Args) ->
    debugger:start(),
    int:i(Module),
    int:break(Module, Function),
    apply(Module, Function, Args),
    debugger:stop().
```

Observer provides real-time system monitoring. Call `observer:start()` to launch the GUI, then examine processes, memory usage, reduction counts, and message queues while tests run. **Observer is indispensable for understanding system behavior during complex tests**.

The `dbg` module enables programmatic tracing without GUI overhead. Start the tracer, enable process tracing, trace functions, and capture trace output. Match specs filter traced calls.

```erlang
%% Trace parser function calls
trace_parser() ->
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(my_parser, parse, [{'_', [], [{return_trace}]}]),
    
    %% Now run your test
    my_parser:parse(<<"# Test\n">>),
    
    %% Stop tracing
    dbg:stop_clear().
```

The `recon_trace` library provides safer production-ready tracing. Unlike `dbg`, `recon_trace` limits trace message volume to prevent overwhelming the system.

```erlang
%% Safe tracing with rate limiting
recon_trace:calls({my_parser, parse, fun(_) -> return_trace() end}, 10).
```

For systematic debugging of test failures, use CT logging liberally. `ct:pal/2` logs to both console and HTML report. Log inputs, intermediate values, and outputs for failing tests. Add detailed logging in `end_per_testcase/2` that activates only for failures.

```erlang
end_per_testcase(TC, Config) ->
    case ?config(tc_status, Config) of
        ok -> 
            ok;
        {failed, Reason} ->
            %% Log extra diagnostics for failures
            ct:pal("Test ~p failed: ~p", [TC, Reason]),
            ct:pal("Config: ~p", [Config]),
            
            %% Dump parser state if available
            case ?config(parser_state, Config) of
                undefined -> ok;
                State -> ct:pal("Parser state: ~p", [State])
            end
    end.
```

---

## Lessons from Reference Implementations

Studying successful parser implementations reveals patterns that work in production. The markdown-rs, cmark, and Erlang OTP compiler projects demonstrate testing strategies that scale to complex parsers handling millions of documents.

### How markdown-rs Achieves 100% Coverage

The markdown-rs project demonstrates that 100% code coverage is achievable through systematic testing. It combines spec conformance tests (650+ CommonMark tests), extensive additional tests (1000+ cases verified against reference implementations), and continuous fuzzing with both libFuzzer and honggfuzz. The key insight is that **coverage alone doesn't guarantee correctness**—fuzzing finds bugs that fall through 100% coverage by exploring input combinations that tests miss.

Test organization follows Rust idioms with unit tests embedded in source files using `#[cfg(test)]` modules. Integration tests live in the `tests/` directory. Fuzzing targets are in `fuzz/`. This separation clarifies test types and purposes.

The project generates tests programmatically from specifications. A code generator reads CommonMark spec JSON, extracts examples, and generates test functions. This automation ensures all spec tests run and makes updating for new spec versions trivial.

Performance testing is integrated with correctness testing. The project includes benchmarks comparing against cmark and other parsers. Tracking performance relative to the C reference implementation ensures markdown-rs maintains competitive speed.

### cmark's Spec-Driven Test Approach

The cmark project pioneered embedding tests in the specification itself. The `spec.txt` file is simultaneously human-readable documentation and machine-executable test cases. Special delimiters mark test examples, which include both input and expected output.

The test extraction script `spec_tests.py` parses `spec.txt`, extracts examples, and runs them against any parser accepting input on stdin and producing HTML on stdout. This language-agnostic interface enables testing parsers in any language against the same spec tests.

Pathological input testing is a first-class concern in cmark. The project includes `pathological_tests.py` with specific examples designed to stress parsers. After extensive AFL fuzzing revealed performance bugs, the team optimized hot paths and added regression tests. **These pathological tests now serve as regression prevention**.

The project's makefile integrates testing thoroughly: `make test`, `make leakcheck`, `make fuzztest`. This integration makes quality gates easy to run.

### Erlang OTP Compiler Patterns

The Erlang/OTP compiler test suites demonstrate testing at scale. Hundreds of test suites cover every aspect of compilation. The `compiler/test/` directory contains focused suites for specific features, each named `*_SUITE.erl`. This granular organization makes tests easy to find and run individually.

Test suites mirror module structure—one test suite per major module or feature. This convention helps developers locate tests quickly. The parallel structure between source and tests makes navigation intuitive.

The OTP project uses makefile-based test execution extensively. This integration with the build system ensures tests always use the correct built artifacts.

Error path testing receives equal attention to success paths. Every error message has a test verifying it triggers under appropriate conditions and contains useful information. This discipline prevents error handling code from rotting.

---

## Putting It All Together: A Practical Test Suite Blueprint

Building a comprehensive Markdown parser test suite requires integrating all these strategies coherently. Start with spec conformance as the foundation, add property-based testing for randomized coverage, implement differential testing against reference parsers, integrate fuzzing for crash detection, build performance regression tracking, and establish robust CI/CD.

### Initial Setup and Directory Structure

Begin with a clean directory structure that separates concerns. Place source code in `src/`, tests in `test/`, documentation in `doc/`, and configuration at the root. Within `test/`, create subdirectories for each test type.

```
erlmd/
├── src/
│   ├── erlmd.app.src
│   ├── erlmd.erl                 % Public API
│   ├── erlmd_parser.erl          % Main parser orchestration
│   ├── erlmd_tokenizer.erl       % Core state machine
│   ├── erlmd_ast.erl             % AST utilities
│   ├── erlmd_event.erl           % Event generation/handling
│   └── ...
├── test/
│   ├── unit/
│   │   ├── lexer_SUITE.erl
│   │   └── parser_SUITE.erl
│   ├── integration/
│   │   └── full_pipeline_SUITE.erl
│   ├── spec/
│   │   ├── spec_SUITE.erl
│   │   └── spec_SUITE_data/
│   │       └── commonmark.json
│   ├── perf/
│   │   └── perf_SUITE.erl
│   ├── property/
│   │   ├── generators.erl
│   │   └── properties.erl
│   └── data/
│       ├── fixtures/
│       ├── golden/
│       └── pathological/
├── rebar.config
└── .github/
    └── workflows/
        └── ci.yml
```

Configure rebar3 with appropriate defaults:

```erlang
%% rebar.config
{erl_opts, [debug_info, warnings_as_errors]}.

{deps, []}.

{profiles, [
    {test, [
        {deps, [
            {proper, "1.4.0"}
        ]},
        {erl_opts, [nowarn_export_all]}
    ]}
]}.

{cover_enabled, true}.
{cover_opts, [verbose]}.
{cover_excl_mods, [test_helper, mock_parser]}.

{dialyzer, [
    {warnings, [unknown]},
    {plt_apps, all_deps}
]}.

{alias, [
    {check, [compile, dialyzer, eunit, ct, cover]}
]}.
```

### Test Execution Workflow

Developers should run tests frequently during development. The workflow starts with unit tests for rapid feedback, then integration tests, then the full suite before committing.

```bash
# Quick feedback loop
rebar3 eunit  # < 10 seconds

# More comprehensive
rebar3 ct     # < 2 minutes

# Property tests
rebar3 proper # 2-5 minutes

# Full validation
rebar3 check  # < 10 minutes
```

For comprehensive pre-commit validation: `rebar3 do compile, dialyzer, eunit, ct, cover`. The entire suite should complete in under 10 minutes for rapid iteration.

In CI, run the full suite including extended tests. Increase PropEr iteration counts from 100 to 10,000 for thorough property checking. Run pathological input tests with large inputs. Execute performance benchmarks and memory leak detection.

### Coverage Goals and Metrics

Set explicit coverage targets by code type:
- Parser core logic: 90%+ coverage
- Error handling: 80%+ coverage
- Utility functions: 70%+ coverage
- Test support code: No tracking

Track coverage trends over time rather than absolute numbers. Coverage should increase or remain stable with each commit. **Make coverage increase a requirement for accepting pull requests**.

Focus on branch coverage, not just line coverage. The Erlang `cover` tool provides both metrics. Review branch coverage reports to find untested error conditions and edge cases.

Exclude appropriate code from coverage metrics. Test helpers, mock modules, and generated code shouldn't count toward coverage.

### Continuous Improvement

Test suites evolve with the parser. When fixing bugs, first add a failing test demonstrating the bug, then implement the fix. This ensures the bug stays fixed.

Regularly review and refactor tests. Remove obsolete tests. Consolidate duplicates. Improve test names and documentation. **Well-maintained tests are as important as well-maintained code**.

Monitor test execution time and optimize slow tests. Profile test execution to find bottlenecks. Use test groups to run fast tests frequently and slow tests less often.

Add tests for every user-reported bug. When users find issues, it indicates gaps in the test suite. Write regression tests, verify they fail without the fix, then implement the fix.

## Implementation Timeline and Priorities

**Phase 1 (2-3 weeks): Foundation - Correctness First**
- Implement spec conformance testing framework
- Achieve 80%+ CommonMark spec pass rate
- Build basic unit tests for lexer and parser
- Establish Common Test integration
- Set up test data directory structure

**Phase 2 (2-3 weeks): Depth - Property-Based Testing**
- Implement PropEr generators for Markdown structures
- Write core properties (roundtrip, structural invariants)
- Achieve 90%+ spec pass rate
- Add differential testing against cmark
- Reach 80%+ code coverage

**Phase 3 (2-3 weeks): Robustness - Non-Functional Testing**
- Integrate fuzzing infrastructure
- Implement performance regression tests
- Add memory leak detection
- Test pathological inputs
- Achieve 90%+ code coverage

**Phase 4 (1-2 weeks): Automation - CI/CD Pipeline**
- Build comprehensive CI/CD pipeline
- Integrate all tests into CI
- Set up coverage tracking and trending
- Implement automated performance monitoring
- Configure multi-version OTP testing

**Total: 8-12 weeks** from starting testing to production-ready parser, assuming a parser of moderate complexity. **This investment is essential**—the testing infrastructure prevents bugs, enables confident refactoring, and provides documentation through examples.

---

## Conclusion

Testing a Markdown parser in Erlang requires multiple complementary strategies working together:

- **PropEr property-based testing** finds edge cases through randomization
- **Common Test** provides organizational structure and spec conformance validation
- **Differential testing** ensures semantic correctness against reference implementations
- **Fuzzing** discovers crashes and security issues
- **Golden files** validate complex outputs
- **Performance tests** prevent regressions
- **Comprehensive CI/CD** automates everything

This defense-in-depth approach has enabled production parsers to achieve 100% coverage and serve millions of documents reliably. Start with spec conformance, add property testing, integrate fuzzing, and build robust infrastructure. The result is a parser you can trust in production.

The patterns and code examples in this guide are drawn from production systems and can be implemented immediately. Begin testing from day one of parser development—retrofitting tests is much harder than building them alongside the parser. With proper testing infrastructure, you'll ship a robust, correct, and performant Markdown parser that users can depend on.
