-module(test_blockquote_spec_analysis).
-export([run/0]).

run() ->
    io:format("~n=== Blockquote Hard Break Behavior Analysis ===~n~n"),

    % Test 1: Regular consecutive blockquotes (no trailing spaces)
    io:format("Test 1: Regular blockquote merge (no hard break)~n"),
    test("> line1\n> line2", "Should merge with <br />"),

    % Test 2: Hard break in first line (two trailing spaces)
    io:format("Test 2: Hard break in first blockquote line~n"),
    test("> line1  \n> line2", "First line has trailing spaces"),

    % Test 3: Hard break in second line
    io:format("Test 3: Hard break in second blockquote line~n"),
    test("> line1\n> line2  ", "Second line has trailing spaces"),

    % Test 4: Hard breaks in both lines
    io:format("Test 4: Hard breaks in both lines~n"),
    test("> line1  \n> line2  ", "Both lines have trailing spaces"),

    % Test 5: What about a single blockquote with hard break?
    io:format("Test 5: Single blockquote line with trailing spaces~n"),
    test("> line1  ", "Single line with trailing spaces"),

    % Test 6: Single blockquote, then normal line (with hard break)
    io:format("Test 6: Blockquote with hard break, then normal~n"),
    test("> line1  \nnormal", "Blockquote consumes normal after hard break"),

    io:format("~n=== Analysis Complete ===~n").

test(Input, Description) ->
    io:format("  Input: ~p~n", [Input]),
    io:format("  Desc:  ~s~n", [Description]),

    Orig = erlmd:conv_original(Input),
    AST = erlmd:conv_ast(Input),

    io:format("  Original: ~s", [Orig]),
    io:format("  AST:      ~s", [AST]),

    case Orig =:= AST of
        true -> io:format("  ✓ MATCH~n~n");
        false -> io:format("  ✗ MISMATCH~n~n")
    end.
