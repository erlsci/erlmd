-module(test_blockquotes).
-export([run/0]).

run() ->
    io:format("~n=== Testing Blockquote Cases ===~n~n"),

    %% Test 1: Single blockquote
    test_case(">blah", "<blockquote>\n  <p>blah</p>\n</blockquote>", "Single blockquote"),

    %% Test 2: Blockquote consumes normal line
    test_case("> blah\na", "<blockquote>\n  <p>blah\na</p>\n</blockquote>", "Blockquote consumes normal"),

    %% Test 3: Consecutive blockquotes (line 35)
    test_case("> alice\n> bob\n> chaz",
              "<blockquote>\n  <p>alice\n<br /> bob\n<br /> chaz</p>\n</blockquote>",
              "Consecutive blockquotes"),

    %% Test 4: Consecutive with empty line (line 36)
    test_case("> alice\n> \n> bob\n> chaz",
              "<blockquote>\n  <p>alice\n<br /> \n<br /> bob\n<br /> chaz</p>\n</blockquote>",
              "Consecutive with empty"),

    %% Test 5: Separate blockquote (line 126)
    test_case("bleh\n> blah",
              "<p>bleh</p>\n\n<blockquote>\n  <p>blah</p>\n</blockquote>",
              "Paragraph then blockquote"),

    %% Test 6: No space after > (line 288)
    test_case(">ab:c\na",
              "<blockquote>\n  <p>ab:c\na</p>\n</blockquote>",
              "No space after >"),

    io:format("~n=== Blockquote Testing Complete ===~n").

test_case(Input, Expected, Name) ->
    io:format("Test: ~s~n", [Name]),
    io:format("Input: ~p~n", [Input]),

    Result = erlmd:conv(Input),

    io:format("Result:   ~p~n", [Result]),
    io:format("Expected: ~p~n", [Expected]),

    Match = Result =:= Expected,

    case Match of
        true -> io:format("✓ MATCH~n~n");
        false -> io:format("✗ MISMATCH~n~n")
    end.
