-module(test_blockquote_edge).
-export([run/0]).

run() ->
    io:format("~n=== Testing Blockquote Edge Cases ===~n~n"),

    %% Test with trailing hard break pattern (two spaces + newline)
    test_case("> line1  \n> line2",
              "Hard break before blockquote merge"),

    %% Test with tab before newline
    test_case("> line1\t\n> line2",
              "Tab before blockquote merge"),

    %% Test with regular newline (no hard break)
    test_case("> line1\n> line2",
              "Regular blockquote merge"),

    %% Test blockquote consuming normal with hard break
    test_case("> line1  \nnormal",
              "Hard break then normal consumption"),

    io:format("~n=== Edge Case Testing Complete ===~n").

test_case(Input, Name) ->
    io:format("Test: ~s~n", [Name]),
    io:format("Input: ~p~n", [Input]),

    Orig = erlmd:conv_original(Input),
    AST = erlmd:conv_ast(Input),

    io:format("Original:~n~s~n", [Orig]),
    io:format("AST:~n~s~n", [AST]),

    case Orig =:= AST of
        true -> io:format("✓ MATCH~n~n");
        false ->
            io:format("✗ MISMATCH~n"),
            io:format("Orig bytes: ~w~n", [Orig]),
            io:format("AST bytes:  ~w~n~n", [AST])
    end.
