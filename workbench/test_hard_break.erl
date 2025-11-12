-module(test_hard_break).
-export([run/0]).

run() ->
    %% Test case 1: Two spaces + newline
    test_case("Two spaces + LF", "Hey Ho  \nLets Go", "<p>Hey Ho <br />\nLets Go</p>"),

    %% Test case 2: Tab + newline
    test_case("Tab + LF", "Hey Ho\t\nLets Go", "<p>Hey Ho <br />\nLets Go</p>"),

    %% Test case 3: Multiple hard breaks
    test_case("Multiple", "    \nHey\nHo!  \nLets Go", "<p>Hey\nHo! <br />\nLets Go</p>").

test_case(Name, Input, Expected) ->
    io:format("~n=== ~s ===~n", [Name]),
    io:format("Input: ~p~n", [Input]),

    Orig = erlmd:conv_original(Input),
    AST = erlmd:conv_ast(Input),

    io:format("Expected: ~p~n", [Expected]),
    io:format("Original: ~p~n", [Orig]),
    io:format("AST:      ~p~n", [AST]),

    case AST =:= Expected of
        true -> io:format("✓ AST MATCHES EXPECTED~n");
        false -> io:format("✗ AST MISMATCH~n")
    end,

    case Orig =:= AST of
        true -> io:format("✓ ORIG MATCHES AST~n");
        false -> io:format("✗ ORIG/AST MISMATCH~n")
    end.
