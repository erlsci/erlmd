-module(test_list_edge).
-export([run/0]).

run() ->
    %% Test case from line 118: should be list + separate paragraph
    MD = "- a\n\n\n\nXX+ b",
    io:format("Testing edge case: ~p~n", [MD]),

    Expected = "<ul>\n<li>a</li>\n</ul>\n\n<p>XX+ b</p>",

    Orig = erlmd:conv_original(MD),
    io:format("~nOriginal:~n~s~n", [Orig]),

    AST = erlmd:conv_ast(MD),
    io:format("AST:~n~s~n", [AST]),

    io:format("~nExpected:~n~s~n", [Expected]),

    case Orig =:= Expected of
        true -> io:format("✓ ORIG MATCHES EXPECTED~n");
        false -> io:format("✗ ORIG MISMATCH~n")
    end,

    case AST =:= Expected of
        true -> io:format("✓ AST MATCHES EXPECTED~n");
        false -> io:format("✗ AST MISMATCH~n")
    end.
