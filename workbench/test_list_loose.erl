-module(test_list_loose).
-export([run/0]).

run() ->
    %% Test case: loose list (blank lines between items)
    MD = "4. a\n\n5. b\n\n6. c",
    io:format("Testing loose list: ~p~n", [MD]),

    Expected = "<ol>\n<li><p>a</p></li>\n<li><p>b</p></li>\n<li><p>c</p></li>\n</ol>",

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
    end,

    case Orig =:= AST of
        true -> io:format("✓ ORIG MATCHES AST~n");
        false -> io:format("✗ ORIG/AST MISMATCH~n")
    end.
