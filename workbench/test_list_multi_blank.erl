-module(test_list_multi_blank).
-export([run/0]).

run() ->
    %% Test case from line 115
    MD = "- a\n\n \n\t\n XX+ b",
    io:format("Testing multi-blank continuation: ~p~n", [MD]),

    Expected = "<ul>\n<li><p>a</p>\n\n<p>XX+ b</p></li>\n</ul>",

    Orig = erlmd:conv_original(MD),
    io:format("~nOriginal:~n~s~n", [Orig]),

    AST = erlmd:conv_ast(MD),
    io:format("AST:~n~s~n", [AST]),

    io:format("~nExpected:~n~s~n", [Expected]),

    case AST =:= Expected of
        true -> io:format("✓ AST MATCHES EXPECTED~n");
        false ->
            io:format("✗ AST MISMATCH~n"),
            io:format("Expected bytes: ~w~n", [Expected]),
            io:format("Got bytes:      ~w~n", [AST])
    end.
