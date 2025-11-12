-module(test_code).
-export([run/0]).

run() ->
    %% Test case: codeblocks separated by blank line
    MD = "\tcode1\n\n\tcode2",
    io:format("Testing separated codeblocks: ~p~n", [MD]),

    Orig = erlmd:conv_original(MD),
    io:format("Original:~n~s~n", [Orig]),

    AST = erlmd:conv_ast(MD),
    io:format("AST:~n~s~n", [AST]),

    case Orig =:= AST of
        true -> io:format("~nMATCH!~n");
        false -> io:format("~nMISMATCH!~n~nExpected: ~p~nGot: ~p~n", [Orig, AST])
    end.
