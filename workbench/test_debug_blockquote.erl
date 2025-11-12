-module(test_debug_blockquote).
-export([run/0]).

run() ->
    io:format("~n=== Debugging Blockquote Tokens ===~n~n"),

    % Test input: "> line1  \n> line2"
    Input = "> line1  \n> line2",

    io:format("Input: ~p~n~n", [Input]),

    % Manually walk through the processing
    Lex = erlmd:lex(Input),
    io:format("Lex: ~p~n~n", [Lex]),

    UntypedLines = erlmd:make_lines(Lex),
    io:format("UntypedLines: ~p~n~n", [UntypedLines]),

    {TypedLines, _Refs} = erlmd:type_lines(UntypedLines),
    io:format("TypedLines: ~p~n~n", [TypedLines]),

    io:format("~n=== Analysis Complete ===~n").
