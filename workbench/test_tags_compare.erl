-module(test_tags_compare).
-export([run/0]).

run() ->
    io:format("~n=== Comparing Tag Handling: Original vs AST ===~n~n"),

    % Test A: Simple tag (from test suite line 14)
    compare("<flame on>", "Simple tag"),

    % Test B: Tag followed by text on next line
    compare("<flame on>\nText", "Tag followed by text"),

    % Test C: Tag followed by blank then text
    compare("<flame on>\n\nNext", "Tag followed by blank"),

    % Test D: Text then tag
    compare("Text\n<flame on>", "Text then tag"),

    % Test E: Multiple consecutive tags
    compare("<br>\n<br>\n<br>", "Multiple tags"),

    % Test F: Self-closing tag
    compare("<br />", "Self-closing tag"),

    % Test G: Tag alone (end of doc case from original)
    compare("<flame on>", "Tag alone at end"),

    io:format("~n=== Comparison Complete ===~n").

compare(Input, Description) ->
    io:format("Test: ~s~n", [Description]),
    io:format("  Input: ~w~n", [Input]),

    Original = erlmd:conv_original(Input),
    AST = erlmd:conv_ast(Input),

    io:format("  Original: ~s~n", [Original]),
    io:format("  AST:      ~s~n", [AST]),

    case Original =:= AST of
        true -> io:format("  ✓ MATCH~n~n");
        false -> io:format("  ✗ DIFFER~n~n")
    end.
