-module(test_tab_codeblock).
-export([run/0]).

run() ->
    io:format("~n=== Testing Tab in Code Blocks ===~n~n"),

    % Test tab in indented code block (should NOT be expanded)
    test("    code\twith\ttabs", "<pre><code>code\twith\ttabs\n</code></pre>\n\n", "Tab in code block"),

    % Test tab in inline code span (should NOT be expanded)
    test("Text with `code\ttab` here", "<p>Text with <code>code\ttab</code> here</p>", "Tab in code span"),

    io:format("~n=== Code Block Tests Complete ===~n").

test(Input, Expected, Description) ->
    io:format("Test: ~s~n", [Description]),
    io:format("  Input: ~w~n", [Input]),

    Result = erlmd:conv(Input),
    io:format("  Result: ~w~n", [Result]),
    io:format("  Expected: ~w~n", [Expected]),

    % Check if tabs are preserved (should be character 9)
    TabsInResult = length([C || C <- Result, C =:= 9]),
    io:format("  Tabs in result: ~p~n", [TabsInResult]),

    case Result =:= Expected of
        true -> io:format("  ✓ PASS~n~n");
        false -> io:format("  ✗ FAIL~n~n")
    end.
