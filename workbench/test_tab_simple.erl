-module(test_tab_simple).
-export([run/0]).

run() ->
    io:format("~n=== Testing Tab Expansion ===~n~n"),

    % Test the specific failing test from line 190
    test("xyz\tab:c\na", "<p>xyz    ab:c\na</p>", "Tab in middle of text"),

    % Test line 188 which is reported as failing
    test("xyz\r\nab:c\na", "<p>xyz\nab:c\na</p>", "Carriage return + linefeed"),

    io:format("~n=== Tests Complete ===~n").

test(Input, Expected, Description) ->
    io:format("Test: ~s~n", [Description]),
    io:format("  Input: ~w~n", [Input]),
    io:format("  Expected: ~w~n", [Expected]),

    Result = erlmd:conv(Input),
    io:format("  Result:   ~w~n", [Result]),

    case Result =:= Expected of
        true -> io:format("  ✓ PASS~n~n");
        false -> io:format("  ✗ FAIL~n~n")
    end.
