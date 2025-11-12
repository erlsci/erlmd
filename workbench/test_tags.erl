-module(test_tags).
-export([run/0]).

run() ->
    io:format("~n=== Testing Tag Handling ===~n~n"),

    % Test A: Simple tag (from test suite line 14)
    test("<flame on>", "<p><flame on></p>", "Simple tag"),

    % Test B: Tag followed by text
    test("<flame on>\nText", "", "Tag followed by text"),

    % Test C: Tag followed by blank
    test("<flame on>\n\nNext paragraph", "", "Tag followed by blank"),

    % Test D: Tag at end of text
    test("Text\n<flame on>", "", "Tag at end of text"),

    % Test E: Multiple tags
    test("<br>\n<br>\n<br>", "", "Multiple tags"),

    % Test F: Self-closing tag
    test("<br />", "", "Self-closing tag"),

    io:format("~n=== Tag Tests Complete ===~n").

test(Input, Expected, Description) ->
    io:format("Test: ~s~n", [Description]),
    io:format("  Input: ~w~n", [Input]),

    Result = erlmd:conv(Input),
    io:format("  Result: ~s~n", [Result]),

    case Expected of
        "" ->
            io:format("  (No expected value - just showing output)~n~n");
        _ ->
            io:format("  Expected: ~s~n", [Expected]),
            case Result =:= Expected of
                true -> io:format("  ✓ PASS~n~n");
                false -> io:format("  ✗ FAIL~n~n")
            end
    end.
