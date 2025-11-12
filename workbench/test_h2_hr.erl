-module(test_h2_hr).
-export([run/0]).

run() ->
    io:format("~n=== Testing h2_or_hr Handling ===~n~n"),

    % Test A: Setext H2 (normal + dashes)
    test("Header Text\n---", "<h2>Header Text</h2>", "Setext h2: normal + dashes"),

    % Test B: Standalone HR
    test("---", "<hr />", "Standalone hr: just dashes"),

    % Test C: Greedy consumption (h2_or_hr + normal)
    test("---\nFollowing text", "<p>--- Following text</p>", "Greedy: h2_or_hr + normal merges"),

    % Test D: Multiple dashes (longer hr)
    test("-----", "<hr />", "Standalone hr: multiple dashes"),

    % Test E: Setext h2 with longer text
    test("This is a longer header\n---", "<h2>This is a longer header</h2>", "Setext h2: longer text"),

    % Test F: Blockquote followed by dashes
    test("> Quote text\n---", "<h2>> Quote text</h2>", "Blockquote + dashes = h2"),

    % Test G: HR between paragraphs
    test("First paragraph.\n\n---\n\nSecond paragraph.",
         "<p>First paragraph.</p>\n\n<hr />\n\n<p>Second paragraph.</p>",
         "HR between paragraphs"),

    % Test H: Empty line breaks setext h2
    test("Text\n\n---", "<p>Text</p>\n\n<hr />", "Blank line prevents h2"),

    % Test I: h2_or_hr at start of document
    test("---\nText after", "<p>--- Text after</p>", "h2_or_hr at start + text"),

    % Test J: Just hr at start
    test("---", "<hr />", "Just hr at document start"),

    % Test K: Multiple consecutive hrs
    test("---\n---", "<hr />\n\n<hr />", "Multiple consecutive hrs"),

    % Test L: Blockquote + single dash (from test suite)
    test("> a\n-", "<h2>> a</h2>", "Blockquote + single dash"),

    % Test M: Normal + dashes + text (from test suite)
    test("blahblah\n-----\nblah", "<h2>blahblah</h2>\n\n<p>blah</p>", "h2 followed by paragraph"),

    io:format("~n=== h2_or_hr Tests Complete ===~n").

test(Input, Expected, Description) ->
    io:format("Test: ~s~n", [Description]),
    io:format("  Input: ~w~n", [Input]),

    Result = erlmd:conv(Input),
    io:format("  Result:   ~w~n", [Result]),
    io:format("  Expected: ~w~n", [Expected]),

    case Result =:= Expected of
        true -> io:format("  ✓ PASS~n~n");
        false ->
            io:format("  ✗ FAIL~n", []),
            % Show what element type was created
            case {string:str(Expected, "<h2>"), string:str(Result, "<h2>")} of
                {N, 0} when N > 0 ->
                    io:format("    Expected <h2>, got something else~n");
                {0, N} when N > 0 ->
                    io:format("    Got <h2>, expected something else~n");
                _ -> ok
            end,
            case {string:str(Expected, "<hr"), string:str(Result, "<hr")} of
                {N2, 0} when N2 > 0 ->
                    io:format("    Expected <hr>, got something else~n");
                {0, N2} when N2 > 0 ->
                    io:format("    Got <hr>, expected something else~n");
                _ -> ok
            end,
            io:format("~n")
    end.
