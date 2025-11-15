# Phase 6: Basic Inline Constructs - Implementation Guide

**Project**: erlmd (Erlang Markdown Parser)  
**Phase**: 6 of 12  
**Duration**: 1.5 weeks  
**AI Assistant**: Claude Code  
**Date**: November 15, 2025

---

## Table of Contents

1. [Phase Overview](#phase-overview)
2. [Module 1: Character Escape](#module-1-character-escape)
3. [Module 2: Character Reference](#module-2-character-reference)
4. [Module 3: Code Text (Inline Code)](#module-3-code-text-inline-code)
5. [Module 4: Hard Break Escape](#module-4-hard-break-escape)
6. [Module 5: Text Dispatcher Updates](#module-5-text-dispatcher-updates)
7. [Testing Strategy](#testing-strategy)
8. [Integration & Validation](#integration--validation)
9. [Common Pitfalls](#common-pitfalls)
10. [Acceptance Criteria](#acceptance-criteria)

---

## Phase Overview

### Context

**Phase 6** implements the foundational inline constructs that handle special characters and inline code. These are simpler than the complex inline constructs (emphasis, links, images) that come in Phase 7, but they're essential building blocks.

**Dependencies**:
- Phase 1: Core types and character utilities ✅
- Phase 2: Tokenizer framework ✅
- Phase 3: Simple constructs (partial_data, blank_line, etc.) ✅
- Phase 4: Content dispatchers (including `erlmd_cnstr_text`) ✅
- Phase 5: Basic block constructs ✅

**What We're Building**:

1. **Character Escape** (`erlmd_cnstr_character_escape`) - Handles `\*`, `\[`, etc.
2. **Character Reference** (`erlmd_cnstr_character_reference`) - Handles `&amp;`, `&#123;`, `&#xAB;`
3. **Code Text** (`erlmd_cnstr_code_text`) - Handles `` `code` `` and `` ``code`` ``
4. **Hard Break Escape** (`erlmd_cnstr_hard_break_escape`) - Handles `\` followed by newline

### Why These Constructs First?

These inline constructs are chosen for Phase 6 because:

1. **Simplicity**: They have straightforward parsing rules with no complex resolution
2. **Independence**: They don't require delimiter matching or nested resolution
3. **Foundation**: They're needed by more complex constructs (e.g., escapes affect link parsing)
4. **Testing**: Easy to verify with isolated test cases

### Reference Materials

- **Rust Implementation**: 
  - `markdown-rs/src/construct/character_escape.rs`
  - `markdown-rs/src/construct/character_reference.rs`
  - `markdown-rs/src/construct/raw_text.rs` (code text)
  - `markdown-rs/src/construct/hard_break_escape.rs`
  - `markdown-rs/src/construct/text.rs` (text dispatcher)
  - `markdown-rs/src/util/character_reference.rs` (utilities)

- **CommonMark Spec**:
  - Section 2.4: Backslash escapes
  - Section 2.5: Entity and numeric character references
  - Section 6.1: Code spans
  - Section 6.7: Hard line breaks

- **Erlang Patterns**: 
  - Binary pattern matching (Section 2)
  - State machine without gen_statem (Section 3)
  - Tail recursion patterns (Section 5)

### Naming Conventions

**CRITICAL**: Use the naming conventions from `012.3-naming-quick-reference.md`:

- Module prefix: `erlmd_cnstr_` (NOT `erlmd_construct_`)
- Utility prefix: `erlmd_util_`
- Character reference utility: `erlmd_util_char_ref` (NOT `character_reference`)
- Code text: `erlmd_cnstr_code_text` (NOT `raw_text`)

---

## Module 1: Character Escape

### Overview

Character escapes allow backslash-escaping of ASCII punctuation characters. For example, `\*` renders as a literal asterisk instead of starting emphasis.

**Module**: `src/erlmd_cnstr_character_escape.erl`

### CommonMark Specification

From CommonMark 0.31.2, Section 2.4:

> Any ASCII punctuation character may be backslash-escaped.
> Backslashes before other characters are treated as literal backslashes.

**ASCII Punctuation**: `! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ \` { | } ~`

### Implementation Requirements

1. **Entry**: Must detect `\` followed by ASCII punctuation
2. **Event Structure**: Enter CharacterEscape → Marker → Value → Exit
3. **Failure**: Return `nok` if not an escapable character
4. **Performance**: Must preserve match context

### Record/Type Definitions

```erlang
%% No new records needed - uses standard event record from types.hrl
```

### Key Functions

```erlang
-module(erlmd_cnstr_character_escape).
-export([start/1, inside/1]).

-include("types.hrl").

%% @doc Start of character escape.
%% At `\` in the input.
%% Example: "a\*b"
%%            ^
-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc After `\`, checking if next character is ASCII punctuation.
%% Example: "a\*b"
%%             ^
-spec inside(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc Check if a byte is ASCII punctuation (can be escaped).
-spec is_ascii_punctuation(byte()) -> boolean().
```

### Implementation

```erlang
-module(erlmd_cnstr_character_escape).
-export([start/1, inside/1]).

-include("types.hrl").

%% @doc Start of character escape.
%%
%% Detects backslash followed by ASCII punctuation.
%%
%% ```markdown
%% > | a\*b
%%      ^
%% ```
start(T) ->
    case erlmd_tokenizer:current(T) of
        $\\ ->
            T1 = erlmd_tokenizer:enter(T, character_escape),
            T2 = erlmd_tokenizer:enter(T1, character_escape_marker),
            T3 = erlmd_tokenizer:consume(T2),
            T4 = erlmd_tokenizer:exit(T3),
            %% Transition to check next character
            {next, inside, T4};
        _ ->
            {nok, T}
    end.

%% @doc After `\`, at potential punctuation.
%%
%% ```markdown
%% > | a\*b
%%       ^
%% ```
inside(T) ->
    case erlmd_tokenizer:current(T) of
        Byte when is_integer(Byte) ->
            case is_ascii_punctuation(Byte) of
                true ->
                    T1 = erlmd_tokenizer:enter(T, character_escape_value),
                    T2 = erlmd_tokenizer:consume(T1),
                    T3 = erlmd_tokenizer:exit(T2),
                    T4 = erlmd_tokenizer:exit(T3),
                    {ok, T4};
                false ->
                    {nok, T}
            end;
        eof ->
            {nok, T}
    end.

%% @doc Check if byte is ASCII punctuation.
%%
%% ASCII punctuation: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
is_ascii_punctuation(Byte) ->
    (Byte >= $! andalso Byte =< $/) orelse    % ! " # $ % & ' ( ) * + , - . /
    (Byte >= $: andalso Byte =< $@) orelse    % : ; < = > ? @
    (Byte >= $[ andalso Byte =< $`) orelse    % [ \ ] ^ _ `
    (Byte >= ${ andalso Byte =< $~).          % { | } ~
```

### Pattern Notes

**Why This Pattern Works**:

1. **Two-step process**: First function checks for `\`, second checks for punctuation
2. **Event nesting**: Proper Enter/Exit for both the escape and its parts
3. **Tail calls**: Each state transition is a tail call
4. **Match context**: Uses `current()` API instead of binary pattern matching

**State Transition**:
```
start (at \) → enter character_escape 
            → enter marker
            → consume \
            → exit marker
            → {next, inside, T}

inside (at char) → if punctuation:
                      enter value
                      consume char
                      exit value
                      exit character_escape
                      {ok, T}
                   else:
                      {nok, T}
```

### Test Requirements

```erlang
-module(erlmd_cnstr_character_escape_test).
-include_lib("eunit/include/eunit.hrl").

%% Basic functionality
escape_asterisk_test() ->
    Input = <<"\\*">>,
    Events = test_helpers:parse_inline(Input),
    ?assertMatch([
        #event{kind = enter, name = character_escape},
        #event{kind = enter, name = character_escape_marker},
        #event{kind = exit, name = character_escape_marker},
        #event{kind = enter, name = character_escape_value},
        #event{kind = exit, name = character_escape_value},
        #event{kind = exit, name = character_escape}
    ], [E || E <- Events, E#event.name == character_escape orelse 
                          E#event.name == character_escape_marker orelse
                          E#event.name == character_escape_value]).

%% All ASCII punctuation
escape_all_punctuation_test() ->
    Punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~",
    lists:foreach(fun(C) ->
        Input = iolist_to_binary([$\\, C]),
        {ok, HTML} = erlmd:to_html(Input),
        %% Should render the literal character
        ?assert(binary:match(HTML, <<C>>) =/= nomatch)
    end, Punctuation).

%% Not escaped - letter after backslash
not_escape_letter_test() ->
    Input = <<"\\a">>,
    {ok, HTML} = erlmd:to_html(Input),
    %% Should render backslash and 'a'
    ?assertMatch(<<"<p>\\a</p>\n">>, HTML).

%% CommonMark Example 298
commonmark_298_test() ->
    Input = <<"\\!\\\"\\#\\$\\%\\&\\'\\(\\)\\*\\+\\,\\-\\.\\/\\:\\;\\<\\=\\>\\?\\@\\[\\\\\\]\\^\\_\\`\\{\\|\\}\\~">>,
    Expected = <<"<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\\]^_`{|}~</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 299
commonmark_299_test() ->
    Input = <<"\\→\\A\\a\\ \\3\\φ\\«">>,
    Expected = <<"<p>\\→\\A\\a\\ \\3\\φ\\«</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

### Integration Points

The character escape construct is called from the text dispatcher when it encounters `\`:

```erlang
%% In erlmd_cnstr_text.erl
handle_backslash(T) ->
    %% Try character escape first
    case erlmd_tokenizer:attempt(T, character_escape, data) of
        {ok, T1} -> {ok, T1};
        {nok, T1} ->
            %% Try hard break escape next
            case erlmd_tokenizer:attempt(T1, hard_break_escape, data) of
                {ok, T2} -> {ok, T2};
                {nok, T2} -> 
                    %% Just data
                    parse_data(T2)
            end
    end.
```

---

## Module 2: Character Reference

### Overview

Character references allow including special characters using HTML entity syntax. Three forms:
1. **Named**: `&amp;`, `&lt;`, `&AElig;` (HTML entities)
2. **Decimal**: `&#123;` (numeric, base 10)
3. **Hexadecimal**: `&#xAB;` or `&#X1F4A9;` (numeric, base 16)

**Module**: `src/erlmd_cnstr_character_reference.erl`  
**Utility Module**: `src/erlmd_util_char_ref.erl`

### CommonMark Specification

From CommonMark 0.31.2, Section 2.5:

> Valid HTML entity references and numeric character references can be used in place of the corresponding Unicode character.

**Rules**:
- Named references: 1-31 alphanumeric characters, case-sensitive
- Decimal: 1-7 digits
- Hexadecimal: 1-6 hex digits (after `x` or `X`)
- Must end with `;`
- Invalid numeric values replaced with U+FFFD (�)

### Implementation Requirements

1. **Entry**: Detect `&` and determine reference type
2. **State Machine**: Navigate through markers (`#`, `x`) and value
3. **Validation**: Check named references against entity table
4. **Event Structure**: Complex nesting for markers and value
5. **Performance**: Use binary match for entity lookup

### Record/Type Definitions

```erlang
%% No new records - uses tokenizer state fields:
%% - marker: tracks reference type (b'&' | b'#' | b'x')
%% - size: tracks characters consumed in value
```

### Key Functions

```erlang
-module(erlmd_cnstr_character_reference).
-export([start/1, open/1, numeric/1, value/1]).

-include("types.hrl").

%% @doc Start of character reference at &
-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc After &, determining reference type (# for numeric or alphanumeric for named)
-spec open(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc After #, determining decimal vs hexadecimal
-spec numeric(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc Parsing the value (digits or name) until ;
-spec value(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
```

### Utility Module: erlmd_util_char_ref.erl

This module handles entity decoding and validation:

```erlang
-module(erlmd_util_char_ref).
-export([
    decode_named/1,
    decode_numeric/2,
    value_max/1,
    value_test/1
]).

%% @doc Decode named entity (e.g., "amp" -> "&")
%% Returns undefined if not found
-spec decode_named(string()) -> string() | undefined.
decode_named(Name) ->
    %% Lookup in entity table
    case entity_table:lookup(Name) of
        {ok, Value} -> Value;
        error -> undefined
    end.

%% @doc Decode numeric reference
%% Radix is 10 for decimal, 16 for hexadecimal
%% Returns replacement character for invalid values
-spec decode_numeric(string(), 10 | 16) -> string().
decode_numeric(Value, Radix) ->
    case string:to_integer(Value, Radix) of
        {Int, []} when Int >= 0 ->
            case is_valid_codepoint(Int) of
                true -> [Int];
                false -> "\uFFFD"  %% Replacement character
            end;
        _ ->
            "\uFFFD"
    end.

%% @doc Get maximum value length for reference type
-spec value_max(byte()) -> pos_integer().
value_max($&) -> 31;  % Named: max 31 chars
value_max($x) -> 6;   % Hex: max 6 digits  
value_max($#) -> 7.   % Decimal: max 7 digits

%% @doc Get validator function for reference type
-spec value_test(byte()) -> fun((byte()) -> boolean()).
value_test($&) -> fun erlmd_util_char:is_ascii_alphanumeric/1;
value_test($x) -> fun erlmd_util_char:is_ascii_hexdigit/1;
value_test($#) -> fun erlmd_util_char:is_ascii_digit/1.

%% @doc Check if codepoint is valid (not C0 control, C1 control, surrogate, etc.)
-spec is_valid_codepoint(integer()) -> boolean().
is_valid_codepoint(Code) when Code >= 16#0 andalso Code =< 16#08 -> false;  % C0 controls (except HT, LF, FF, CR)
is_valid_codepoint(16#0B) -> false;
is_valid_codepoint(Code) when Code >= 16#0E andalso Code =< 16#1F -> false;
is_valid_codepoint(Code) when Code >= 16#7F andalso Code =< 16#9F -> false;  % DEL and C1 controls
is_valid_codepoint(Code) when Code >= 16#D800 andalso Code =< 16#DFFF -> false;  % Surrogates
is_valid_codepoint(Code) when Code >= 16#FDD0 andalso Code =< 16#FDEF -> false;  % Noncharacters
is_valid_codepoint(Code) when Code >= 16#10FFFF -> false;  % Out of range
is_valid_codepoint(_) -> true.
```

### Entity Table Module: erlmd_util_entity_table.erl

Create a fast lookup table for HTML entities:

```erlang
-module(erlmd_util_entity_table).
-export([lookup/1]).

%% @doc Lookup HTML entity name
%% Returns {ok, Value} or error
-spec lookup(string()) -> {ok, string()} | error.

%% Common entities (fast path)
lookup("amp") -> {ok, "&"};
lookup("lt") -> {ok, "<"};
lookup("gt") -> {ok, ">"};
lookup("quot") -> {ok, "\""};
lookup("apos") -> {ok, "'"};

%% Extended entities - use map for remaining ~2000 entities
lookup(Name) ->
    case get_entity_map() of
        Map when is_map(Map) ->
            maps:get(Name, Map, error);
        _ ->
            error
    end.

%% @doc Get or initialize entity map
get_entity_map() ->
    case persistent_term:get({?MODULE, entity_map}, undefined) of
        undefined ->
            Map = build_entity_map(),
            persistent_term:put({?MODULE, entity_map}, Map),
            Map;
        Map ->
            Map
    end.

%% @doc Build complete HTML5 entity map
%% This should include all 2125 entities from CHARACTER_REFERENCES constant
build_entity_map() ->
    #{
        %% Add all HTML5 entities here
        %% This is a large list - can be generated from markdown-rs constants
        "AElig" => "Æ",
        "Aacute" => "Á",
        %% ... (2000+ more entries)
    }.
```

**Note**: The full entity table should be generated from the Rust `CHARACTER_REFERENCES` constant in `markdown-rs/src/util/constant.rs`.

### Main Construct Implementation

```erlang
-module(erlmd_cnstr_character_reference).
-export([start/1, open/1, numeric/1, value/1]).

-include("types.hrl").

%% @doc Start of character reference.
%%
%% ```markdown
%% > | a&amp;b
%%      ^
%% > | a&#123;b
%%      ^
%% > | a&#x9;b
%%      ^
%% ```
start(T) ->
    case erlmd_tokenizer:current(T) of
        $& ->
            T1 = erlmd_tokenizer:enter(T, character_reference),
            T2 = erlmd_tokenizer:enter(T1, character_reference_marker),
            T3 = erlmd_tokenizer:consume(T2),
            T4 = erlmd_tokenizer:exit(T3),
            {next, open, T4};
        _ ->
            {nok, T}
    end.

%% @doc After &, determining type.
%%
%% ```markdown
%% > | a&amp;b
%%       ^
%% > | a&#123;b
%%       ^
%% ```
open(T) ->
    case erlmd_tokenizer:current(T) of
        $# ->
            T1 = erlmd_tokenizer:enter(T, character_reference_marker_numeric),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            {next, numeric, T3};
        _ ->
            %% Named reference - set marker and start value
            T1 = erlmd_tokenizer:set_marker(T, $&),
            T2 = erlmd_tokenizer:enter(T1, character_reference_value),
            {retry, value, T2}
    end.

%% @doc After #, checking for x (hex) or digit (decimal).
%%
%% ```markdown
%% > | a&#x9;b
%%        ^
%% > | a&#123;b
%%        ^
%% ```
numeric(T) ->
    case erlmd_tokenizer:current(T) of
        C when C =:= $x; C =:= $X ->
            T1 = erlmd_tokenizer:enter(T, character_reference_marker_hexadecimal),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            T4 = erlmd_tokenizer:set_marker(T3, $x),
            T5 = erlmd_tokenizer:enter(T4, character_reference_value),
            {next, value, T5};
        _ ->
            %% Decimal numeric
            T1 = erlmd_tokenizer:set_marker(T, $#),
            T2 = erlmd_tokenizer:enter(T1, character_reference_value),
            {retry, value, T2}
    end.

%% @doc Parsing value until semicolon.
%%
%% ```markdown
%% > | a&amp;b
%%       ^^^
%% > | a&#123;b
%%        ^^^
%% ```
value(T) ->
    Current = erlmd_tokenizer:current(T),
    Marker = erlmd_tokenizer:get_marker(T),
    Size = erlmd_tokenizer:get_size(T),
    
    case Current of
        $; when Size > 0 ->
            %% For named references, validate against entity table
            case Marker of
                $& ->
                    %% Extract value and validate
                    ValueStr = erlmd_tokenizer:slice(T, Size),
                    case erlmd_util_char_ref:decode_named(ValueStr) of
                        undefined ->
                            %% Invalid named reference
                            T1 = clear_state(T),
                            {nok, T1};
                        _ValidEntity ->
                            %% Valid - complete the reference
                            T1 = erlmd_tokenizer:exit(T),  % Exit value
                            T2 = erlmd_tokenizer:enter(T1, character_reference_marker_semi),
                            T3 = erlmd_tokenizer:consume(T2),
                            T4 = erlmd_tokenizer:exit(T3),
                            T5 = erlmd_tokenizer:exit(T4),  % Exit character_reference
                            T6 = clear_state(T5),
                            {ok, T6}
                    end;
                _ ->
                    %% Numeric - don't need to validate here (done in output phase)
                    T1 = erlmd_tokenizer:exit(T),  % Exit value
                    T2 = erlmd_tokenizer:enter(T1, character_reference_marker_semi),
                    T3 = erlmd_tokenizer:consume(T2),
                    T4 = erlmd_tokenizer:exit(T3),
                    T5 = erlmd_tokenizer:exit(T4),  % Exit character_reference
                    T6 = clear_state(T5),
                    {ok, T6}
            end;
        Byte when is_integer(Byte) ->
            MaxSize = erlmd_util_char_ref:value_max(Marker),
            TestFn = erlmd_util_char_ref:value_test(Marker),
            
            if
                Size < MaxSize andalso TestFn(Byte) ->
                    %% Valid character for this reference type
                    T1 = erlmd_tokenizer:increment_size(T),
                    T2 = erlmd_tokenizer:consume(T1),
                    {next, value, T2};
                true ->
                    %% Invalid or too long
                    T1 = clear_state(T),
                    {nok, T1}
            end;
        _ ->
            %% EOF or other character
            T1 = clear_state(T),
            {nok, T1}
    end.

%% Helper to clear tokenizer state
clear_state(T) ->
    T1 = erlmd_tokenizer:set_marker(T, 0),
    erlmd_tokenizer:set_size(T1, 0).
```

### Test Requirements

```erlang
-module(erlmd_cnstr_character_reference_test).
-include_lib("eunit/include/eunit.hrl").

%% Named entities
named_amp_test() ->
    Input = <<"&amp;">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertMatch(<<"<p>&amp;</p>\n">>, HTML).

named_lt_gt_test() ->
    Input = <<"&lt;&gt;">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertMatch(<<"<p>&lt;&gt;</p>\n">>, HTML).

%% Decimal numeric
decimal_simple_test() ->
    Input = <<"&#123;">>,  % {
    {ok, HTML} = erlmd:to_html(Input),
    ?assert(binary:match(HTML, <<"{">>) =/= nomatch).

%% Hexadecimal numeric
hex_simple_test() ->
    Input = <<"&#x7B;">>,  % { in hex
    {ok, HTML} = erlmd:to_html(Input),
    ?assert(binary:match(HTML, <<"{">>) =/= nomatch).

hex_uppercase_x_test() ->
    Input = <<"&#X7B;">>,  % Uppercase X
    {ok, HTML} = erlmd:to_html(Input),
    ?assert(binary:match(HTML, <<"{">>) =/= nomatch).

%% Invalid references
invalid_named_test() ->
    Input = <<"&notanentity;">>,
    {ok, HTML} = erlmd:to_html(Input),
    %% Should render literally
    ?assertMatch(<<"<p>&amp;notanentity;</p>\n">>, HTML).

missing_semicolon_test() ->
    Input = <<"&amp">>,
    {ok, HTML} = erlmd:to_html(Input),
    %% Should render literally
    ?assertMatch(<<"<p>&amp;amp</p>\n">>, HTML).

%% CommonMark Examples
commonmark_311_test() ->
    Input = <<"&nbsp; &amp; &copy; &AElig; &Dcaron;\n&#35; &#1234; &#992; &#0;">>,
    Expected = <<"<p>\u00A0 &amp; © Æ Ď\n# Ӓ Ϡ �</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

commonmark_312_test() ->
    Input = <<"&#X22; &#XD06; &#xcab;">>,
    Expected = <<"<p>&quot; ആ ಫ</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

---

## Module 3: Code Text (Inline Code)

### Overview

Inline code is text wrapped in backticks: `` `code` ``. Multiple backticks can be used: `` ``code with ` backtick`` ``.

**Module**: `src/erlmd_cnstr_code_text.erl`

### CommonMark Specification

From CommonMark 0.31.2, Section 6.1:

> A backtick string is a string of one or more backtick characters (`` ` ``) that is neither preceded nor followed by a backtick.

**Rules**:
1. Opening and closing sequences must have same number of backticks
2. Cannot be preceded/followed by more backticks (greedy)
3. Line endings become spaces in output
4. Leading and trailing spaces trimmed if both present and content has non-space

### Implementation Requirements

1. **Opening Sequence**: Count backticks in opening sequence
2. **Content**: Consume everything until matching closing sequence
3. **Closing Sequence**: Count backticks and match against opening
4. **Line Endings**: Track for output processing
5. **Performance**: Efficient sequence matching

### Record/Type Definitions

```erlang
%% Uses tokenizer state fields:
%% - marker: the backtick character (b'`')
%% - size: opening sequence length
%% - size_b: closing sequence length (during matching)
%% - token_1: code_text
%% - token_2: code_text_sequence
%% - token_3: code_text_data
```

### Key Functions

```erlang
-module(erlmd_cnstr_code_text).
-export([
    start/1,
    sequence_open/1,
    between/1,
    data/1,
    sequence_close/1
]).

-include("types.hrl").

%% @doc Start of code text at backtick
-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc Counting opening backticks
-spec sequence_open(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc Between opening and content/closing
-spec between(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc Inside code text data
-spec data(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc Counting closing backticks
-spec sequence_close(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
```

### Implementation

```erlang
-module(erlmd_cnstr_code_text).
-export([start/1, sequence_open/1, between/1, data/1, sequence_close/1]).

-include("types.hrl").

%% @doc Start of code text.
%%
%% Must check that backtick is not preceded by another backtick
%% (unless preceded by character escape).
%%
%% ```markdown
%% > | `a`
%%     ^
%% > | \`a`
%%      ^
%% ```
start(T) ->
    Current = erlmd_tokenizer:current(T),
    Previous = erlmd_tokenizer:previous(T),
    
    %% Check if current is backtick
    case Current of
        $` ->
            %% Check if not preceded by backtick (or if preceded by escape)
            case Previous =/= $` orelse was_escaped(T) of
                true ->
                    %% Initialize state
                    T1 = erlmd_tokenizer:set_marker(T, $`),
                    T2 = erlmd_tokenizer:set_token_1(T1, code_text),
                    T3 = erlmd_tokenizer:set_token_2(T2, code_text_sequence),
                    T4 = erlmd_tokenizer:set_token_3(T3, code_text_data),
                    T5 = erlmd_tokenizer:enter(T4, code_text),
                    T6 = erlmd_tokenizer:enter(T5, code_text_sequence),
                    {retry, sequence_open, T6};
                false ->
                    {nok, T}
            end;
        _ ->
            {nok, T}
    end.

%% @doc In opening sequence, counting backticks.
%%
%% ```markdown
%% > | `a`
%%     ^
%% > | ``a``
%%     ^^
%% ```
sequence_open(T) ->
    case erlmd_tokenizer:current(T) of
        $` ->
            T1 = erlmd_tokenizer:increment_size(T),
            T2 = erlmd_tokenizer:consume(T1),
            {next, sequence_open, T2};
        _ ->
            %% End of opening sequence
            T1 = erlmd_tokenizer:exit(T),  % Exit code_text_sequence
            {retry, between, T1}
    end.

%% @doc Between opening sequence and content or closing.
%%
%% ```markdown
%% > | `a`
%%      ^^
%% ```
between(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            %% No closing sequence found
            T1 = clear_state(T),
            {nok, T1};
        $\n ->
            %% Line ending - include as line ending token
            T1 = erlmd_tokenizer:enter(T, line_ending),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            {next, between, T3};
        $` ->
            %% Potential closing sequence
            T1 = erlmd_tokenizer:enter(T, code_text_sequence),
            {retry, sequence_close, T1};
        _ ->
            %% Regular data
            T1 = erlmd_tokenizer:enter(T, code_text_data),
            {retry, data, T1}
    end.

%% @doc In data content.
%%
%% ```markdown
%% > | `a`
%%      ^
%% ```
data(T) ->
    Current = erlmd_tokenizer:current(T),
    case Current of
        C when C =:= eof; C =:= $\n; C =:= $` ->
            T1 = erlmd_tokenizer:exit(T),  % Exit code_text_data
            {retry, between, T1};
        _ ->
            T1 = erlmd_tokenizer:consume(T),
            {next, data, T1}
    end.

%% @doc In closing sequence, counting backticks.
%%
%% ```markdown
%% > | `a`
%%       ^
%% > | ``a``
%%        ^^
%% ```
sequence_close(T) ->
    case erlmd_tokenizer:current(T) of
        $` ->
            T1 = erlmd_tokenizer:increment_size_b(T),
            T2 = erlmd_tokenizer:consume(T1),
            {next, sequence_close, T2};
        _ ->
            %% End of sequence - check if matches opening
            T1 = erlmd_tokenizer:exit(T),  % Exit code_text_sequence
            OpenSize = erlmd_tokenizer:get_size(T1),
            CloseSize = erlmd_tokenizer:get_size_b(T1),
            
            if
                OpenSize =:= CloseSize ->
                    %% Match! Complete the code text
                    T2 = erlmd_tokenizer:exit(T1),  % Exit code_text
                    T3 = clear_state(T2),
                    {ok, T3};
                true ->
                    %% Mismatch - treat closing sequence as data
                    T2 = reclassify_sequence_as_data(T1),
                    T3 = erlmd_tokenizer:set_size_b(T2, 0),
                    {retry, between, T3}
            end
    end.

%% @doc Check if previous character was escaped
was_escaped(T) ->
    Events = erlmd_tokenizer:events(T),
    case Events of
        [] -> false;
        [Last | _] -> Last#event.name =:= character_escape
    end.

%% @doc Clear tokenizer state
clear_state(T) ->
    T1 = erlmd_tokenizer:set_marker(T, 0),
    T2 = erlmd_tokenizer:set_size(T1, 0),
    T3 = erlmd_tokenizer:set_size_b(T2, 0),
    T4 = erlmd_tokenizer:set_token_1(T3, data),
    T5 = erlmd_tokenizer:set_token_2(T4, data),
    erlmd_tokenizer:set_token_3(T5, data).

%% @doc Reclassify last two events (sequence enter/exit) as data
reclassify_sequence_as_data(T) ->
    Events = erlmd_tokenizer:events(T),
    Len = length(Events),
    UpdatedEvents = case Len >= 2 of
        true ->
            {PreEvents, [Exit, Enter]} = lists:split(Len - 2, Events),
            PreEvents ++ [
                Enter#event{name = code_text_data},
                Exit#event{name = code_text_data}
            ];
        false ->
            Events
    end,
    erlmd_tokenizer:set_events(T, UpdatedEvents).
```

### Pattern Notes

**Sequence Matching Strategy**:

The key challenge is matching opening and closing backtick sequences:

1. **Opening**: Count backticks, store in `size`
2. **Content**: Consume until backtick
3. **Closing**: Count backticks in `size_b`
4. **Compare**: If `size == size_b`, success. Otherwise, reclassify closing as data and continue

**Greedy Sequences**:

The "not preceded/followed" rule is enforced by:
- `start()`: Checks `previous =/= backtick`
- Output phase: Checks what follows closing sequence

### Test Requirements

```erlang
-module(erlmd_cnstr_code_text_test).
-include_lib("eunit/include/eunit.hrl").

%% Simple cases
simple_code_test() ->
    Input = <<"`code`">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertEqual(<<"<p><code>code</code></p>\n">>, HTML).

double_backtick_test() ->
    Input = <<"``code``">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertEqual(<<"<p><code>code</code></p>\n">>, HTML).

%% Backtick inside code
backtick_inside_test() ->
    Input = <<"`` ` ``">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertEqual(<<"<p><code>`</code></p>\n">>, HTML).

backtick_inside_more_test() ->
    Input = <<"`a``b`">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertEqual(<<"<p><code>a``b</code></p>\n">>, HTML).

%% Line endings become spaces
line_ending_test() ->
    Input = <<"`code\nwith newline`">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertEqual(<<"<p><code>code with newline</code></p>\n">>, HTML).

%% Mismatched sequences
not_code_mismatch_1_test() ->
    Input = <<"``x`">>,
    {ok, HTML} = erlmd:to_html(Input),
    %% Should not be code
    ?assertNot(binary:match(HTML, <<"<code>">>) =/= nomatch).

not_code_mismatch_2_test() ->
    Input = <<"`x``">>,
    {ok, HTML} = erlmd:to_html(Input),
    %% Should not be code
    ?assertNot(binary:match(HTML, <<"<code>">>) =/= nomatch).

%% CommonMark Examples
commonmark_328_test() ->
    Input = <<"`foo`">>,
    Expected = <<"<p><code>foo</code></p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

commonmark_329_test() ->
    Input = <<"`` foo ` bar ``">>,
    Expected = <<"<p><code>foo ` bar</code></p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

commonmark_330_test() ->
    Input = <<"` `` `">>,
    Expected = <<"<p><code>``</code></p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

commonmark_332_test() ->
    Input = <<"`foo``bar``">>,
    Expected = <<"<p><code>foo``bar``</code></p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

---

## Module 4: Hard Break Escape

### Overview

A hard break (escape) is a backslash followed by a line ending, creating a `<br>` in HTML output.

**Module**: `src/erlmd_cnstr_hard_break_escape.erl`

### CommonMark Specification

From CommonMark 0.31.2, Section 6.7:

> A line ending (not in a code span or HTML tag) that is preceded by two or more spaces, or by a backslash, is parsed as a hard line break.

This module handles the backslash variant.

### Implementation Requirements

1. **Entry**: Detect `\` at end of line (followed by `\n`)
2. **Event**: Simple - just enter/exit hard_break_escape
3. **Distinction**: Different from character_escape (which requires punctuation after `\`)

### Key Functions

```erlang
-module(erlmd_cnstr_hard_break_escape).
-export([start/1, after_escape/1]).

-include("types.hrl").

%% @doc Start at backslash
-spec start(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.

%% @doc After backslash, checking for line ending
-spec after_escape(erlmd_tokenizer:tokenizer()) -> {state_result(), erlmd_tokenizer:tokenizer()}.
```

### Implementation

```erlang
-module(erlmd_cnstr_hard_break_escape).
-export([start/1, after_escape/1]).

-include("types.hrl").

%% @doc Start of hard break (escape).
%%
%% ```markdown
%% > | a\
%%      ^
%%   | b
%% ```
start(T) ->
    case erlmd_tokenizer:current(T) of
        $\\ ->
            T1 = erlmd_tokenizer:enter(T, hard_break_escape),
            T2 = erlmd_tokenizer:consume(T1),
            {next, after_escape, T2};
        _ ->
            {nok, T}
    end.

%% @doc After backslash, at line ending.
%%
%% ```markdown
%% > | a\
%%       ^
%%   | b
%% ```
after_escape(T) ->
    case erlmd_tokenizer:current(T) of
        $\n ->
            T1 = erlmd_tokenizer:exit(T),
            {ok, T1};
        _ ->
            {nok, T}
    end.
```

### Pattern Notes

**Interaction with Character Escape**:

The text dispatcher tries constructs in order:
1. First try `character_escape` (checks for punctuation)
2. If that fails, try `hard_break_escape` (checks for newline)
3. This ensures `\*` is escape, but `\` followed by newline is hard break

### Test Requirements

```erlang
-module(erlmd_cnstr_hard_break_escape_test).
-include_lib("eunit/include/eunit.hrl").

%% Basic hard break
simple_hard_break_test() ->
    Input = <<"foo\\\nbar">>,
    {ok, HTML} = erlmd:to_html(Input),
    ?assertEqual(<<"<p>foo<br />\nbar</p>\n">>, HTML).

%% Multiple hard breaks
multiple_hard_breaks_test() ->
    Input = <<"foo\\\nbar\\\nbaz">>,
    {ok, HTML} = erlmd:to_html(Input),
    Expected = <<"<p>foo<br />\nbar<br />\nbaz</p>\n">>,
    ?assertEqual(Expected, HTML).

%% Not a hard break - backslash not followed by newline
not_hard_break_test() ->
    Input = <<"foo\\bar">>,
    {ok, HTML} = erlmd:to_html(Input),
    %% Should render backslash literally
    ?assertEqual(<<"<p>foo\\bar</p>\n">>, HTML).

%% CommonMark Example 654
commonmark_654_test() ->
    Input = <<"foo\\\nbar">>,
    Expected = <<"<p>foo<br />\nbar</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).

%% CommonMark Example 655
commonmark_655_test() ->
    Input = <<"foo\\\n     bar">>,
    Expected = <<"<p>foo<br />\nbar</p>\n">>,
    ?assertEqual(Expected, erlmd:to_html(Input)).
```

---

## Module 5: Text Dispatcher Updates

### Overview

The text content dispatcher (`erlmd_cnstr_text.erl`) needs to be updated to call these new inline constructs.

**Module**: `src/erlmd_cnstr_text.erl` (already exists from Phase 4)

### Updates Required

Add the new constructs to the text dispatcher's decision logic:

```erlang
-module(erlmd_cnstr_text).
-export([start/1, before/1, before_data/1]).

-include("types.hrl").

%% Marker characters that can start inline constructs
-define(MARKERS, [$!, $&, $*, $<, $[, $\\, $], $_, $`, $~]).

%% @doc Start of text content
start(T) ->
    T1 = erlmd_tokenizer:set_markers(T, ?MARKERS),
    {next, before, T1}.

%% @doc Before text content
before(T) ->
    case erlmd_tokenizer:current(T) of
        eof ->
            %% Register resolvers for text
            T1 = erlmd_tokenizer:register_resolver(T, text),
            T2 = erlmd_tokenizer:register_resolver(T1, data),
            {ok, T2};
        
        $` ->
            %% Try code text
            case erlmd_tokenizer:attempt(T, code_text, before_data) of
                {ok, T1} -> {next, before, T1};
                {nok, T1} -> {retry, before_data, T1}
            end;
        
        $& ->
            %% Try character reference
            case erlmd_tokenizer:attempt(T, character_reference, before_data) of
                {ok, T1} -> {next, before, T1};
                {nok, T1} -> {retry, before_data, T1}
            end;
        
        $\\ ->
            %% Try character escape first, then hard break escape
            case erlmd_tokenizer:attempt(T, character_escape, undefined) of
                {ok, T1} -> 
                    {next, before, T1};
                {nok, T1} ->
                    case erlmd_tokenizer:attempt(T1, hard_break_escape, before_data) of
                        {ok, T2} -> {next, before, T2};
                        {nok, T2} -> {retry, before_data, T2}
                    end
            end;
        
        %% Other markers will be added in Phase 7
        _ ->
            {retry, before_data, T}
    end.

%% @doc Before data (fallback)
before_data(T) ->
    case erlmd_tokenizer:attempt(T, data, undefined) of
        {ok, T1} -> {next, before, T1};
        {nok, T1} -> {nok, T1}
    end.
```

### Marker Optimization

The `MARKERS` list helps the tokenizer skip ahead to the next interesting character. Update it to include:

- `` $` `` - code text
- `$&` - character reference
- `$\\` - character escape / hard break

---

## Testing Strategy

### Unit Test Organization

Create test files for each module:

```
test/
├── erlmd_cnstr_character_escape_test.erl
├── erlmd_cnstr_character_reference_test.erl
├── erlmd_cnstr_code_text_test.erl
├── erlmd_cnstr_hard_break_escape_test.erl
├── erlmd_util_char_ref_test.erl
└── erlmd_util_entity_table_test.erl
```

### Test Helper Module

Create or update `test/test_helpers.erl`:

```erlang
-module(test_helpers).
-export([
    parse_inline/1,
    parse/1,
    feed_loop/2
]).

-include("types.hrl").

%% @doc Parse inline content (text)
parse_inline(Binary) ->
    T = erlmd_tokenizer:new(Binary, #{}),
    case erlmd_cnstr_text:start(T) of
        {ok, T1} -> erlmd_tokenizer:events(T1);
        {nok, _} -> []
    end.

%% @doc Full parse (document level)
parse(Binary) ->
    erlmd:parse(Binary).

%% @doc Feed loop for testing state machines
%% Simulates the tokenizer's feed mechanism
feed_loop(State, Tokenizer) ->
    case erlmd_state:call(State, Tokenizer) of
        {ok, T1} -> {ok, T1};
        {nok, T1} -> {nok, T1};
        {next, NextState, T1} -> feed_loop(NextState, T1);
        {retry, RetryState, T1} -> feed_loop(RetryState, T1)
    end.
```

### CommonMark Test Integration

Update the CommonMark test runner to include examples from sections:
- 2.4 (Backslash escapes): Examples 298-302
- 2.5 (Character references): Examples 311-324
- 6.1 (Code spans): Examples 328-349
- 6.7 (Hard breaks): Examples 654-656

```erlang
-module(erlmd_commonmark_phase6_test).
-include_lib("eunit/include/eunit.hrl").

run_all_phase6_tests_test() ->
    Spec = load_commonmark_spec(),
    Phase6Examples = filter_examples(Spec, [
        {298, 302},   % Character escapes
        {311, 324},   % Character references
        {328, 349},   % Code spans
        {654, 656}    % Hard breaks
    ]),
    
    Results = lists:map(fun run_example/1, Phase6Examples),
    Passed = length([R || R <- Results, R =:= pass]),
    Total = length(Results),
    
    io:format("Phase 6 CommonMark: ~p/~p passed~n", [Passed, Total]),
    ?assert(Passed =:= Total).
```

### Property-Based Testing

Add PropEr tests for character reference validation:

```erlang
-module(erlmd_char_ref_proper_test).
-include_lib("proper/include/proper.hrl").

prop_named_entity_roundtrip() ->
    ?FORALL(EntityName, entity_name(),
        begin
            case erlmd_util_char_ref:decode_named(EntityName) of
                undefined -> true;  % Unknown entities are OK
                Decoded ->
                    %% Should be valid unicode
                    is_list(Decoded) orelse is_binary(Decoded)
            end
        end).

prop_numeric_valid_codepoint() ->
    ?FORALL(Code, integer(0, 16#10FFFF),
        begin
            Decimal = integer_to_list(Code),
            Result = erlmd_util_char_ref:decode_numeric(Decimal, 10),
            %% Result should be string
            is_list(Result)
        end).

entity_name() ->
    ?LET(Chars, 
        non_empty(list(oneof(lists:seq($a, $z) ++ lists:seq($A, $Z) ++ lists:seq($0, $9)))),
        lists:sublist(Chars, 31)  % Max 31 chars
    ).
```

---

## Integration & Validation

### Phase 6 Completion Checklist

- [ ] **Module 1**: Character escape implemented and tested
- [ ] **Module 2**: Character reference implemented with entity table
- [ ] **Module 3**: Code text implemented with sequence matching
- [ ] **Module 4**: Hard break escape implemented
- [ ] **Module 5**: Text dispatcher updated
- [ ] **Utility**: Entity table module created
- [ ] **Utility**: Character reference utilities implemented
- [ ] **Tests**: All unit tests passing
- [ ] **Tests**: CommonMark examples 298-302, 311-324, 328-349, 654-656 passing
- [ ] **Tests**: Property-based tests passing
- [ ] **Integration**: All constructs work together
- [ ] **Performance**: No binary optimization warnings
- [ ] **Documentation**: All modules documented with EDoc

### Integration Test

Create an integration test that uses all Phase 6 constructs together:

```erlang
-module(erlmd_phase6_integration_test).
-include_lib("eunit/include/eunit.hrl").

all_inline_constructs_test() ->
    Input = <<"Inline constructs: `code`, &amp;, \\*, and a\\\nhard break.">>,
    {ok, HTML} = erlmd:to_html(Input),
    
    %% Should contain all constructs
    ?assert(binary:match(HTML, <<"<code>code</code>">>) =/= nomatch),
    ?assert(binary:match(HTML, <<"&amp;">>) =/= nomatch),
    ?assert(binary:match(HTML, <<"*">>) =/= nomatch),
    ?assert(binary:match(HTML, <<"<br">>) =/= nomatch).

nested_in_paragraph_test() ->
    Input = <<"Para with `code &amp;` and \\*escape\\*.">>,
    {ok, HTML} = erlmd:to_html(Input),
    
    Expected = <<"<p>Para with <code>code &amp;</code> and *escape*.</p>\n">>,
    ?assertEqual(Expected, HTML).
```

### Performance Validation

Run binary optimization check:

```bash
erlc +bin_opt_info src/erlmd_cnstr_*.erl
```

Expected output: No warnings about match context not being reused.

### Progress Tracking

Update the progress dashboard:

```markdown
## Phase 6 Status: ✅ Complete

| Module | Status | Tests | Notes |
|--------|--------|-------|-------|
| character_escape | ✅ | 8/8 | All CommonMark tests pass |
| character_reference | ✅ | 15/15 | Entity table complete |
| code_text | ✅ | 22/22 | Sequence matching working |
| hard_break_escape | ✅ | 5/5 | Simple implementation |
| text dispatcher | ✅ | 3/3 | All constructs integrated |

**CommonMark Tests**: 50/649 (8%) - Phase 6 complete
**Next**: Phase 7 - Complex Inline (emphasis, links, images)
```

---

## Common Pitfalls

### Pitfall 1: Character Escape vs Hard Break

**Problem**: Both start with `\`, but have different continuations.

**Solution**: Text dispatcher tries `character_escape` first (which checks for punctuation), then `hard_break_escape` (which checks for newline).

```erlang
%% CORRECT order
case erlmd_tokenizer:attempt(T, character_escape, undefined) of
    {ok, T1} -> {ok, T1};
    {nok, T1} ->
        erlmd_tokenizer:attempt(T1, hard_break_escape, before_data)
end
```

### Pitfall 2: Entity Table Performance

**Problem**: Looking up 2000+ entities on every reference is slow.

**Solution**: 
1. Use `persistent_term` for the entity map (one-time load)
2. Inline fast path for common entities (`&amp;`, `&lt;`, `&gt;`)
3. Use binary keys and values for efficiency

```erlang
%% Fast path for common entities
lookup("amp") -> {ok, "&"};
lookup("lt") -> {ok, "<"};
lookup("gt") -> {ok, ">"};
lookup(Name) ->
    %% Fallback to map
    maps:get(Name, get_entity_map(), error).
```

### Pitfall 3: Code Text Sequence Matching

**Problem**: Mismatched sequences should become data, not fail.

**Solution**: When closing sequence doesn't match opening, reclassify the closing sequence events as data and continue parsing.

```erlang
%% In sequence_close/1
if
    OpenSize =:= CloseSize ->
        {ok, complete_code_text(T)};
    true ->
        %% Reclassify as data and continue
        T1 = reclassify_as_data(T),
        {retry, between, T1}
end
```

### Pitfall 4: Line Endings in Code Text

**Problem**: Line endings should be preserved as line ending events, not consumed as data.

**Solution**: Handle `\n` specially in the `between` state:

```erlang
between(T) ->
    case erlmd_tokenizer:current(T) of
        $\n ->
            T1 = erlmd_tokenizer:enter(T, line_ending),
            T2 = erlmd_tokenizer:consume(T1),
            T3 = erlmd_tokenizer:exit(T2),
            {next, between, T3};
        %% ... other cases
    end.
```

### Pitfall 5: Character Reference Value Limits

**Problem**: References can be too long or invalid.

**Solution**: Track size and enforce maximums:

```erlang
%% In value/1
MaxSize = erlmd_util_char_ref:value_max(Marker),
Size = erlmd_tokenizer:get_size(T),

if
    Size < MaxSize andalso is_valid_char(Current, Marker) ->
        %% Continue
        {next, value, consume_and_increment(T)};
    true ->
        %% Too long or invalid
        {nok, clear_state(T)}
end
```

### Pitfall 6: Not Clearing Tokenizer State

**Problem**: State fields (`marker`, `size`, `token_*`) persist between constructs.

**Solution**: Always clear state on success or failure:

```erlang
clear_state(T) ->
    T1 = erlmd_tokenizer:set_marker(T, 0),
    T2 = erlmd_tokenizer:set_size(T1, 0),
    T3 = erlmd_tokenizer:set_size_b(T2, 0),
    erlmd_tokenizer:set_token_1(T3, data).
```

---

## Acceptance Criteria

### Functional Requirements

- [ ] Character escapes work for all ASCII punctuation
- [ ] Character escapes fail gracefully for non-punctuation
- [ ] Named character references resolve correctly for common entities
- [ ] Numeric character references (decimal and hex) work
- [ ] Invalid character references render literally
- [ ] Code text works with single backticks
- [ ] Code text works with multiple backticks
- [ ] Code text sequence matching is correct
- [ ] Line endings in code text are preserved
- [ ] Hard break escapes create `<br>` tags
- [ ] Hard breaks only work with line endings
- [ ] Text dispatcher routes to all constructs correctly
- [ ] Constructs don't interfere with each other

### Performance Requirements

- [ ] No binary optimization warnings
- [ ] All state transitions are tail-recursive
- [ ] Entity table lookup is O(log n) or better
- [ ] Parse rate > 50 MB/s for typical input

### Testing Requirements

- [ ] All unit tests pass
- [ ] CommonMark examples 298-302 pass (escapes)
- [ ] CommonMark examples 311-324 pass (references)
- [ ] CommonMark examples 328-349 pass (code text)
- [ ] CommonMark examples 654-656 pass (hard breaks)
- [ ] Property-based tests pass
- [ ] Integration tests pass
- [ ] No test failures or warnings

### Code Quality Requirements

- [ ] All functions have -spec type declarations
- [ ] All modules have EDoc documentation
- [ ] Code follows Erlang style guide
- [ ] Naming conventions from 012.3 followed consistently
- [ ] No compiler warnings
- [ ] No dialyzer errors
- [ ] Pattern matching in function heads (not bodies)
- [ ] Proper error handling (no crashes on invalid input)

### Documentation Requirements

- [ ] Module documentation explains purpose and usage
- [ ] Function documentation includes examples
- [ ] Complex algorithms have inline comments
- [ ] Test files have descriptive test names
- [ ] README updated with Phase 6 completion

---

## Next Steps

After completing Phase 6:

1. **Verify**: Run full test suite
2. **Benchmark**: Measure parsing performance
3. **Document**: Update implementation plan with actual vs. estimated time
4. **Review**: Check for any technical debt or cleanup needed
5. **Prepare**: Phase 7 planning (complex inline: emphasis, links, images)

**Phase 7 Preview**: The next phase tackles the most complex inline constructs:
- Attention (emphasis and strong) with delimiter resolution
- Links and images with label matching
- Label references and definitions
- These require the resolver algorithm

---

## Appendix A: Event Stream Examples

### Character Escape

Input: `\*`

```erlang
[
    {enter, character_escape, Point},
    {enter, character_escape_marker, Point},
    {exit, character_escape_marker, Point},
    {enter, character_escape_value, Point},
    {exit, character_escape_value, Point},
    {exit, character_escape, Point}
]
```

### Character Reference

Input: `&amp;`

```erlang
[
    {enter, character_reference, Point},
    {enter, character_reference_marker, Point},
    {exit, character_reference_marker, Point},
    {enter, character_reference_value, Point},
    {exit, character_reference_value, Point},
    {enter, character_reference_marker_semi, Point},
    {exit, character_reference_marker_semi, Point},
    {exit, character_reference, Point}
]
```

### Code Text

Input: `` `code` ``

```erlang
[
    {enter, code_text, Point},
    {enter, code_text_sequence, Point},
    {exit, code_text_sequence, Point},
    {enter, code_text_data, Point},
    {exit, code_text_data, Point},
    {enter, code_text_sequence, Point},
    {exit, code_text_sequence, Point},
    {exit, code_text, Point}
]
```

### Hard Break Escape

Input: `\` followed by `\n`

```erlang
[
    {enter, hard_break_escape, Point},
    {exit, hard_break_escape, Point}
]
```

---

## Appendix B: Rust to Erlang Translation Guide

### State Function Pattern

**Rust**:
```rust
pub fn start(tokenizer: &mut Tokenizer) -> State {
    if tokenizer.current == Some(b'\\') {
        tokenizer.enter(Name::CharacterEscape);
        tokenizer.consume();
        State::Next(StateName::CharacterEscapeInside)
    } else {
        State::Nok
    }
}
```

**Erlang**:
```erlang
start(T) ->
    case erlmd_tokenizer:current(T) of
        $\\ ->
            T1 = erlmd_tokenizer:enter(T, character_escape),
            T2 = erlmd_tokenizer:consume(T1),
            {next, inside, T2};
        _ ->
            {nok, T}
    end.
```

### Match Expression Pattern

**Rust**:
```rust
match tokenizer.current {
    Some(b'!' ..= b'/' | b':' ..= b'@') => { /* ... */ }
    _ => State::Nok,
}
```

**Erlang**:
```erlang
case erlmd_tokenizer:current(T) of
    Byte when (Byte >= $! andalso Byte =< $/) orelse
              (Byte >= $: andalso Byte =< $@) ->
        %% ...
    _ ->
        {nok, T}
end
```

### Tokenizer State Fields

**Rust**:
```rust
tokenizer.tokenize_state.marker = b'&';
tokenizer.tokenize_state.size = 0;
```

**Erlang**:
```erlang
T1 = erlmd_tokenizer:set_marker(T, $&),
T2 = erlmd_tokenizer:set_size(T1, 0),
```

---

## Summary

Phase 6 implements the foundational inline constructs that handle special characters and inline code. These constructs are simpler than the complex inline elements (emphasis, links, images) coming in Phase 7, but they're essential building blocks.

**Key Achievements**:
1. ✅ Character escapes for all ASCII punctuation
2. ✅ Character references (named and numeric)
3. ✅ Inline code with sequence matching
4. ✅ Hard break escapes
5. ✅ Complete entity table (2000+ entries)
6. ✅ ~50 CommonMark tests passing

**Estimated Effort**: 1.5 weeks (60-80 hours)

**Next Phase**: Complex Inline Constructs (emphasis, strong, links, images, attention resolution)

Good luck with the implementation! 🚀
