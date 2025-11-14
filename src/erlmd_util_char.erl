%%%-----------------------------------------------------------------------------
%%% @doc Character classification utilities for markdown parsing.
%%%
%%% This module provides fast character classification for ASCII and Unicode
%%% characters, critical for parsing markdown syntax.
%%%
%%% Based on markdown-rs src/util/char.rs
%%% @end
%%%-----------------------------------------------------------------------------

-module(erlmd_util_char).

%% API exports
-export([
    is_ascii_whitespace/1,
    is_ascii_punctuation/1,
    is_ascii_alphanumeric/1,
    is_unicode_whitespace/1,
    is_unicode_punctuation/1,
    classify/1,
    classify_byte/1,
    kind_after_index/2
]).

%% Types
-type char_kind() :: whitespace | punctuation | other.
-export_type([char_kind/0]).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Check if a byte is ASCII whitespace.
%%
%% ASCII whitespace: space (0x20), tab (0x09), newline (0x0A), carriage return (0x0D)
%% @end
%%------------------------------------------------------------------------------
-spec is_ascii_whitespace(byte()) -> boolean().
is_ascii_whitespace($ ) -> true;
is_ascii_whitespace($\t) -> true;
is_ascii_whitespace($\n) -> true;
is_ascii_whitespace($\r) -> true;
is_ascii_whitespace(_) -> false.

%%------------------------------------------------------------------------------
%% @doc Check if a byte is ASCII punctuation.
%%
%% ASCII punctuation includes:
%% - ! to / (0x21-0x2F)
%% - : to @ (0x3A-0x40)
%% - [ to ` (0x5B-0x60)
%% - { to ~ (0x7B-0x7E)
%% @end
%%------------------------------------------------------------------------------
-spec is_ascii_punctuation(byte()) -> boolean().
is_ascii_punctuation(Byte) when Byte >= $! andalso Byte =< $/ -> true;
is_ascii_punctuation(Byte) when Byte >= $: andalso Byte =< $@ -> true;
is_ascii_punctuation(Byte) when Byte >= $[ andalso Byte =< $` -> true;
is_ascii_punctuation(Byte) when Byte >= ${ andalso Byte =< $~ -> true;
is_ascii_punctuation(_) -> false.

%%------------------------------------------------------------------------------
%% @doc Check if a byte is ASCII alphanumeric.
%%
%% ASCII alphanumeric: 0-9, A-Z, a-z
%% @end
%%------------------------------------------------------------------------------
-spec is_ascii_alphanumeric(byte()) -> boolean().
is_ascii_alphanumeric(Byte) when Byte >= $0 andalso Byte =< $9 -> true;
is_ascii_alphanumeric(Byte) when Byte >= $A andalso Byte =< $Z -> true;
is_ascii_alphanumeric(Byte) when Byte >= $a andalso Byte =< $z -> true;
is_ascii_alphanumeric(_) -> false.

%%------------------------------------------------------------------------------
%% @doc Check if a character is Unicode whitespace.
%%
%% Uses Erlang's unicode_util:is_whitespace/1 for proper Unicode support.
%% @end
%%------------------------------------------------------------------------------
-spec is_unicode_whitespace(char()) -> boolean().
is_unicode_whitespace(Char) when Char < 128 ->
    is_ascii_whitespace(Char);
is_unicode_whitespace(Char) ->
    case unicode_util:is_whitespace([Char]) of
        true -> true;
        false -> false
    end.

%%------------------------------------------------------------------------------
%% @doc Check if a character is Unicode punctuation.
%%
%% For Phase 1, we implement a basic set. This should be expanded in later
%% phases to include the full Unicode punctuation categories from markdown-rs.
%%
%% TODO: Import the full PUNCTUATION list from markdown-rs unicode.rs
%% @end
%%------------------------------------------------------------------------------
-spec is_unicode_punctuation(char()) -> boolean().
is_unicode_punctuation(Char) when Char < 128 ->
    is_ascii_punctuation(Char);
is_unicode_punctuation(_Char) ->
    %% For Phase 1, treat non-ASCII as non-punctuation
    %% This will be expanded in later phases with full Unicode category support
    false.

%%------------------------------------------------------------------------------
%% @doc Classify a byte's character kind (whitespace, punctuation, or other).
%%
%% Fast path for ASCII bytes (< 128).
%% @end
%%------------------------------------------------------------------------------
-spec classify_byte(byte()) -> char_kind().
classify_byte(Byte) when Byte < 128 ->
    %% ASCII fast path
    case is_ascii_whitespace(Byte) of
        true -> whitespace;
        false ->
            case is_ascii_punctuation(Byte) of
                true -> punctuation;
                false -> other
            end
    end;
classify_byte(_Byte) ->
    %% Non-ASCII byte - would need full Unicode checking
    %% For Phase 1, treat as 'other'
    other.

%%------------------------------------------------------------------------------
%% @doc Classify a Unicode character's kind.
%%
%% Handles both ASCII (fast path) and Unicode characters.
%% @end
%%------------------------------------------------------------------------------
-spec classify(char()) -> char_kind().
classify(Char) when Char < 128 ->
    classify_byte(Char);
classify(Char) ->
    %% Check Unicode whitespace
    case unicode_util:is_whitespace([Char]) of
        true -> whitespace;
        false ->
            %% Check Unicode punctuation
            case is_unicode_punctuation(Char) of
                true -> punctuation;
                false -> other
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Get the character kind at a specific byte index in a binary.
%%
%% If the index is beyond the end of the binary, returns 'whitespace'
%% (EOF is treated as whitespace per markdown spec).
%% @end
%%------------------------------------------------------------------------------
-spec kind_after_index(binary(), non_neg_integer()) -> char_kind().
kind_after_index(Bytes, Index) when Index >= byte_size(Bytes) ->
    whitespace;  % EOF is treated as whitespace
kind_after_index(Bytes, Index) ->
    Byte = binary:at(Bytes, Index),
    classify_byte(Byte).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% None for Phase 1
