%%%-----------------------------------------------------------------------------
%%% @doc Constants for erlmd markdown parser.
%%%
%%% This header file defines all magic numbers and constants used in parsing.
%%% All constants are defined as macros for compile-time substitution.
%%%
%%% Based on markdown-rs src/util/constant.rs
%%% @end
%%%-----------------------------------------------------------------------------

-ifndef(ERLMD_CONST_HRL).
-define(ERLMD_CONST_HRL, true).

%%%=============================================================================
%%% Core sizes
%%%=============================================================================

%% Number of spaces that represent a tab stop
-define(TAB_SIZE, 4).

%% Number of spaces that represent code indentation
-define(CODE_INDENT_SIZE, 4).

%% Maximum nesting level for ATX headings (# through ######)
-define(HEADING_ATX_OPENING_FENCE_SIZE_MAX, 6).

%% Minimum number of markers for thematic break (---, ***, ___)
-define(THEMATIC_BREAK_MARKER_COUNT_MIN, 3).

%% Minimum sequence length for code fences (``` or ~~~)
-define(CODE_FENCED_SEQUENCE_SIZE_MIN, 3).

%% Sequence size for frontmatter (---)
-define(FRONTMATTER_SEQUENCE_SIZE, 3).

%% Minimum sequence size for math flow ($$)
-define(MATH_FLOW_SEQUENCE_SIZE_MIN, 2).

%% Maximum digits in list item value (e.g., 1234567890.)
-define(LIST_ITEM_VALUE_SIZE_MAX, 10).

%% Minimum trailing spaces for hard line break
-define(HARD_BREAK_PREFIX_SIZE_MIN, 2).

%% Maximum size of link reference
-define(LINK_REFERENCE_SIZE_MAX, 999).

%% Maximum nesting depth for balanced parens in link destinations
-define(RESOURCE_DESTINATION_BALANCE_MAX, 32).

%%%=============================================================================
%%% Character reference limits
%%%=============================================================================

%% Maximum digits in decimal character reference (&#1234567;)
-define(CHARACTER_REFERENCE_DECIMAL_SIZE_MAX, 7).

%% Maximum digits in hexadecimal character reference (&#x1a2b3c;)
-define(CHARACTER_REFERENCE_HEXADECIMAL_SIZE_MAX, 6).

%% Maximum length of named character reference (&CounterClockwiseContourIntegral;)
-define(CHARACTER_REFERENCE_NAMED_SIZE_MAX, 31).

%%%=============================================================================
%%% Autolink limits
%%%=============================================================================

%% Maximum scheme length for autolinks (http://, https://, etc.)
-define(AUTOLINK_SCHEME_SIZE_MAX, 32).

%% Maximum domain label length (DNS limit is 63)
-define(AUTOLINK_DOMAIN_SIZE_MAX, 63).

%%%=============================================================================
%%% HTML limits
%%%=============================================================================

%% Maximum length of GFM tagfilter names
-define(GFM_HTML_TAGFILTER_SIZE_MAX, 9).

%% Maximum length of HTML raw tag names
-define(HTML_RAW_SIZE_MAX, 8).

%%%=============================================================================
%%% HTML tag name lists
%%%=============================================================================

%% Block-level HTML tags
%% From markdown-rs HTML_BLOCK_NAMES
-define(HTML_BLOCK_NAMES, [
    address, article, aside, base, basefont, blockquote, body,
    caption, center, col, colgroup, dd, details, dialog, dir,
    div, dl, dt, fieldset, figcaption, figure, footer, form,
    frame, frameset, h1, h2, h3, h4, h5, h6, head, header,
    hr, html, iframe, legend, li, link, main, menu, menuitem,
    nav, noframes, ol, optgroup, option, p, param, search,
    section, summary, table, tbody, td, tfoot, th, thead,
    title, tr, track, ul
]).

%% Raw HTML tags (content is not parsed)
%% From markdown-rs HTML_RAW_NAMES
-define(HTML_RAW_NAMES, [pre, script, style, textarea]).

%% GFM HTML tag filter names (potentially dangerous tags)
%% From markdown-rs GFM_TAGFILTER_NAMES
-define(GFM_HTML_TAGFILTER_NAMES, [
    iframe, noembed, noframes, plaintext, script,
    style, textarea, title, xmp
]).

%%%=============================================================================
%%% HTML special sequences
%%%=============================================================================

%% CDATA section prefix
-define(HTML_CDATA_PREFIX, <<"CDATA[">>).

%%%=============================================================================
%%% Safe protocols
%%%=============================================================================

%% Safe protocols for href attributes
-define(SAFE_PROTOCOL_HREF, [
    <<"http">>, <<"https">>, <<"irc">>,
    <<"ircs">>, <<"mailto">>, <<"xmpp">>
]).

%% Safe protocols for src attributes (more restrictive)
-define(SAFE_PROTOCOL_SRC, [
    <<"http">>, <<"https">>
]).

-endif. % ERLMD_CONST_HRL
