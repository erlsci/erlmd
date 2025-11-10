%%% AST Type Definitions for erlmd
%%% This file defines the Abstract Syntax Tree structure for parsed Markdown documents

%% Top-level document structure
-record(document, {
    blocks :: [block()]
}).

-type document() :: #document{}.

%% Block-level elements

-record(paragraph, {
    content :: [inline()]
}).

-record(header, {
    level :: 1..6,
    content :: [inline()],
    type :: atx | setext  % track header style for potential round-tripping
}).

-record(blockquote, {
    blocks :: [block()]  % blockquotes can contain multiple block elements
}).

-record(code_block, {
    content :: string(),
    language :: string() | undefined  % for future extension (fenced code blocks)
}).

-record(horizontal_rule, {}).

-record(blank_line, {}).

-type paragraph() :: #paragraph{}.
-type header() :: #header{}.
-type blockquote() :: #blockquote{}.
-type code_block() :: #code_block{}.
-type horizontal_rule() :: #horizontal_rule{}.
-type blank_line() :: #blank_line{}.

-type block() :: paragraph()
               | header()
               | blockquote()
               | code_block()
               | horizontal_rule()
               | md_list()
               | html_block()
               | blank_line().

%% List structures
-record(list, {
    type :: ul | ol,
    items :: [list_item()],
    tight :: boolean()  % tight vs loose list spacing
}).

-record(list_item, {
    content :: [block()]  % list items can contain multiple block elements
}).

%% HTML block (pass-through)
-record(html_block, {
    tag :: string(),
    content :: string(),
    type :: open | close | self_closing
}).

%% Inline elements

-record(text, {
    content :: string()
}).

-record(emphasis, {
    content :: [inline()],
    delimiter :: $* | $_  % track which delimiter was used
}).

-record(strong, {
    content :: [inline()],
    delimiter :: $* | $_
}).

-record(code_span, {
    content :: string(),
    delimiter :: single | double  % ` vs ``
}).

-record(link, {
    text :: [inline()],
    url :: string(),
    title :: string()
}).

-record(image, {
    alt_text :: [inline()],
    url :: string(),
    title :: string()
}).

-record(line_break, {
    type :: soft | hard  % regular newline vs two-space hard break
}).

-record(html_inline, {
    content :: string()
}).

-type md_list() :: #list{}.
-type list_item() :: #list_item{}.
-type html_block() :: #html_block{}.
-type text() :: #text{}.
-type emphasis() :: #emphasis{}.
-type strong() :: #strong{}.
-type code_span() :: #code_span{}.
-type link() :: #link{}.
-type image() :: #image{}.
-type line_break() :: #line_break{}.
-type html_inline() :: #html_inline{}.

-type inline() :: text()
                | emphasis()
                | strong()
                | code_span()
                | link()
                | image()
                | line_break()
                | html_inline().

%% Export types for use in other modules
-export_type([
    document/0,
    block/0,
    inline/0,
    paragraph/0,
    header/0,
    blockquote/0,
    code_block/0,
    horizontal_rule/0,
    md_list/0,
    list_item/0,
    html_block/0,
    blank_line/0,
    text/0,
    emphasis/0,
    strong/0,
    code_span/0,
    link/0,
    image/0,
    line_break/0,
    html_inline/0
]).
