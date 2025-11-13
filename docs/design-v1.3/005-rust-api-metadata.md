# markdown-rs Project Source Metadata

## File: src/lib.rs

### Module Doc

Public API of `markdown-rs`.

This module exposes primarily [`to_html()`][].
It also exposes [`to_html_with_options()`][] and [`to_mdast()`][].

* [`to_html()`][] — safe way to transform (untrusted?) markdown into HTML
* [`to_html_with_options()`][] — like `to_html` but lets you configure how markdown is turned into HTML, such as allowing dangerous HTML or turning on/off different constructs (GFM, MDX, and the like)
* [`to_mdast()`][] — turn markdown into a syntax tree

**Features:**

* **`default`** — nothing is enabled by default
* **`log`** — enable logging (includes `dep:log`); you can show logs with `RUST_LOG=debug`
* **`serde`** — enable serde to serialize ASTs and configuration (includes `dep:serde`)

### Dependencies

```rust
extern crate alloc;
pub mod mdast;
pub mod message;
pub mod unist;

pub use util::character_reference::{decode_named, decode_numeric};
pub use util::identifier::{id_cont, id_start};
pub use util::sanitize_uri::sanitize;
pub use util::location::Location;
pub use util::line_ending::LineEnding;
pub use util::mdx::{EsmParse as MdxEsmParse, ExpressionKind as MdxExpressionKind, ExpressionParse as MdxExpressionParse, Signal as MdxSignal};
pub use configuration::{CompileOptions, Constructs, Options, ParseOptions};
use alloc::string::String;
```

### Public Functions

/// Turn markdown into HTML.
/// Compiles markdown to HTML according to `CommonMark`.

```rust
pub fn to_html(value: &str) -> String
```

/// Turn markdown into HTML, with configuration.
/// Can error with MDX syntax errors.

```rust
pub fn to_html_with_options(value: &str, options: &Options) -> Result<String, message::Message>
```

/// Turn markdown into a syntax tree.
/// Can error with MDX syntax errors.

```rust
pub fn to_mdast(value: &str, options: &ParseOptions) -> Result<mdast::Node, message::Message>
```

---

## File: src/configuration.rs

### Module Doc

None

### Dependencies

```rust
use crate::util::{
    line_ending::LineEnding,
    mdx::{EsmParse as MdxEsmParse, ExpressionParse as MdxExpressionParse},
};
use alloc::{boxed::Box, fmt, string::String};
```

### Types

```rust
/// Control which constructs are enabled.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Constructs {
    pub attention: bool,
    pub autolink: bool,
    pub block_quote: bool,
    pub character_escape: bool,
    pub character_reference: bool,
    pub code_indented: bool,
    pub code_fenced: bool,
    pub code_text: bool,
    pub definition: bool,
    pub frontmatter: bool,
    pub gfm_autolink_literal: bool,
    pub gfm_footnote_definition: bool,
    pub gfm_label_start_footnote: bool,
    pub gfm_strikethrough: bool,
    pub gfm_table: bool,
    pub gfm_task_list_item: bool,
    pub hard_break_escape: bool,
    pub hard_break_trailing: bool,
    pub heading_atx: bool,
    pub heading_setext: bool,
    pub html_flow: bool,
    pub html_text: bool,
    pub label_start_image: bool,
    pub label_start_link: bool,
    pub label_end: bool,
    pub list_item: bool,
    pub math_flow: bool,
    pub math_text: bool,
    pub mdx_esm: bool,
    pub mdx_expression_flow: bool,
    pub mdx_expression_text: bool,
    pub mdx_jsx_flow: bool,
    pub mdx_jsx_text: bool,
    pub thematic_break: bool,
}

/// Configuration that describes how to compile to HTML.
#[derive(Clone, Debug, Default)]
pub struct CompileOptions {
    pub allow_any_img_src: bool,
    pub allow_dangerous_html: bool,
    pub allow_dangerous_protocol: bool,
    pub default_line_ending: LineEnding,
    pub gfm_footnote_back_label: Option<String>,
    pub gfm_footnote_clobber_prefix: Option<String>,
    pub gfm_footnote_label_attributes: Option<String>,
    pub gfm_footnote_label_tag_name: Option<String>,
    pub gfm_footnote_label: Option<String>,
    pub gfm_task_list_item_checkable: bool,
    pub gfm_tagfilter: bool,
}

/// Configuration that describes how to parse from markdown.
pub struct ParseOptions {
    pub constructs: Constructs,
    pub gfm_strikethrough_single_tilde: bool,
    pub math_text_single_dollar: bool,
    pub mdx_expression_parse: Option<Box<MdxExpressionParse>>,
    pub mdx_esm_parse: Option<Box<MdxEsmParse>>,
}

/// Configuration that describes how to parse from markdown and compile to HTML.
#[derive(Debug, Default)]
pub struct Options {
    pub parse: ParseOptions,
    pub compile: CompileOptions,
}
```

### Public Functions

```rust
impl Constructs {
    /// GFM constructs
    pub fn gfm() -> Self

    /// MDX constructs
    pub fn mdx() -> Self
}

impl CompileOptions {
    /// GFM compile options
    pub fn gfm() -> Self
}

impl ParseOptions {
    /// GFM parse options
    pub fn gfm() -> Self

    /// MDX parse options
    pub fn mdx() -> Self
}

impl Options {
    /// GFM options
    pub fn gfm() -> Self
}
```

---

## File: src/message.rs

### Module Doc

None

### Dependencies

```rust
use crate::unist::{Point, Position};
use alloc::{boxed::Box, fmt, string::String};
```

### Types

```rust
#[derive(Clone, Debug, PartialEq)]
pub struct Message {
    pub place: Option<Box<Place>>,
    pub reason: String,
    pub rule_id: Box<String>,
    pub source: Box<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Place {
    Position(Position),
    Point(Point),
}
```

---

## File: src/unist.rs

### Module Doc

abstract syntax trees: [unist](https://github.com/syntax-tree/unist)

### Dependencies

```rust
use alloc::fmt;
```

### Types

```rust
/// One place in a source file.
#[derive(Clone, Eq, PartialEq)]
pub struct Point {
    /// 1-indexed integer representing a line in a source file.
    pub line: usize,
    /// 1-indexed integer representing a column in a source file.
    pub column: usize,
    /// 0-indexed integer representing a character in a source file.
    pub offset: usize,
}

/// Location of a node in a source file.
#[derive(Clone, Eq, PartialEq)]
pub struct Position {
    /// Represents the place of the first character of the parsed source region.
    pub start: Point,
    /// Represents the place of the first character after the parsed source region.
    pub end: Point,
}
```

### Public Functions

```rust
impl Point {
    pub fn new(line: usize, column: usize, offset: usize) -> Point
}

impl Position {
    pub fn new(start_line: usize, start_column: usize, start_offset: usize, end_line: usize, end_column: usize, end_offset: usize) -> Position
}
```

---

## File: src/mdast.rs

### Module Doc

markdown syntax tree: [mdast](https://github.com/syntax-tree/mdast)

### Dependencies

```rust
use crate::unist::Position;
use alloc::{
    fmt,
    string::{String, ToString},
    vec::Vec,
};
```

### Types

```rust
pub type Stop = (usize, usize);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ReferenceKind {
    Shortcut,
    Collapsed,
    Full,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum AlignKind {
    Left,
    Right,
    Center,
    None,
}

#[derive(Clone, Eq, PartialEq)]
pub enum Node {
    Root(Root),
    Blockquote(Blockquote),
    FootnoteDefinition(FootnoteDefinition),
    MdxJsxFlowElement(MdxJsxFlowElement),
    List(List),
    MdxjsEsm(MdxjsEsm),
    Toml(Toml),
    Yaml(Yaml),
    Break(Break),
    InlineCode(InlineCode),
    InlineMath(InlineMath),
    Delete(Delete),
    Emphasis(Emphasis),
    MdxTextExpression(MdxTextExpression),
    FootnoteReference(FootnoteReference),
    Html(Html),
    Image(Image),
    ImageReference(ImageReference),
    MdxJsxTextElement(MdxJsxTextElement),
    Link(Link),
    LinkReference(LinkReference),
    Strong(Strong),
    Text(Text),
    Code(Code),
    Math(Math),
    MdxFlowExpression(MdxFlowExpression),
    Heading(Heading),
    Table(Table),
    ThematicBreak(ThematicBreak),
    TableRow(TableRow),
    TableCell(TableCell),
    ListItem(ListItem),
    Definition(Definition),
    Paragraph(Paragraph),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AttributeContent {
    Expression(MdxJsxExpressionAttribute),
    Property(MdxJsxAttribute),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct AttributeValueExpression {
    pub value: String,
    pub stops: Vec<Stop>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum AttributeValue {
    Expression(AttributeValueExpression),
    Literal(String),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Root {
    pub children: Vec<Node>,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Paragraph {
    pub children: Vec<Node>,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Heading {
    pub children: Vec<Node>,
    pub position: Option<Position>,
    pub depth: u8,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ThematicBreak {
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Blockquote {
    pub children: Vec<Node>,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct List {
    pub children: Vec<Node>,
    pub position: Option<Position>,
    pub ordered: bool,
    pub start: Option<u32>,
    pub spread: bool,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ListItem {
    pub children: Vec<Node>,
    pub position: Option<Position>,
    pub spread: bool,
    pub checked: Option<bool>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Html {
    pub value: String,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Code {
    pub value: String,
    pub position: Option<Position>,
    pub lang: Option<String>,
    pub meta: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Math {
    pub value: String,
    pub position: Option<Position>,
    pub meta: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Definition {
    pub position: Option<Position>,
    pub url: String,
    pub title: Option<String>,
    pub identifier: String,
    pub label: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Text {
    pub value: String,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Emphasis {
    pub children: Vec<Node>,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Strong {
    pub children: Vec<Node>,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InlineCode {
    pub value: String,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct InlineMath {
    pub value: String,
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Break {
    pub position: Option<Position>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Link {
    pub children: Vec<Node>,
    pub position: Option<Position>,
    pub url: String,
    pub title: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Image {
    pub position: Option<Position>,
    pub alt: String,
    pub url: String,
    pub title: Option<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct LinkReference {
    pub children: Vec<Node>,
    pub position: Option<Position>,
    pub reference_kind: ReferenceKind,
    pub identifier: String,
    pub label: Option<String>,
}
```

### Public Functions

```rust
impl Node {
    pub fn children(&self) -> Option<&Vec<Node>>
    pub fn children_mut(&mut self) -> Option<&mut Vec<Node>>
    pub fn position(&self) -> Option<&Position>
    pub fn position_mut(&mut self) -> Option<&mut Position>
    pub fn position_set(&mut self, position: Option<Position>)
}
```

---

## File: src/parser.rs

### Module Doc

Turn bytes of markdown into events.

### Dependencies

```rust
use crate::event::{Event, Point};
use crate::message;
use crate::state::{Name as StateName, State};
use crate::subtokenize::subtokenize;
use crate::tokenizer::Tokenizer;
use crate::util::location::Location;
use crate::ParseOptions;
use alloc::{string::String, vec, vec::Vec};
```

### Types

```rust
/// Info needed, in all content types, when parsing markdown.
#[derive(Debug)]
pub struct ParseState<'a> {
    pub location: Option<Location>,
    pub options: &'a ParseOptions,
    pub bytes: &'a [u8],
    pub definitions: Vec<String>,
    pub gfm_footnote_definitions: Vec<String>,
}
```

### Public Functions

/// Turn a string of markdown into events.
/// Passes the bytes back so the compiler can access the source.

```rust
pub fn parse<'a>(value: &'a str, options: &'a ParseOptions) -> Result<(Vec<Event>, ParseState<'a>), message::Message>
```

---

## File: src/event.rs

### Module Doc

Semantic labels of things happening.

### Dependencies

```rust
use crate::unist;
use crate::util::constant::TAB_SIZE;
```

### Types

```rust
/// Semantic label of a span.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Name {
    AttentionSequence,
    Autolink,
    AutolinkEmail,
    AutolinkMarker,
    AutolinkProtocol,
    BlankLineEnding,
    BlockQuote,
    BlockQuoteMarker,
    BlockQuotePrefix,
    ByteOrderMark,
    CharacterEscape,
    CharacterEscapeMarker,
    CharacterEscapeValue,
    CharacterReference,
    CharacterReferenceMarker,
    CharacterReferenceMarkerHexadecimal,
    CharacterReferenceMarkerNumeric,
    // ... (many more event names)
}
```

---

## File: src/state.rs

### Module Doc

States of the state machine.

### Dependencies

```rust
use crate::construct;
use crate::message;
use crate::tokenizer::Tokenizer;
```

### Types

```rust
/// Result of a state.
#[derive(Clone, Debug, PartialEq)]
pub enum State {
    Error(message::Message),
    Next(Name),
    Retry(Name),
    Ok,
    Nok,
}

/// Names of states to move to.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Name {
    AttentionStart,
    AttentionInside,
    AutolinkStart,
    // ... (many more state names)
}
```

### Public Functions

```rust
impl State {
    /// Turn a final state into a result.
    pub fn to_result(&self) -> Result<(), message::Message>
}

/// Call the corresponding state for a state name.
pub fn call(tokenizer: &mut Tokenizer, name: Name) -> State
```

---

## File: src/tokenizer.rs

### Module Doc

A tokenizer glues states from the state machine together.

It facilitates everything needed to turn bytes into events with a state machine.
It also enables the logic needed for parsing markdown, such as an [`attempt`][] to try and parse something, which can succeed or, when unsuccessful, revert the attempt.

### Dependencies

```rust
use crate::event::{Content, Event, Kind, Link, Name, Point, VOID_EVENTS};
use crate::message;
use crate::parser::ParseState;
use crate::resolve::{call as call_resolve, Name as ResolveName};
use crate::state::{call, State};
use crate::subtokenize::Subresult;
use crate::util::{constant::TAB_SIZE, edit_map::EditMap};
use alloc::{boxed::Box, string::String, vec, vec::Vec};
```

### Types

```rust
/// Containers found when tokenizing document content.
#[derive(Debug, Eq, PartialEq)]
pub enum Container {
    BlockQuote,
    ListItem,
    GfmFootnoteDefinition,
}

/// Info used to tokenize a container.
#[derive(Debug)]
pub struct ContainerState {
    pub kind: Container,
    pub blank_initial: bool,
    pub size: usize,
}

/// Label start kind.
#[derive(Debug, PartialEq, Eq)]
pub enum LabelKind {
    Image,
    Link,
    GfmFootnote,
    GfmUndefinedFootnote,
}

/// Label start, looking for an end.
#[derive(Debug)]
pub struct LabelStart {
    pub kind: LabelKind,
    pub start: (usize, usize),
    pub inactive: bool,
}

/// Valid label.
#[derive(Debug)]
pub struct Label {
    pub kind: LabelKind,
    pub start: (usize, usize),
    pub end: (usize, usize),
}

/// A lot of shared fields used to tokenize things.
#[derive(Debug)]
pub struct TokenizeState<'a> {
    pub document_child: Option<Box<Tokenizer<'a>>>,
    pub document_child_state: Option<State>,
    pub document_container_stack: Vec<ContainerState>,
    pub document_continued: usize,
    pub document_data_index: Option<usize>,
    pub document_exits: Vec<Option<Vec<Event>>>,
    pub document_lazy_accepting_before: bool,
    pub document_at_first_paragraph_of_list_item: bool,
    pub space_or_tab_eol_content: Option<Content>,
    pub space_or_tab_eol_connect: bool,
    pub space_or_tab_eol_ok: bool,
    pub space_or_tab_connect: bool,
    pub space_or_tab_content: Option<Content>,
    pub space_or_tab_min: usize,
    pub space_or_tab_max: usize,
    pub space_or_tab_size: usize,
    pub space_or_tab_token: Name,
    pub label_starts: Vec<LabelStart>,
    pub label_starts_loose: Vec<LabelStart>,
    pub labels: Vec<Label>,
    pub definitions: Vec<String>,
    pub gfm_footnote_definitions: Vec<String>,
    pub mdx_last_parse_error: Option<(String, String, String)>,
    pub connect: bool,
    pub marker: u8,
    pub marker_b: u8,
    pub markers: &'static [u8],
    pub seen: bool,
    pub size: usize,
    pub size_b: usize,
    pub size_c: usize,
    pub start: usize,
    pub end: usize,
    pub token_1: Name,
    pub token_2: Name,
    pub token_3: Name,
    pub token_4: Name,
    pub token_5: Name,
    pub token_6: Name,
}

/// A tokenizer itself.
#[derive(Debug)]
pub struct Tokenizer<'a> {
    pub current: Option<u8>,
    pub previous: Option<u8>,
    pub point: Point,
    pub events: Vec<Event>,
    pub stack: Vec<Name>,
    pub map: EditMap,
    pub resolvers: Vec<ResolveName>,
    pub parse_state: &'a ParseState<'a>,
    pub tokenize_state: TokenizeState<'a>,
    pub interrupt: bool,
    pub concrete: bool,
    pub pierce: bool,
    pub lazy: bool,
}
```

### Public Functions

```rust
impl<'a> Tokenizer<'a> {
    /// Create a new tokenizer.
    pub fn new(point: Point, parse_state: &'a ParseState) -> Tokenizer<'a>

    /// Register a resolver.
    pub fn register_resolver(&mut self, name: ResolveName)

    /// Register a resolver, before others.
    pub fn register_resolver_before(&mut self, name: ResolveName)

    /// Define a jump between two places.
    pub fn define_skip(&mut self, point: Point)

    /// Consume the current byte.
    pub fn consume(&mut self)

    /// Mark the start of a semantic label.
    pub fn enter(&mut self, name: Name)

    /// Enter with a link.
    pub fn enter_link(&mut self, name: Name, link: Link)

    /// Mark the end of a semantic label.
    pub fn exit(&mut self, name: Name)

    /// Stack an attempt, moving to `ok` on `State::Ok` and `nok` on `State::Nok`, reverting in both cases.
    pub fn check(&mut self, ok: State, nok: State)

    /// Stack an attempt, moving to `ok` on `State::Ok` and `nok` on `State::Nok`, reverting in the latter case.
    pub fn attempt(&mut self, ok: State, nok: State)

    /// Tokenize.
    pub fn push(&mut self, from: (usize, usize), to: (usize, usize), state: State) -> State

    /// Flush.
    pub fn flush(&mut self, state: State, resolve: bool) -> Result<Subresult, message::Message>
}
```

---

## File: src/resolve.rs

### Module Doc

Resolve events.

### Dependencies

```rust
use crate::construct;
use crate::message;
use crate::subtokenize::Subresult;
use crate::tokenizer::Tokenizer;
```

### Types

```rust
/// Names of resolvers.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Name {
    Label,
    Attention,
    GfmTable,
    HeadingAtx,
    HeadingSetext,
    ListItem,
    Content,
    Data,
    String,
    Text,
}
```

### Public Functions

/// Call the corresponding resolver.

```rust
pub fn call(tokenizer: &mut Tokenizer, name: Name) -> Result<Option<Subresult>, message::Message>
```

---

## File: src/subtokenize.rs

### Module Doc

Deal with content in other content.

To deal with content in content, *you* (a `markdown-rs` contributor) add info on events.
Events are a flat list, but they can be connected to each other with a `Link`.
Links must occur on `Enter` events only, which are void.

Links will then be passed through a tokenizer for the corresponding content type by `subtokenize`.

### Dependencies

```rust
use crate::event::{Content, Event, Kind, Name, VOID_EVENTS};
use crate::message;
use crate::parser::ParseState;
use crate::state::{Name as StateName, State};
use crate::tokenizer::Tokenizer;
use crate::util::{edit_map::EditMap, skip};
use alloc::{string::String, vec, vec::Vec};
```

### Types

```rust
#[derive(Debug)]
pub struct Subresult {
    pub done: bool,
    pub gfm_footnote_definitions: Vec<String>,
    pub definitions: Vec<String>,
}
```

### Public Functions

/// Link two `Event`s.

```rust
pub fn link(events: &mut [Event], index: usize)
```

/// Link two arbitrary `Event`s together.

```rust
pub fn link_to(events: &mut [Event], previous: usize, next: usize)
```

/// Parse linked events.
/// Supposed to be called repeatedly, returns `true` when done.

```rust
pub fn subtokenize(events: &mut Vec<Event>, parse_state: &ParseState, filter: Option<&Content>) -> Result<Subresult, message::Message>
```

/// Divide `child_events` over links in `events`.

```rust
pub fn divide_events(map: &mut EditMap, events: &[Event], link_index: usize, child_events: &mut Vec<Event>, acc_before: (usize, usize)) -> (usize, usize)
```

---

## File: src/to_html.rs

### Module Doc

Turn events into a string of HTML.

### Dependencies

```rust
use crate::event::{Event, Kind, Name};
use crate::mdast::AlignKind;
use crate::util::{
    character_reference::decode as decode_character_reference,
    constant::{SAFE_PROTOCOL_HREF, SAFE_PROTOCOL_SRC},
    encode::encode,
    gfm_tagfilter::gfm_tagfilter,
    infer::{gfm_table_align, list_loose},
    normalize_identifier::normalize_identifier,
    sanitize_uri::{sanitize, sanitize_with_protocols},
    skip,
    slice::{Position, Slice},
};
use crate::{CompileOptions, LineEnding};
use alloc::{format, string::{String, ToString}, vec, vec::Vec};
use core::str;
```

### Public Functions

/// Turn events and bytes into a string of HTML.

```rust
pub fn compile(events: &[Event], bytes: &[u8], options: &CompileOptions) -> String
```

---

## File: src/to_mdast.rs

### Module Doc

Turn events into a syntax tree.

### Dependencies

```rust
use crate::event::{Event, Kind, Name};
use crate::mdast::{
    AttributeContent, AttributeValue, AttributeValueExpression, Blockquote, Break, Code,
    Definition, Delete, Emphasis, FootnoteDefinition, FootnoteReference, Heading, Html, Image,
    ImageReference, InlineCode, InlineMath, Link, LinkReference, List, ListItem, Math,
    MdxFlowExpression, MdxJsxAttribute, MdxJsxExpressionAttribute, MdxJsxFlowElement,
    MdxJsxTextElement, MdxTextExpression, MdxjsEsm, Node, Paragraph, ReferenceKind, Root, Strong,
    Table, TableCell, TableRow, Text, ThematicBreak, Toml, Yaml,
};
use crate::message;
use crate::unist::{Point, Position};
use crate::util::{
    character_reference::{decode as decode_character_reference, parse as parse_character_reference},
    infer::{gfm_table_align, list_item_loose, list_loose},
    mdx_collect::{collect, Result as CollectResult},
    normalize_identifier::normalize_identifier,
    slice::{Position as SlicePosition, Slice},
};
use alloc::{boxed::Box, format, string::{String, ToString}, vec, vec::Vec};
use core::str;
```

### Public Functions

/// Turn events and bytes into a syntax tree.

```rust
pub fn compile(events: &[Event], bytes: &[u8]) -> Result<Node, message::Message>
```

---

## File: src/construct/mod.rs

### Module Doc

Constructs found in markdown.

Constructs are grouped by content type. Which content type is allowed somewhere, prescribes which constructs are allowed there.

**Content types:**
* document
* flow
* string
* text

**Constructs in CommonMark:**
* attention (strong, emphasis, extension: GFM strikethrough)
* autolink
* blank line
* block quote
* character escape
* character reference
* code (indented)
* content
* definition
* hard break (escape)
* heading (atx)
* heading (setext)
* html (flow)
* html (text)
* label end
* label start (image)
* label start (link)
* list item
* paragraph
* raw (flow) (code (fenced), extensions: math (flow))
* raw (text) (code (text), extensions: math (text))
* thematic break

**Extension constructs:**
* frontmatter
* gfm autolink literal
* gfm footnote definition
* gfm label start footnote
* gfm table
* gfm task list item check
* mdx esm
* mdx expression (flow)
* mdx expression (text)
* mdx jsx (flow)
* mdx jsx (text)

**Subroutines:**
* bom
* data
* destination
* label
* mdx expression
* mdx jsx
* non lazy continuation
* space or tab
* space or tab, eol
* title
* whitespace

### Dependencies

```rust
pub mod attention;
pub mod autolink;
pub mod blank_line;
pub mod block_quote;
pub mod character_escape;
pub mod character_reference;
pub mod code_indented;
pub mod content;
pub mod definition;
pub mod document;
pub mod flow;
pub mod frontmatter;
pub mod gfm_autolink_literal;
pub mod gfm_footnote_definition;
pub mod gfm_label_start_footnote;
pub mod gfm_table;
pub mod gfm_task_list_item_check;
pub mod hard_break_escape;
pub mod heading_atx;
pub mod heading_setext;
pub mod html_flow;
pub mod html_text;
pub mod label_end;
pub mod label_start_image;
pub mod label_start_link;
pub mod list_item;
pub mod mdx_esm;
pub mod mdx_expression_flow;
pub mod mdx_expression_text;
pub mod mdx_jsx_flow;
pub mod mdx_jsx_text;
pub mod paragraph;
pub mod partial_bom;
pub mod partial_data;
pub mod partial_destination;
pub mod partial_label;
pub mod partial_mdx_expression;
pub mod partial_mdx_jsx;
pub mod partial_non_lazy_continuation;
pub mod partial_space_or_tab;
pub mod partial_space_or_tab_eol;
pub mod partial_title;
pub mod partial_whitespace;
pub mod raw_flow;
pub mod raw_text;
pub mod string;
pub mod text;
pub mod thematic_break;
```

---

## File: src/util/mod.rs

### Module Doc

Utilities used when processing markdown.

### Dependencies

```rust
pub mod char;
pub mod character_reference;
pub mod constant;
pub mod edit_map;
pub mod encode;
pub mod gfm_tagfilter;
pub mod identifier;
pub mod infer;
pub mod line_ending;
pub mod location;
pub mod mdx;
pub mod mdx_collect;
pub mod normalize_identifier;
pub mod sanitize_uri;
pub mod skip;
pub mod slice;
pub mod unicode;
```

---

This comprehensive metadata document covers the main public API surface of the markdown-rs project, including all key structs, enums, and public functions. The document focuses on the publicly-exposed types and functions that users of the library would interact with, while omitting implementation details, private functions, test code, and function bodies as requested.
