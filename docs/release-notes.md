# Release Notes

## Version 1.2.0 (2025) - Major Architecture Update

This release represents a major refactoring of **erlsci/erlmd**, a fork of the erlmarkdown library

### Breaking Changes

- **Module renamed**: `markdown` → `erlmd`
  - Old: `markdown:conv/1`
  - New: `erlmd:conv/1`
- **Application renamed**: `markdown` → `erlmd` (update your rebar.config dependencies)

### New Features

#### AST-Based Architecture
- **New intermediate representation**: Markdown is now parsed into an Abstract Syntax Tree (AST) before HTML generation
- **New module**: `erlmd_ast.erl` - AST builder with full type definitions
- **New module**: `erlmd_html.erl` - Clean HTML renderer from AST
- **Type definitions**: `include/types.hrl` - Complete AST type specifications

#### New API Functions
- `erlmd:conv_ast/1` - Convert Markdown string to AST (returns `#document{}` record)
- `erlmd:conv_html/1` - Convert Markdown string or AST to HTML
- `erlmd:conv/2` - High-level conversion with options: `#{format => html | ast}`
- `erlmd:conv/1` - Convert to HTML using default options (backward compatible)

### Improvements

#### Enhanced CommonMark Compliance (298/298 tests passing)

1. **Phase 1**: Fixed list and blockquote continuation line merging
2. **Phase 2**: Fixed codeblock merging with proper token handling
3. **Phase 3**: Implemented correct hard line breaks (two trailing spaces → `<br />`)
4. **Phase 4**: Proper list tight/loose detection and rendering
5. **Phase 5**: Improved blockquote hard break handling for better CommonMark compliance
6. **Phase 6**: Fixed tab expansion (tabs expand to spaces in text, preserved in code spans)
7. **Phase 7**: Fixed h2_or_hr disambiguation (setext headers vs horizontal rules)
8. **Phase 8**: Improved HTML tag pass-through handling

#### Test Infrastructure
- Migrated from JavaScript-generated tests to native Erlang EUnit tests
- 298 comprehensive test cases covering all Markdown features
- Removed old test generation infrastructure (tests/generate_tests.js, etc.)
- Added extensive test documentation in `test/erlmd_tests.erl`

### Migration Guide (1.1.x → 1.2.0)

**Step 1**: Update your dependency in `rebar.config`:
```erlang
% Old
{deps, [{markdown, "1.1.*"}]}.

% New
{deps, [{erlmd, "1.2.0"}]}.
```

**Step 2**: Update module calls in your code:
```erlang
% Old
HTML = markdown:conv(MarkdownText).

% New
HTML = erlmd:conv(MarkdownText).
```

**Step 3**: If using UTF-8:
```erlang
% Old
HTML = markdown:conv_utf8(Utf8Text).

% New
HTML = erlmd:conv_utf8(Utf8Text).
```

**Step 4**: Using the new AST API:
```erlang
% Get AST
AST = erlmd:conv_ast("# Hello").

% Convert AST to HTML
HTML = erlmd:ast_to_html(AST).

% Or use conv/2 with options
AST = erlmd:conv("# Hello", #{format => ast}).
HTML = erlmd:conv("# Hello", #{format => html}).  % default
```

### Documentation

- Design documents for AST architecture: `docs/design/`
- Phase-by-phase refactoring notes: `workbench/PHASE*_FINDINGS.md`

---

## Version 1.1.12 Production Bug Fix

Bug fix for "<>"

## Version 1.1.11 Production Bug Fix

Bug Fix for tags with a couple more shonky unfixed whitespace bugs

## Version 1.1.10 Production Bug Fix

Shonk Alert! A couple of whitespace bugs remain unfixed

## Version 1.1.9 Production Bug Fix

Wasn't handling special white space inserted by the ***PARSER*** (don't ask!)

## Version 1.1.8 Production Bug Fix

Some blocktag/html errrors fixed

## Version 1.1.7 Production Bug Fix

URl's were not html encoded.

Failing tests of 1.1.5 not addressed in this release

## Version 1.1.6 UTF8 Support

Can now specify a UTF 8 entry point - this addresses the non-breaking space/ascii 160 problem of Version 1.1.3

Failing tests of 1.1.5 not addressed in this release

## Version 1.1.5 (Interim)

Integration of additional tests from markdownsharp:
http://code.google.com/p/markdownsharp/source/browse/trunk/MarkdownSharpTests/#MarkdownSharpTests/testfiles%3Fstate%3Dclosed

17 currently failing which is why this is an interim release

## Version 1.1.4 Production Bug Fix

Fixes 4 sets of bugs:
* you can now put an image inside an href
  ` (eg [![alt](/img/some.png)](http://example.com/dir/)`
* text in ordered and unordered lists now renders correctly
  eg bold and italic etc
* the 'you can't have 2 code segments in a single line' bug is fixed

## Version 1.1.3 Production Bug Fix

Under certain circumstances non-breaking spaces (ascii 160) could wedge
the server.

fuzz.erl rejigged to make for better diagnostics and run with 1,000,000 random
characters against markdown.

***CAVEAT***: having serious doubts as to how erlmarkdown will handle unicode

## Version 1.1.2 Production Bug Fix

A number of failures have been noticed in production of bits of markdown
failing to convert.

A fuzz generator has been added to produce large amounts or random characters
with particular emphasis on characters that are 'structural' in markdown.
50,000 random character strings found 3 or 4 new bugs which have been fixed
in this release.

## Version 1.1.1 Production Bug Fix

Fixed a bug involving underscores/formatting characters inside inline
references and images

## Version 1.1 Production Bug Fix

In the beginning erlmarkdown was an implementation of markdown written to spec
- the spec being the daring-fireball syntax page.

BUT it soon transpired that there were significant differences between *our*
implementation of the spec and that of showdown (from attacklabs.net). Given
that the client-side markdown and the server-side markdown need to work together
we switched the erlang code from being an implementation of the spec to being
a server-side implementation of showdown.

BUT the WMD dialog box produces (as input) code which is 'off spec' (but which
showdown supports), **so**...

Version 1.1 is tested for compatibility with:
* showdown
* the markdown input produced by the WMD dialog box editor

We are tracking the release of WMD **and** showdown from this version on github:
<http://github.com/derobins/wmd/commit/980f68797307d28f0541868c740974cb2eeb1209>

We are no longer tracking the showdown code on attacklabs.net

As a result of this is that the list of ERATTA AND KNOWN BUGS is a bit longer

## Version 1.0 Production Release

This is a major rewrite. There is little point writing servers-side markdown
without a client-side markdown to preview it.

This markdown will now track showdown from Attack Labs as its twin
implementation.

<http://attacklab.net/showdown/>

To that end the test suite has been repurposed to test
compatibility with showdown. The current release of showdown used is V0.9

The directory `/tests` now contains an webpage `index.html` which generates
the tests (ie the file `markdown_tests.erl` in `/src`).

`index.html` loads a javascript file `generate_tests.js` which holds a list of
strings which it generates the tests from (ie testing if markdown produces the
same output as showdown...)

There are a number of places where markdown is not *whitespace compatible* with showdown - you can inspect these by looking for commented out tests in
`generate_tests.js`. There are also a number of showdown bugs or other
inconsistencies which also show up as commented out tests in
`generate_tests.js`.

The biggest single difference is that showdown doesn't escape any html tags
by default whereas markdown has a whitelist of blocklevel tags which it doesn't
escape (see the function `markdown:is_block_tag/1`).

(Other) Known Bugs and Errata:
as per the previous versions (see bottom of document)

There are 261 Unit Tests.

## Version 1.0 RC1.1 First Point Release

Various bug fixes:
* supports short inline URL references of the form 'some text [id] some more'
* fixes bugs with lines containing markdown only like '#', '>', '=' and '-'
* fixes a bug where a terminating '\n\n' would mean each line came back reversed
* fixes various 'atx header' bugs
* fixes bugs where lines start with an emphasis or strong marker (thanks to Tom McNulty)

Unit tests bumped up to 255.

## Version 1.0 RC1 Production Release Candidate

This version comes with 226 Unit Tests and is being released for pre-production testing.

It also has 1 System Test - this document :)
