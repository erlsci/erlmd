# erlmd

*A new Erlang AST-based Markdown parser*

## About

Versions of this project 1.2.x and early are forks of [erlmarkdown](https://github.com/erlware/erlmarkdown), with 1.1.x being HTML-only, and 1.2.x introducing a Markdown AST. Those releases of the software are licnesed as BSD-2. Versions 1.3 and above share no code in common with earlier versions, instead sharing a design lineage from the [Rust library](https://github.com/wooorm/markdown-rs), adjusted to keep Erlang's strengths in mind. Versions 1.3 and above are licensed as Apache-2.

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {erlmd, "1.3.0"}
]}.
```

## Usage

### Markdown to HTML

```erlang
1> erlmd:conv("# Hello *World*").
"<h1>Hello <em>World</em></h1>"

2> erlmd:conv("- Item 1\n- Item 2").
"<ul>\n<li>Item 1</li>\n<li>Item 2</li>\n</ul>"
```

### Markdown to AST

```erlang
1> AST = erlmd:conv_ast("# Hello *World*").
{document,[{header,1,
                   [{text,"Hello "},{emphasis,[{text,"World"}],42}],
                   atx}]}

2> erlmd:conv_html(AST).
"<h1>Hello <em>World</em></h1>"

% Or use the options API
3> erlmd:conv("# Hello\n\n- Item 1\n- Item 2\n\n", #{format => ast}).
{document,[{header,1,[{text,"Hello"}],atx},
           {blank_line},
           {list,ul,
                 [{list_item,[{paragraph,[{text,"Item 1"}]}],true},
                  {list_item,[{paragraph,[{text,"Item 2"}]}],true}],
                 true}]}
```

## Documentation

- [Release Notes](docs/release-notes.md) - Version history and migration guide
- [Design Documents](docs/design/) - AST architecture details

## License

Apache, Version 2.0