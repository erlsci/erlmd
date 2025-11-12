# erlmd

*Erlang Markdown parser with HTML and AST output (erlmarkdown fork)*

## About

- **Original library** ([erlmarkdown](https://github.com/erlware/erlmarkdown)): Focused on Markdown → HTML conversion, preserved with community improvements in the branch `release/1.1.x`
- **This fork** (erlsci/erlmd): Added support for Markdown → AST (Abstract Syntax Tree) - `release/1.2.x`

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {erlmd, "1.2.0"}
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

Same as the original Markdown (BSD-style). See the [Daring Fireball](http://daringfireball.net/projects/markdown/) project for details.
