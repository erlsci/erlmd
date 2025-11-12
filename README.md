# erlmd

Erlang Markdown parser with HTML and AST output.

## About

- **Original library** ([erlmarkdown](https://github.com/erlware/erlmarkdown)): Focused on Markdown → HTML conversion
- **This fork** (erlsci/erlmd): Added support for Markdown → AST (Abstract Syntax Tree)

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
1> erlmd:conv_ast("# Hello").
#document{blocks = [#header{level = 1,
                            content = [#text{content = "Hello"}],
                            type = atx}]}

2> AST = erlmd:conv_ast("# Hello"),
   erlmd_html:render(AST).
"<h1>Hello</h1>"
```

## Documentation

- [Release Notes](docs/release-notes.md) - Version history and migration guide
- [Design Documents](docs/design/) - AST architecture details

## License

Same as the original Markdown (BSD-style). See the [Daring Fireball](http://daringfireball.net/projects/markdown/) project for details.
