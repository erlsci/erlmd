# CommonMark Blockquote Hard Break Analysis

## Markdown Input
```
> line1
> line2
```
(Note: two trailing spaces after "line1")

## Reasoning

According to CommonMark principles:

1. **Two trailing spaces create a hard line break** (`<br />`)
2. **Blockquote lines are parsed together** - consecutive `>` lines form a single blockquote
3. **The `>` marker is stripped** from each line
4. **Content is parsed as inline elements**

## Expected Processing

After stripping `>` markers:
```
line1
line2
```

This is equivalent to a paragraph with a hard line break:
```
line1
line2
```

Which should produce:
```html
<blockquote>
  <p>line1<br />
line2</p>
</blockquote>
```

## Key Question

When blockquotes merge consecutive `>` lines, do they:

**Option A**: Keep the hard break from trailing spaces
- Result: `line1<br />\nline2` (single `<br />`)

**Option B**: Add an explicit merge `<br />` between blockquote lines
- Result: `line1<br />\n<br /> line2` (double `<br />`)

**Option C**: Preserve trailing spaces as-is and add merge `<br />`
- Result: `line1  \n<br /> line2` (spaces + single `<br />`)

## Current Implementations

- **Original erlmd**: Option C - `line1  \n<br /> line2`
- **AST erlmd**: Option B - `line1 <br />\n<br /> line2`

## Hypothesis

The original behavior (Option C) seems odd - why would you add `<br />` between lines if the first line already has a hard break?

The most logical behavior seems to be **Option A** - the hard break handles the line break, so no additional `<br />` is needed.

However, the original adds `<br />` between ALL consecutive blockquote lines, regardless of hard breaks.
