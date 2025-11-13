# Prompt: Extract Consolidated Rust API Documentation for Erlang Rewrite Planning

I'm planning to rewrite the `markdown-rs` Rust parser in Erlang. To create an effective phased implementation plan, I need you to analyze the Rust codebase and extract consolidated API documentation.

## What I'll Provide You

1. The complete Rust source code from the markdown-rs project
2. A list of all source files (already have this)

## What I Need From You

Please analyze the codebase and produce a **consolidated API reference document** that covers:

### 1. Core Data Structures (CRITICAL)

For each major type/struct, document:
- **Name and purpose** (one-line summary)
- **Key fields/properties** and what they represent
- **Lifetime/ownership patterns** (if complex)
- **Relationships to other types** (what it contains, what contains it)

Focus on:
- `Event` - What is an event? What types exist?
- `Token` - How do tokens differ from events?
- `State` - What state is tracked during parsing?
- `Tokenizer` - What does it do? What does it contain?
- `Point`, `Location` - How are positions tracked?
- `ParseState` - Top-level parsing state?
- Any other central types

### 2. Module Architecture (CRITICAL)

Create a **module responsibility matrix**:

| Module | Primary Responsibility | Key Types Exported | Depends On | Used By |
|--------|----------------------|-------------------|------------|---------|
| parser.rs | ... | ... | ... | ... |
| tokenizer.rs | ... | ... | ... | ... |
| state.rs | ... | ... | ... | ... |

Include ALL modules from the file list, organized by category:
- **Core parsing engine** (parser, tokenizer, state)
- **Constructs** (all the construct/*.rs files)
- **Utilities** (util/*.rs files)
- **Output generation** (to_html, to_mdast)
- **Resolution/processing** (resolve, subtokenize)

### 3. Parsing Flow Architecture (CRITICAL)

Describe the high-level algorithm:
1. **Entry point**: What function starts parsing? What does it return?
2. **State machine**: How does the parser progress? (state transitions?)
3. **Token/Event generation**: When are tokens created vs events emitted?
4. **Nesting/recursion**: How are nested structures (lists, blockquotes) handled?
5. **Backtracking**: Is there backtracking? How is it implemented?
6. **Content types**: What are "flow", "text", "string" content types?

### 4. Key Function Signatures (IMPORTANT)

For the most important 10-15 functions, provide:
```rust
// Module: parser.rs
fn parse_something(
    input: &str,
    options: &Options
) -> Result<Vec<Event>, Error>

/// Purpose: One-line description
/// Returns: What it produces
/// Side effects: Does it mutate state?
```

Focus on:
- Main parsing entry points
- State transition functions
- Event/token creation functions
- Utility functions used everywhere

### 5. Construct Pattern (IMPORTANT)

The `construct/` directory has 40+ files. Explain:
- **What is a "construct"?** (Is it a markdown element type?)
- **Common pattern**: Do all constructs follow the same interface/pattern?
- **Example walkthrough**: Pick 2-3 constructs (simple, medium, complex) and show how they work
- **Registration/discovery**: How does the parser know which constructs to try?

### 6. Extension Points (NICE TO HAVE)

- How are GFM extensions handled differently from core?
- How are MDX extensions integrated?
- Configuration/feature flags

### 7. Dependencies Between Modules (CRITICAL)

Create a **dependency graph** showing:
- Which modules can be implemented independently
- Which modules require others to exist first
- Circular dependencies (if any)

Format as layers:
```
Layer 0 (no dependencies): util/char.rs, util/constant.rs
Layer 1 (depends on Layer 0): util/identifier.rs, state.rs
Layer 2 (depends on Layer 1): tokenizer.rs, construct/blank_line.rs
...
```

### 8. Critical Algorithms (NICE TO HAVE)

If there are any particularly clever or complex algorithms, describe them:
- How are character references decoded?
- How is indentation handled?
- How are setext headings distinguished from paragraphs?

## Output Format

Please structure the output as a **single Markdown document** with clear sections:

```markdown
# markdown-rs API Documentation for Erlang Rewrite

## 1. Core Data Structures
[detailed descriptions]

## 2. Module Architecture
[responsibility matrix and descriptions]

## 3. Parsing Flow
[algorithm description]

## 4. Key Function Signatures
[organized by module]

## 5. Construct System
[pattern explanation]

## 6. Module Dependencies
[layered dependency graph]

## 7. Extension Points
[brief notes]

## 8. Critical Algorithms
[any special techniques]

## 9. Implementation Implications for Erlang
[Your insights on what will translate easily vs. what will need adaptation]
```

## What I'll Do With This

I'll use this documentation to:
1. Create a phased implementation plan (Phase 1: core, Phase 2: basic constructs, etc.)
2. Identify which Erlang patterns from my implementation guide apply where
3. Generate specific, focused AI prompts for implementing each phase
4. Understand what can be built/tested independently

## Notes

- **Prioritize breadth over depth** - I need to see the whole picture first
- **Focus on public APIs and contracts** between modules
- **Ignore implementation details** like specific string manipulation code
- **Highlight unusual patterns** that might not translate to Erlang
- **Be explicit about uncertainties** - if you can't tell something from the code, say so

Thank you! This will be invaluable for planning the rewrite.
