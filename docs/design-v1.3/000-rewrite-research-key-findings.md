# Summary of Key Findings

## 1. Markdown Specifications:

* **CommonMark 0.31.2** is your baseline (650+ tests, formal spec)
* **GFM** is a strict superset of CommonMark (adds tables, strikethrough, task lists, autolinks)
* **Pandoc** has ~90% overlap but conflicts with CommonMark in some areas (list indentation, header requirements)
* **Recommendation**: Start with CommonMark + GFM, add Pandoc later

## 2. The markdown-rs Architecture:

* Uses an **event-based state machine** (non-negotiable for compliance)
* Three-stage pipeline: normalization → tokenization (events) → compilation
* Character-by-character processing with O(n) complexity
* Modular "construct" system for each Markdown element
* Events (Enter/Exit pairs) provide perfect source mapping

## 3. Testing Strategy:

* markdown-rs uses: 650+ CommonMark tests, 1000+ differential tests, fuzz testing, 100% coverage
* **Property-based testing with PropEr is excellent for Markdown parsers**
* Best properties: roundtrip (parse→render→parse), structural invariants, differential testing
* Optimal strategy: Layer spec tests + PBT + fuzzing + differential + golden tests

## 4. Erlang Implementation Patterns:

* OTP 26-28 binary optimizations make Erlang competitive (100+ MB/s possible)
* Use **hand-rolled binary pattern matching** (not gen_statem for core)
* Records internally for speed, maps externally for API
* Match context optimization is critical
* Avoid NIFs unless profiling proves necessity

## 5. Implementation Plan:

* 6-12 months for production quality (1-3 developers)
* 6 phases: Foundation → Basic Infrastructure → Block Structures → Inline Parsing → Resolution → AST Generation
* Hardest parts: Nested lists (2-3 weeks), emphasis parsing (17 rules), HTML blocks
* Critical: Start with simple throwaway spike, then rebuild properly