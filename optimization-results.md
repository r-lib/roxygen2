# Optimization results

## Experiment 1: Skip XML parsing in `markdown_evaluate()` for text without inline R code

**Hypothesis:** `markdown_evaluate()` parses every tag's text to XML to check for inline R code (`\`r ...\``), but the vast majority of tags don't contain any. A fast `grepl()` pre-check can skip the expensive XML parsing.

**Data:** Of 240 calls to `markdown_evaluate()` during `roxygenize()` on roxygen2, only 2 contain inline R code (238 are pure text).

**Change:** Added early return in `markdown_evaluate()` when text doesn't contain `` `r `` or `` ```{ `` patterns.

**Microbenchmark (240 typical calls, no inline R code):**

| Version | min | median |
|---------|-----|--------|
| Before | 13.23ms | 13.47ms |
| After | 1.01ms | 1.18ms |

**Result:** 11x faster for `markdown_evaluate()`, saving ~12ms per `roxygenize()` run. All 1105 tests pass.

**Verdict:** SUCCESS — committed.
