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

## Experiment 2: Skip Rd tag escaping in `escape_rd_for_md()` for text without backslashes

**Hypothesis:** `escape_rd_for_md()` searches for Rd macros (e.g., `\code{}`, `\link{}`) to protect them from markdown parsing. But most roxygen text doesn't contain any Rd macros. A fast `grepl("\\", text, fixed = TRUE)` check can skip all the work.

**Data:** Of 240 calls during `roxygenize()` on roxygen2, only 4 contain backslashes (236 are plain text).

**Change:** Added early return in `escape_rd_for_md()` when text contains no backslash character, attaching the required empty attribute structure for `unescape_rd_for_md()`.

**Microbenchmark (per call, simple text):**

| Version | min | median |
|---------|-----|--------|
| Before | 106µs | 121µs |
| After | 1.1µs | 1.3µs |

**Result:** 92x faster for simple text, saving ~28ms per `roxygenize()` run (236 × 120µs). All 1105 tests pass.

**Verdict:** SUCCESS — committed.

## Experiment 3: Cache `is_s3_generic()` results during `roxygenize()`

**Hypothesis:** `is_s3_generic()` is called 729 times during `roxygenize()` on roxygen2, with only 301 unique names (59% redundancy). Each call involves `exists()`, `get()`, `tryCatch(getNamespaceName(...))`, and potentially recursive `calls_use_method()`. Caching results by name avoids redundant work.

**Change:** Added an environment-based cache that is activated during `roxygenize()` and cleared afterwards. The cache is inactive during direct calls (e.g., in tests) to avoid cross-environment contamination.

**Microbenchmark (729 calls matching a real roxygenize run):**

| Version | min | median |
|---------|-----|--------|
| Before | 16.4ms | 17.3ms |
| After | 13.6ms | 14.3ms |

**Result:** ~17% faster for `is_s3_generic()` calls, saving ~3ms per `roxygenize()` run. Savings scale with package size (more S3 methods = more cache hits). All 1105 tests pass.

**Verdict:** SUCCESS — committed. Small but clean improvement.
