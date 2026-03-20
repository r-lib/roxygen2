# Optimization results

## Overall results

End-to-end `roxygenize(load_code = "source")` on the roxygen2 package itself (10 iterations, `devtools::load_all()` in a clean R subprocess):

| Version | min | median | Improvement |
|---------|-----|--------|-------------|
| Before (main) | 863ms | 936ms | — |
| After (all experiments) | 805ms | 839ms | 10.4% faster |

The optimizations are all fast-path short-circuits that avoid expensive work when it's not needed. They have the largest impact on packages where most roxygen tags contain simple text without inline R code, Rd macros, or markdown links (which is the common case).

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

## Experiment 4: Use base `strsplit()` instead of `stringr::str_split()` in `find_generic()`

**Hypothesis:** `stringr::str_split()` has overhead from the stringr/stringi infrastructure. For a simple fixed-character split, base `strsplit()` is faster.

**Change:** Replaced `str_split(name, fixed("."))[[1]]` with `strsplit(name, ".", fixed = TRUE)[[1]]` in `find_generic()`.

**Microbenchmark (per call):**

| Version | min | median |
|---------|-----|--------|
| str_split | 10.5µs | 12.6µs |
| strsplit | 410ns | 533ns |

**Result:** 24x faster per call, saving ~3.4ms per `roxygenize()` run (280 calls × 12µs). All 1105 tests pass.

**Verdict:** SUCCESS — committed.

## Experiment 5: Skip link-reference regex in `get_md_linkrefs()` when no `[` present

**Hypothesis:** `get_md_linkrefs()` runs a complex regex via `str_match_all()` on every tag's text to find markdown link references like `[function()]`. Most tags don't contain `[` at all.

**Data:** Of 240 calls, only 35 contain `[` (205 are plain text without brackets).

**Change:** Added early return when `grepl("[", text, fixed = TRUE)` is FALSE.

**Microbenchmark (per call, simple text):**

| Version | min | median |
|---------|-----|--------|
| Before | 19.4µs | 23.3µs |
| After | 779ns | 1.4µs |

**Result:** 17x faster for plain text, saving ~4.5ms per `roxygenize()` run (205 × 22µs). All 1105 tests pass.

**Verdict:** SUCCESS — committed.
