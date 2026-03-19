# Roxygen2 Performance Experiments

## Baseline profiling (2026-03-19)

Profiled `roxygen2::roxygenize(".", load_code = "installed")` on the roxygen2 package itself (71 R files). Using `load_code = "installed"` removes pkgload overhead and focuses on roxygen2's own processing.

**Baseline median: 686ms** (10 iterations)

### Top functions by total time (baseline)

| Function | % of time | Notes |
|----------|-----------|-------|
| `parse_package` + `block_create` + `parse_tags` | 23-25% | Core parsing pipeline |
| `markdown_if_active` + `markdown` | 19-21% | Markdown processing |
| `.rlang_purrr_map_mold` / `map_chr` | 15-20% | Standalone purrr overhead |
| `help` / `dev_help` / `dev_topic_find` | 15-18% | pkgload help lookup |
| `tag_two_part` / `roxy_tag_parse.roxy_tag_param` | 8% | Tag parsing |
| `block_find_object` | 11% | Object detection |
| `markdown_pass2` / `mdxml_children_to_rd_top` | 10% | Markdown to Rd conversion |

### Key observations (baseline)

1. **`block_tags()` is a major bottleneck** - called 3,292 times, recomputes tag names via `map_chr` every call
2. **Markdown processing dominates** (~19%) - each of 246 tags gets processed through commonmark XML pipeline, but 55% of inputs have no markdown features
3. **Standalone purrr overhead is massive** (~20%) - 3,021 total `map`/`map_chr`/`map_lgl` calls, each going through `as_function()` even for simple extractors like `\(x) x[["tag"]]`
4. **`str_trim()` overhead** - stringr's `str_trim()` calls `rlang::arg_match()` on every invocation, performing expensive stack introspection via `caller_fn/frame_fn/frame_get`
5. **stringr functions have per-call overhead** from `check_lengths()`, `type()`, `switch()`, `preserve_names_if_possible()` - noticeable for hot-path calls like `str_split`, `str_count`

## Post-optimization profiling (2026-03-19)

**Optimized median: 487ms** (10 iterations)

**Speedup: 29%** (686ms → 487ms)

### Key functions comparison (profiled total time)

| Function | Baseline | Optimized | Change |
|----------|----------|-----------|--------|
| `parse_package` | 155ms (25%) | 130ms (29%) | -16% |
| `block_create` / `parse_tags` | 140ms (23%) | 120ms (27%) | -14% |
| `markdown_if_active` | 120ms (20%) | 100ms (22%) | -17% |
| `.rlang_purrr_map_mold` | 125ms (20%) | 80ms (18%) | -36% |
| `map_chr` | 95ms (15%) | 50ms (11%) | -47% |

### Remaining bottlenecks

The self-time profile after optimization is very flat, with no single function dominating:
- `sub` (6%) - from `trimws()` base R implementation
- `length` (4%) - basic R operations
- `%in%` (3%) - set membership checks
- `paste0` (2%) - string concatenation
- `vapply` (2%) - core iteration

The remaining time is spread across many small operations: XML parsing (commonmark + xml2), knitr evaluation for inline R code, string manipulation, and fundamental R dispatch overhead.
