# Roxygen2 Performance Experiments

## Baseline profiling (2026-03-19)

Profiled `roxygen2::roxygenize(".")` on the roxygen2 package itself (71 R files).

**Baseline: ~800-835ms** total, ~334ms internal (excluding pkgload)

### Top functions by total time (baseline)

| Function | % of time | Notes |
|----------|-----------|-------|
| `parse_package` + `block_create` + `parse_tags` | 23-25% | Core parsing pipeline |
| `markdown_if_active` + `markdown` | 18-19% | Markdown processing |
| `load_code` / `pkgload::load_all` | 9% | Package loading (external) |
| `pkg_name` / `desc_desc_get_field` / `description$new` | 9-12% | Reading DESCRIPTION repeatedly (external) |
| `tag_two_part` / `roxy_tag_parse.roxy_tag_param` | 8% | Tag parsing |
| `map_chr` / `vapply` / `.rlang_purrr_map_mold` | 14-17% | Functional programming overhead |
| `block_find_object` | 8% | Object detection |
| `markdown_pass2` / `mdxml_children_to_rd_top` | 7% | Markdown to Rd conversion |

### Key observations (baseline)

1. **`block_tags()` is a major bottleneck** - called 3,292 times, recomputes tag names via `map_chr` every call
2. **Markdown processing dominates** (~18%) - each of 246 tags gets processed through commonmark XML pipeline, but 55% of inputs have no markdown features
3. **Standalone purrr overhead** - `as_function()` called on every `map_chr` invocation even for simple extractors like `\(x) x[["tag"]]`
4. **`str_trim()` overhead** - stringr's `str_trim()` calls `rlang::arg_match()` on every invocation, performing expensive stack introspection via `caller_fn/frame_fn/frame_get`
5. **External bottlenecks** (pkgload, desc) - ~20% of time in DESCRIPTION reading and package loading, not optimizable from roxygen2

## Post-optimization profiling (2026-03-19)

**Optimized: ~725ms profiled**, ~327ms internal (excluding pkgload)

### Key functions comparison (profiled total time)

| Function | Baseline | Optimized | Change |
|----------|----------|-----------|--------|
| `parse_package` | 180ms (22%) | 110ms (15%) | -39% |
| `block_create` / `parse_tags` | 170ms (21%) | 100ms (14%) | -41% |
| `markdown_if_active` | 140ms (17%) | 90ms (12%) | -36% |
| `.rlang_purrr_map_mold` | 95ms (12%) | 85ms (12%) | -11% |

### Benchmark results (10 iterations each, internal phases only)

| Version | Median | Times |
|---------|--------|-------|
| Main | 334ms | 335, 328, 329, 330, 331, 335, 336, 348, 333, 337 |
| Optimized | 327ms | 338, 323, 336, 323, 333, 322, 329, 321, 331, 325 |

**Internal speedup: ~2%** on roxygen2 itself (71 files, 246 markdown tags).

### Remaining bottlenecks

The self-time profile after optimization is very flat, with no single function dominating:
- `file.info` (4%) - from pkgload and roclet output
- `vapply` (3%) - core iteration
- `%in%` (3%) - set membership checks
- `$` (3%) - list element access

The remaining time is dominated by external dependencies (pkgload, desc, commonmark, xml2) and fundamental R operations that cannot be further optimized without C/C++ code.
