# Roxygen2 Optimizations

## Summary

Profiled `roxygen2::roxygenize(".", load_code = "installed")` using profvis + debrief and implemented 9 optimizations targeting the hottest code paths. The changes reduce processing time by **29%** on roxygen2 itself (686ms → 487ms, 71 R files). The improvements are concentrated in reducing per-call overhead from the standalone purrr implementation (`as_function` dispatch), stringr functions (`arg_match` overhead), and unnecessary XML parsing in the markdown pipeline.

## Optimizations implemented

### 1. Replace `map_chr()` with `vapply()` in hot path functions

**Commit:** `9b8ebc21`

`block_tags()`, `block_find_object()`, `markdown_activate()`, and `parse_description()` all used `map_chr(\(x) x[["tag"]])` which goes through `as_function()` on every call. Replaced with direct ``vapply(x, `[[`, "tag", ...)`` calls. `block_tags()` alone was called 3,292 times per run.

### 2. Environment-based lookup for `special` operators

**Commit:** `ddee09ee`

The `special` vector in `mdxml_code()` (38 elements) was scanned linearly with `%in%` for every inline code element. Replaced with an environment for O(1) hash-based lookup.

### 3. Compute block tag names once in `needs_doc()`

**Commit:** `be2a9646`

`needs_doc()` called `block_has_tags()` twice, each recomputing the tag names vector. Now computes it once and reuses.

### 4. Skip XML parsing in `markdown_evaluate()` for common case

**Commit:** `931e74bb`

`markdown_evaluate()` parsed every tag's text through commonmark XML just to check for inline R code (`` `r ` `` or `` ```{ ``). Added a fast `grepl()` pre-check that skips XML parsing when these markers are absent, which is the vast majority of tags.

### 5. Replace `str_trim()` with `trimws()`

**Commit:** `ac92ea3c`

stringr's `str_trim()` calls `rlang::arg_match()` on every invocation, which performs expensive stack introspection via `caller_fn() → frame_fn() → frame_get()`. Replaced all 30 call sites with base R's `trimws()`, which does the same thing without overhead.

### 6. Replace stringr functions with base R in hot paths

**Commit:** `7839db1d`

`str_split()`, `str_count()`, `str_split_fixed()` from stringr have per-call overhead from `check_lengths()`, `type()`, `switch()`, and `preserve_names_if_possible()`. Replaced with base R `strsplit()`, `grepl()`, `regexpr()`, and `lengths()` in the tag parsing and block creation hot paths.

### 7. Skip XML parsing for plain text in `markdown()`

**Commit:** `77071579`

~55% of `markdown()` calls receive text with no markdown features (no backticks, links, emphasis, headings, or tables). Added a fast path using `grepl()` that bypasses the full commonmark → XML → Rd pipeline for these cases, returning just the `%`-escaped text directly.

### 8. Use `dir.exists()` instead of `file.info()$isdir`

**Commit:** `b1af5dac`

`roclet_output.roclet_rd()` and `roclet_clean.roclet_rd()` used `file.info(paths)$isdir` to filter out directories, which stats all file metadata. Replaced with `dir.exists()`. Also replaced `map_lgl(is.null)` with `vapply()` in `compact()` and `str_detect()` with `grepl()`.

### 9. Replace map_chr/map_lgl with vapply throughout remaining hot paths

**Commit:** `7ba3fad8`

Converted remaining `map_chr()` calls to `vapply()` in markdown XML processing (`mdxml_children_to_rd`, `mdxml_children_to_rd_top`, `mdxml_link_text`, `mdxml_heading`, `mdxml_item`), field formatting, rd-r6-methods, rd-r6-methods-self, rd-template, topo-sort, tag-metadata, and namespace imports filtering. These functions are called hundreds of times during processing and each `map_chr` call goes through `as_function()` overhead.

## What didn't help / wasn't worth pursuing

- **Caching `block_tags()` results on the block**: Blocks are plain lists modified in multiple places (`block_find_object`, `block_replace_tags`, R6 tag mutation). Caching would be fragile and require updating all mutation sites. The `vapply` replacement was sufficient.

- **Batching markdown processing**: Each tag's markdown is processed independently because it needs per-tag error handling and the evaluation environment is shared.

- **Optimizing pkgload/desc calls**: `pkg_name`, `description$new`, and `load_code` are in external packages. With `load_code = "installed"` these are avoided entirely, making the roxygen2-internal optimizations much more visible.

## Architecture notes for future optimization

The remaining processing time (~487ms) breaks down roughly as:

| Area | % of time | Notes |
|------|-----------|-------|
| Parsing (tokenize + parse_tags) | ~28% | Core C++ tokenizer + R tag dispatch |
| Markdown processing | ~22% | commonmark XML + xml2 tree walking |
| Standalone purrr overhead | ~18% | Remaining map/map_chr/map_lgl calls |
| Object detection | ~12% | S3/S4/R6 method detection |
| Rd generation | ~8% | roclet_process.roclet_rd |
| knitr inline code eval | ~7% | Actual R code evaluation in docs |
| Everything else | ~5% | File I/O, setup, namespace |

The biggest remaining single opportunity is the standalone purrr overhead (~18%), which could be addressed by either replacing the standalone file with direct `vapply`/`lapply` calls throughout, or by optimizing `.rlang_purrr_map_mold` to skip `as_function()` when the function is already a function.
