# Optimization plan

## Profiling summary

Profiled `roxygenize(load_code = "source")` on the roxygen2 package itself (5 iterations, ~2.9s total, interval = 5ms). Key bottlenecks by total time:

| Area | % of total | Key functions |
|------|-----------|---------------|
| Tag parsing / markdown | 23% | `parse_tags` → `markdown_if_active` → `markdown` |
| Markdown pass 2 (md→Rd) | 8% | `markdown_pass2` → `mdxml_children_to_rd_top` |
| CLI warnings | 8.5% | `warn_roxy` → `cli::cli_inform` |
| S3 generic detection | 12% | `block_find_object` → `is_s3_generic` / `find_generic` |
| R6 method extraction | 10% | `r6_extract_methods` → `warn_roxy_tag` (76% of its time!) |

## Optimization experiments

### Experiment 1: Cache `is_s3_generic()` results

**Bottleneck:** `find_generic()` calls `is_s3_generic()` in a loop over dot-split pieces of every function name. `is_s3_generic()` does expensive work: `exists()`, `get()`, `tryCatch(getNamespaceName(...))`, `body()`, and recursive `calls_use_method()`. The same generic names (e.g., `print`, `format`, `[`) are tested repeatedly across many functions in a package.

**Proposed fix:** Add a simple environment-based cache inside `is_s3_generic()` keyed by `(name, env)`. Clear the cache at the start of each `roxygenize()` call.

**Risk:** Low — pure function of name + env, env doesn't change during a roxygenize run.

### Experiment 2: Cache `methods::isGeneric()` in `find_generic()`

**Bottleneck:** `find_generic()` calls `methods::isGeneric()` for every function name. This is an S4-method lookup that hits the methods infrastructure.

**Proposed fix:** Cache `methods::isGeneric()` results alongside `is_s3_generic()`.

**Risk:** Low — same reasoning as experiment 1.

### Experiment 3: Replace `cli::cli_inform()` with simpler message in `warn_roxy()`

**Bottleneck:** `warn_roxy()` → `cli::cli_inform()` accounts for ~8.5% of total time. Each call involves `cli::style_hyperlink()`, `cli::format_inline()`, and the full cli formatting pipeline including selector matching. When documenting roxygen2 itself, R6 warnings generate many calls.

**Proposed fix:** Use `cli::cli_inform()` only in interactive sessions or batch the warnings. Alternatively, use `warning()` or `message()` with pre-formatted strings to bypass cli overhead.

**Risk:** Medium — changes user-visible output format. Need to be careful to preserve behavior.

### Experiment 4: Avoid `cli::format_inline()` in `warn_roxy_tag()`

**Bottleneck:** `warn_roxy_tag()` calls `cli::format_inline("{.strong @{tag$tag}} ")` for every warning. This is expensive relative to the simple output it produces.

**Proposed fix:** Replace with `paste0("**@", tag$tag, "** ")` or a simpler string formatting approach, since the bold markup is the only cli feature used.

**Risk:** Low — the formatted tag name is just concatenated into the message anyway.

### Experiment 5: Precompute `internal_f("tools", ".get_internal_S3_generics")()`

**Bottleneck:** In `is_s3_generic()`, for primitive functions, `internal_f("tools", ".get_internal_S3_generics")()` is called each time. This fetches and calls an internal function from the tools package.

**Proposed fix:** Compute this once and cache the result (it never changes within a session).

**Risk:** Very low — the tools package internal generics list is static.

### Experiment 6: Optimize `find_generic()` string splitting

**Bottleneck:** `find_generic()` calls `str_split(name, fixed("."))` for every function name being analyzed, and then `paste0(pieces[seq_len(i)], collapse = ".")` in a loop.

**Proposed fix:** Use `strsplit()` (base R) instead of `str_split()` which has stringr overhead. Pre-compute the candidate generic names more efficiently.

**Risk:** Very low — straightforward string operation replacement.
