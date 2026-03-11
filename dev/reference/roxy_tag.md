# `roxy_tag` S3 constructor

`roxy_tag()` is the constructor for tag objects. `roxy_tag_warning()` is
superseded by `warn_roxy_tag()`; use to generate a warning that includes
the location of the tag.

## Usage

``` r
roxy_tag(tag, raw, val = NULL, file = NA_character_, line = NA_character_)

roxy_tag_parse(x)

roxy_tag_warning(x, ...)

warn_roxy_tag(tag, message, parent = NULL, envir = parent.frame())
```

## Arguments

- tag:

  Tag name. Arguments starting with `.` are reserved for internal usage.

- raw:

  Raw tag value, a string.

- val:

  Parsed tag value, typically a character vector, but sometimes a list.
  Usually filled in by `tag_parsers`

- file, line:

  Location of the tag

- x:

  A tag

- ...:

  Additional data to be stored in the condition object. If you supply
  condition fields, you should usually provide a `class` argument. You
  may consider prefixing condition fields with the name of your package
  or organisation to prevent name collisions.

- message:

  Warning message

- parent:

  Supply `parent` when you rethrow an error from a condition handler
  (e.g. with
  [`try_fetch()`](https://rlang.r-lib.org/reference/try_fetch.html)).

  - If `parent` is a condition object, a *chained error* is created,
    which is useful when you want to enhance an error with more details,
    while still retaining the original information.

  - If `parent` is `NA`, it indicates an unchained rethrow, which is
    useful when you want to take ownership over an error and rethrow it
    with a custom message that better fits the surrounding context.

    Technically, supplying `NA` lets `abort()` know it is called from a
    condition handler. This helps it create simpler backtraces where the
    condition handling context is hidden by default.

  For more information about error calls, see [Including contextual
  information with error
  chains](https://rlang.r-lib.org/reference/topic-error-chaining.html).

- envir:

  passed to
  [`rlang::warn()`](https://rlang.r-lib.org/reference/abort.html) as
  `.envir`.

## Methods

Define a method for `roxy_tag_parse` to support new tags. See
[tag_parsers](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md)
for more details.

## See also

Other extending:
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md),
[`parse_package()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md),
[`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/dev/reference/roc_proc_text.md),
[`roclet_find()`](https://roxygen2.r-lib.org/dev/reference/roclet_find.md),
[`roxy_block()`](https://roxygen2.r-lib.org/dev/reference/roxy_block.md),
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md),
[`tag_parsers`](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md),
[`tags_list()`](https://roxygen2.r-lib.org/dev/reference/tags_list.md)
