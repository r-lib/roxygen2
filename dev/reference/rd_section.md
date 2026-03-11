# Construct an `rd_section` object

An `rd_section` represents an Rd command that can appear at the
top-level of an Rd document, like `\name{}`, `\title{}`,
`\description{}`, or `\section{}`.

## Usage

``` r
rd_section(type, value)
```

## Arguments

- type:

  Section type. Stored in `type` field, and in class
  `rd_section_{type}`. To avoid namespace clashes between different
  extensions, this should include the package name.

- value:

  Section data. Only used by
  [`format()`](https://rdrr.io/r/base/format.html) and
  [`merge()`](https://rdrr.io/r/base/merge.html) methods.

## Methods

If provide your own `rd_section` type, you'll also need to define a
`format.rd_section_{type}` method that returns formatted Rd output. You
may also need to provide a `merge.rd_section_{type}` method if two
sections can not be combined with
`rd_section(x$type, c(x$value, y$value))`. See
[`vignette("extending")`](https://roxygen2.r-lib.org/dev/articles/extending.md)
for more details.

## See also

Other extending:
[`load_options()`](https://roxygen2.r-lib.org/dev/reference/load_options.md),
[`parse_package()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/dev/reference/roc_proc_text.md),
[`roclet_find()`](https://roxygen2.r-lib.org/dev/reference/roclet_find.md),
[`roxy_block()`](https://roxygen2.r-lib.org/dev/reference/roxy_block.md),
[`roxy_tag()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md),
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md),
[`tag_parsers`](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md),
[`tags_list()`](https://roxygen2.r-lib.org/dev/reference/tags_list.md)
