# Generate Rd output from a tag

Provide a method for this generic if you want a tag to generate output
in `.Rd` files. See
[`vignette("extending")`](https://roxygen2.r-lib.org/articles/extending.md)
for more details.

## Usage

``` r
roxy_tag_rd(x, base_path, env)
```

## Arguments

- x:

  The tag

- base_path:

  Path to package root directory.

- env:

  Environment in which to evaluate code (if needed)

## Value

Methods must return a
[rd_section](https://roxygen2.r-lib.org/reference/rd_section.md).

## See also

Other extending:
[`load_options()`](https://roxygen2.r-lib.org/reference/load_options.md),
[`parse_package()`](https://roxygen2.r-lib.org/reference/parse_package.md),
[`rd_section()`](https://roxygen2.r-lib.org/reference/rd_section.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/reference/roc_proc_text.md),
[`roclet_find()`](https://roxygen2.r-lib.org/reference/roclet_find.md),
[`roxy_block()`](https://roxygen2.r-lib.org/reference/roxy_block.md),
[`roxy_tag()`](https://roxygen2.r-lib.org/reference/roxy_tag.md),
[`tag_parsers`](https://roxygen2.r-lib.org/reference/tag_parsers.md),
[`tags_list()`](https://roxygen2.r-lib.org/reference/tags_list.md)
