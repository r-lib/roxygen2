# Load roxygen2 options

Options can be stored in the `Roxygen` field of the `DESCRIPTION`, or in
`man/roxygen/meta.R`. In either case, the code is parsed and evaluated
in a child of the base environment. Call `roxy_meta_get()` to access
current option values from within tag and roclet methods.

Options in `man/roxygen/meta.R` override those present in `DESCRIPTION`.

## Usage

``` r
load_options(base_path = ".")

roxy_meta_get(key = NULL, default = NULL)
```

## Arguments

- base_path:

  Path to package.

- key:

  Key of the options, e.g. `"packages"`.

- default:

  Default value.

## Possible options

- `roclets` `<character>`: giving names of
  [roclets](https://roxygen2.r-lib.org/dev/reference/roclet.md) to run.
  See
  [`roclet_find()`](https://roxygen2.r-lib.org/dev/reference/roclet_find.md)
  for details.

- `packages` `<character>`: packages to load that implement new tags.

- `load` `<string>`: how to load R code. See
  [load](https://roxygen2.r-lib.org/dev/reference/load.md) for details.

- `old_usage` `<flag>`: use old style usage formatting?

- `markdown` `<flag>`: translate markdown syntax to Rd?

- `r6` `<flag>`: document R6 classes?

- `current_package` `<string>` (read only): name of package being
  documented.

- `rd_family_title` `<list>`: overrides for `@family` titles. See the
  *rd* vignette for details:
  [`vignette("rd", package = "roxygen2")`](https://roxygen2.r-lib.org/dev/articles/rd.md)

- `knitr_chunk_options` `<list>`: default chunk options used for knitr.

- `restrict_image_formats` `<flag>`: if `TRUE` then PDF images are only
  included in the PDF manual, and SVG images are only included in the
  HTML manual. (This only applies to images supplied via markdown.)

## How to set

Either set in `DESCRIPTION`:

    Roxygen: list(markdown = TRUE, load = "installed")

Or if longer, you can put in `/man/roxygen/meta.R`:

    list(
      markdown = TRUE,
      load = "installed"
    )

## See also

Other extending:
[`parse_package()`](https://roxygen2.r-lib.org/dev/reference/parse_package.md),
[`rd_section()`](https://roxygen2.r-lib.org/dev/reference/rd_section.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/dev/reference/roc_proc_text.md),
[`roclet_find()`](https://roxygen2.r-lib.org/dev/reference/roclet_find.md),
[`roxy_block()`](https://roxygen2.r-lib.org/dev/reference/roxy_block.md),
[`roxy_tag()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag.md),
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/dev/reference/roxy_tag_rd.md),
[`tag_parsers`](https://roxygen2.r-lib.org/dev/reference/tag_parsers.md),
[`tags_list()`](https://roxygen2.r-lib.org/dev/reference/tags_list.md)
