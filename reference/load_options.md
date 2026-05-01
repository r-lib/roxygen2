# Load roxygen2 options

Options can be stored in `DESCRIPTION` using `Config/roxygen2/` fields,
or in `man/roxygen/meta.R`. Call `roxy_meta_get()` to access current
option values from within tag and roclet methods.

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
  [roclets](https://roxygen2.r-lib.org/reference/roclet.md) to run. See
  [`roclet_find()`](https://roxygen2.r-lib.org/reference/roclet_find.md)
  for details.

- `packages` `<character>`: packages to load that implement new tags.

- `load` `<string>`: how to load R code. See
  [load](https://roxygen2.r-lib.org/reference/load.md) for details.

- `old_usage` `<flag>`: use old style usage formatting?

- `markdown` `<flag>`: translate markdown syntax to Rd?

- `r6` `<flag>`: document R6 classes?

- `current_package` `<string>` (read only): name of package being
  documented.

- `rd_family_title` `<list>`: overrides for `@family` titles. See the
  *rd-functions* vignette for details:
  [`vignette("rd-functions")`](https://roxygen2.r-lib.org/articles/rd-functions.md)

- `knitr_chunk_options` `<list>`: default chunk options used for knitr.

- `restrict_image_formats` `<flag>`: if `TRUE` then PDF images are only
  included in the PDF manual, and SVG images are only included in the
  HTML manual. (This only applies to images supplied via markdown.)

## How to set

Either set in `DESCRIPTION` using `Config/roxygen2/` fields:

    Config/roxygen2/markdown: TRUE
    Config/roxygen2/load: installed

Or if you need more complex options (like `rd_family_title` or
`knitr_chunk_options`), put them in `man/roxygen/meta.R`:

    list(
      rd_family_title = list(models = "Model functions"),
      knitr_chunk_options = list(fig.width = 7)
    )

## See also

Other extending:
[`parse_package()`](https://roxygen2.r-lib.org/reference/parse_package.md),
[`rd_section()`](https://roxygen2.r-lib.org/reference/rd_section.md),
[`roc_proc_text()`](https://roxygen2.r-lib.org/reference/roc_proc_text.md),
[`roclet_find()`](https://roxygen2.r-lib.org/reference/roclet_find.md),
[`roxy_block()`](https://roxygen2.r-lib.org/reference/roxy_block.md),
[`roxy_tag()`](https://roxygen2.r-lib.org/reference/roxy_tag.md),
[`roxy_tag_rd()`](https://roxygen2.r-lib.org/reference/roxy_tag_rd.md),
[`tag_parsers`](https://roxygen2.r-lib.org/reference/tag_parsers.md),
[`tags_list()`](https://roxygen2.r-lib.org/reference/tags_list.md)
