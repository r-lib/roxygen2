# Roclet: make Rd files

This [roclet](https://roxygen2.r-lib.org/dev/reference/roclet.md)
automates the production of the `.Rd` files that R uses to document
functions, datasets, packages, classes, and more. See
[`vignette("rd-functions")`](https://roxygen2.r-lib.org/dev/articles/rd-functions.md)
for details.

It is run by default by
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md).

## Usage

``` r
rd_roclet()
```

## See also

[tags-rd-functions](https://roxygen2.r-lib.org/dev/reference/tags-rd-functions.md),
[tags-rd-datasets](https://roxygen2.r-lib.org/dev/reference/tags-rd-datasets.md),
[tags-rd-S3](https://roxygen2.r-lib.org/dev/reference/tags-rd-S3.md),
[tags-rd-S4](https://roxygen2.r-lib.org/dev/reference/tags-rd-S4.md),
[tags-rd-S7](https://roxygen2.r-lib.org/dev/reference/tags-rd-S7.md),
[tags-rd-R6](https://roxygen2.r-lib.org/dev/reference/tags-rd-R6.md),
[tags-reuse](https://roxygen2.r-lib.org/dev/reference/tags-reuse.md),
[tags-index-crossref](https://roxygen2.r-lib.org/dev/reference/tags-index-crossref.md)
for tags provided by this roclet.

## Examples

``` r
#' Add together two numbers
#' @param x A number.
#' @param y A number.
#' @return A number.
#' @export
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}
```
