# Roclet: make Rd files

This roclet is the workhorse of roxygen2, producing the `.Rd` files that
R uses to document functions, datasets, packages, classes, and more. See
[`vignette("rd")`](https://roxygen2.r-lib.org/dev/articles/rd.md) for
details.

Generally you will not call this function directly but will instead use
[`roxygenise()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md)
specifying the rd roclet.

## Usage

``` r
rd_roclet()
```

## See also

[tags-rd](https://roxygen2.r-lib.org/dev/reference/tags-rd.md),
[tags-rd-other](https://roxygen2.r-lib.org/dev/reference/tags-rd-other.md),
[tags-reuse](https://roxygen2.r-lib.org/dev/reference/tags-reuse.md),
[tags-index-crossref](https://roxygen2.r-lib.org/dev/reference/tags-index-crossref.md)
for tags provided by this roclet.

## Examples

``` r
#' The length of a string (in characters)
#'
#' @param x A character vector.
#' @returns An integer vector the same length as `x`.
#'   `NA` strings have `NA` length.
#' @seealso [nchar()]
#' @export
#' @examples
#' str_length(letters)
#' str_length(c("i", "like", "programming", NA))
str_length <- function(x) {
}
```
