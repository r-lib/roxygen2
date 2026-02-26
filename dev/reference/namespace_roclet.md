# Roclet: make `NAMESPACE`

This roclet automates the production of a `NAMESPACE` file, which
controls the functions imported and exported by your package, as
described in [Writing R
extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html).

The `NAMESPACE` is generated in two passes: the first generates only
import directives (because this can be computed without evaluating
package code), and the second generates everything (after the package
has been loaded).

See
[`vignette("namespace")`](https://roxygen2.r-lib.org/dev/articles/namespace.md)
for details.

## Usage

``` r
namespace_roclet()
```

## See also

[tags-namespace](https://roxygen2.r-lib.org/dev/reference/tags-namespace.md)
for tags that generate `NAMESPACE` directives.

## Examples

``` r
# The most common namespace tag is @export, which declares that a function
# is part of the external interface of your package
#' @export
foofy <- function(x, y, z) {
}

# You'll also often find global imports living in a file called
# R/{package}-package.R.
#' @importFrom magrittr %>%
#' @import rlang
NULL
#> NULL
```
