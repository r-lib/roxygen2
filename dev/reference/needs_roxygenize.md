# Check if documentation needs to be updated

A lightweight check that compares modification times of `.Rd` files in
`man/` with the source files listed in their backrefs. This is much
faster than running
[`roxygenize()`](https://roxygen2.r-lib.org/dev/reference/roxygenize.md)
but can suffer from both false negatives (e.g. if an inherited
documentation topic has changed) and false positives (e.g. if a source
file was modified but the change doesn't affect the documentation).

## Usage

``` r
needs_roxygenize(package.dir = ".", quiet = FALSE)
```

## Arguments

- package.dir:

  Location of package top level directory. Default is working directory.

- quiet:

  If `TRUE`, suppresses the message listing out-of-date man pages.

## Value

A logical value, invisibly. `TRUE` if any man pages appear to be out of
date; `FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
needs_roxygenize()
} # }
```
