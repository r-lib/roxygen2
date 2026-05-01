# Re-build outdated vignettes

This roclet rebuilds outdated vignettes with
[tools::buildVignette](https://rdrr.io/r/tools/buildVignette.html), but
we no longer recommend it because we no longer recommend storing built
vignettes in a package.

By default, it will rebuild all vignettes if the source file is newer
than the output pdf or html. (This means it will automatically re-build
the vignette if you change the vignette source, but *not* when you
change the R code). If you want finer control, add a Makefile to
`vignettes/` and roxygen2 will use that instead.

To prevent RStudio from re-building the vignettes again when checking
your package, add `--no-build-vignettes` to the "Build Source Package"
field in your project options.

## Usage

``` r
vignette_roclet()
```
