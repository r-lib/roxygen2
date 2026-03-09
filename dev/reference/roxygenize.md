# Document a package with roxygen2

This is the workhorse function that builds manual pages and metadata for
a package. It is powered by
[roclets](https://roxygen2.r-lib.org/dev/reference/roclet.md),
roxygen2's plugin system for producing different types of output. See
the documentation of the individual components
([`rd_roclet()`](https://roxygen2.r-lib.org/dev/reference/rd_roclet.md),
[`namespace_roclet()`](https://roxygen2.r-lib.org/dev/reference/namespace_roclet.md),
[`update_collate()`](https://roxygen2.r-lib.org/dev/reference/update_collate.md))
for more details, or learn how to make your own in
[`vignette("extending")`](https://roxygen2.r-lib.org/dev/articles/extending.md).

## Usage

``` r
roxygenize(package.dir = ".", roclets = NULL, load_code = NULL, clean = FALSE)

roxygenise(package.dir = ".", roclets = NULL, load_code = NULL, clean = FALSE)
```

## Arguments

- package.dir:

  Location of package top level directory. Default is working directory.

- roclets:

  Character vector of
  [roclets](https://roxygen2.r-lib.org/dev/reference/roclet.md) to use.

  The default, `NULL`, uses the roxygen `roclets` option, which defaults
  to `c("collate", "namespace", "rd")`. This will update (if needed) the
  `Collate` field with
  [`update_collate()`](https://roxygen2.r-lib.org/dev/reference/update_collate.md),
  produce the `NAMESPACE` file with
  [`namespace_roclet()`](https://roxygen2.r-lib.org/dev/reference/namespace_roclet.md),
  and produce the Rd files with
  [`rd_roclet()`](https://roxygen2.r-lib.org/dev/reference/rd_roclet.md).

  (Note that
  [`update_collate()`](https://roxygen2.r-lib.org/dev/reference/update_collate.md)
  is not technically a roclet but is still controlled with this argument
  for historical reasons.)

- load_code:

  A function used to load all the R code in the package directory. The
  default, `NULL`, uses the strategy defined by the `load` roxygen
  option, which defaults to
  [`load_pkgload()`](https://roxygen2.r-lib.org/dev/reference/load.md).
  See [load](https://roxygen2.r-lib.org/dev/reference/load.md) for more
  details.

- clean:

  If `TRUE`, roxygen will delete all files previously created by roxygen
  before running each roclet.

## Value

`NULL`

## Details

Note that roxygen2 is a dynamic documentation system: it works by
inspecting loaded objects in the package. This means that you must be
able to load the package in order to document it: see
[load](https://roxygen2.r-lib.org/dev/reference/load.md) for details.
