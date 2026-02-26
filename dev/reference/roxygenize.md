# Process a package with the Rd, namespace and collate roclets

This is the workhorse function that uses roclets, the built-in document
transformation functions, to build all documentation for a package. See
the documentation for the individual roclets,
[`rd_roclet()`](https://roxygen2.r-lib.org/dev/reference/rd_roclet.md),
[`namespace_roclet()`](https://roxygen2.r-lib.org/dev/reference/namespace_roclet.md),
and for
[`update_collate()`](https://roxygen2.r-lib.org/dev/reference/update_collate.md),
for more details.

## Usage

``` r
roxygenize(package.dir = ".", roclets = NULL, load_code = NULL, clean = FALSE)

roxygenise(package.dir = ".", roclets = NULL, load_code = NULL, clean = FALSE)
```

## Arguments

- package.dir:

  Location of package top level directory. Default is working directory.

- roclets:

  Character vector of roclet names to use with package. The default,
  `NULL`, uses the roxygen `roclets` option, which defaults to
  `c("collate", "namespace", "rd")`.

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
