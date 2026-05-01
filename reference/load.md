# Load package code

roxygen2 is a dynamic documentation system, which means it works with
the objects inside your package, not just the source code used to create
them. These functions offer various ways of loading your package to suit
various constraints:

- `load_pkgload()` uses
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
  to simulate package loading as closely as we know how. It offers high
  fidelity handling of code that uses S4, but requires that the package
  be compiled.

- `load_source()` simulates package loading by attaching packages listed
  in `Depends` and `Imports`, then sources all files in the `R/`
  directory. This was the default strategy used in roxygen2 6.0.0 and
  earlier; it's primary advantage is that it does not need compilation.

- `load_installed()` uses the installed version of the package. Use this
  strategy if you have installed a development version of the package
  already. This is the highest fidelity strategy, but requires work
  outside of roxygen2.

You can change the default strategy for your function with roxygen2
`load` option. Override the default off `pkgload` to use the `source` or
`installed` strategies:

    Roxygen: list(load = "source")

## Usage

``` r
load_pkgload(path)

load_installed(path)

load_source(path)
```

## Arguments

- path:

  Path to source package
