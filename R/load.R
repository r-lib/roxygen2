#' Load package code
#'
#' @description
#' roxygen2 is a dynamic documentation system, which means it works with the
#' objects inside your package, not just the source code used to create them.
#' These functions offer various ways of loading your package to suit various
#' constraints:
#'
#' * `load_pkgload()` uses `pkgload::load_all()` to simulate package loading
#'   as closely as we know how. It offers high fidelity handling of code that
#'   uses S4, but requires that the package be compiled.
#'
#' * `load_source()` simulates package loading by attaching packages listed in
#'   `Depends` and `Imports`, then sources all files in the `R/` directory.
#'   This was the default strategy used in roxygen2 6.0.0 and earlier;
#'   it's primary advantage is that it does not need compilation.
#'
#' * `load_installed()` uses the installed version of the package. Use this
#'   strategy if you have installed a development version of the package
#'   already. This is the highest fidelity strategy, but requires work
#'   outside of roxygen2.
#'
#' You can change the default strategy for your function with roxygen2 `load`
#' option. Override the default off `pkgload` to use the `source` or
#' `installed` strategies:
#'
#' ```
#' Roxygen: list(load = "source")
#' ```
#' @name load
#' @param path Path to source package
NULL

#' @rdname load
#' @export
load_pkgload <- function(path) {
  pkgload::load_all(path, helpers = FALSE, attach_testthat = FALSE)$env
}

#' @rdname load
#' @export
load_installed <- function(path) {
  package <- desc::desc_get_field("Package", file = path)
  asNamespace(package)
}

#' @rdname load
#' @export
load_source <- function(path) {
  # Create environment
  env <- new.env(parent = globalenv())
  methods::setPackageName("roxygen_devtest", env)

  # Attach dependencies
  deps <- desc::desc_get_deps(path)
  pkgs <- deps$package[
    deps$type %in% c("Depends", "Imports") & deps$package != "R"
  ]
  lapply(pkgs, require, character.only = TRUE)

  # Source files
  lapply(package_files(path), sys_source, envir = env)

  env
}

sys_source <- function(file, envir = baseenv()) {
  exprs <- parse(text = read_lines(file))
  for (expr in exprs) {
    eval(expr, envir = envir)
  }
  invisible()
}

# Helpers -----------------------------------------------------------------

find_load_strategy <- function(x, option = roxy_meta_get("load", "pkgload")) {
  if (is.function(x)) {
    return(x)
  }

  if (is.null(x)) {
    x <- option
    if (!is.character(x) || length(x) != 1) {
      cli::cli_abort("roxygen2 {.code load} option must be a string")
    }
  } else {
    if (!is.character(x) || length(x) != 1) {
      cli::cli_abort("{.code load_code} must be a string or function")
    }
  }

  switch(
    x,
    pkgload = load_pkgload,
    source = load_source,
    installed = load_installed,
    cli::cli_abort("Unknown value of {.code load} option")
  )
}
