#' Process a package with the Rd, namespace and collate roclets.
#'
#' This is the workhorse function that uses roclets, the built-in document
#' tranformation functions, to build all documentation for a package. See the
#' documentation for the individual roclets, \code{\link{rd_roclet}},
#' \code{\link{namespace_roclet}}, and for \code{\link{update_collate}}, for
#' more details.
#'
#' For the arguments \code{roclets} and \code{wrap}, we can specify their values
#' in the \file{DESCRIPTION} file of a package using the \code{Roxygen} field.
#' For example, \option{Roxygen: list(wrap = TRUE, roclets = c("rd"))}. This
#' will overwrite the values passed to \code{roxygenize()}.
#' @param package.dir the package's top directory
#' @param roxygen.dir,copy.package,overwrite,unlink.target deprecated
#' @param roclets character vector of roclet names to apply to package
#' @param wrap whether to re-wrap the text in Rd
#' @param load_code A function used to load all the R code in the package
#'   directory. It is called with the path to the package, and it should return
#'   an environment containing all the sourced code.
#' @return \code{NULL}
#' @export
roxygenize <- function(package.dir = ".",
                       roxygen.dir=package.dir,
                       copy.package=package.dir != roxygen.dir,
                       overwrite=TRUE,
                       unlink.target=FALSE,
                       roclets = c("collate", "namespace", "rd"),
                       wrap = FALSE,
                       load_code = source_package) {
  if (copy.package) {
    stop("Non-inplace roxygen no longer supported")
  }

  base_path <- normalizePath(package.dir)
  man_path <- file.path(base_path, "man")
  dir.create(man_path, recursive = TRUE, showWarnings = FALSE)

  defaults <- list(wrap = wrap, roclets = roclets)
  options <- modifyList(defaults, load_options(base_path))
  roclets <- options$roclets

  # Special case collate: it doesn't need to execute code, and must be run
  # first to ensure that code can be executed
  if ("collate" %in% roclets) {
    update_collate(base_path)
    roclets <- setdiff(roclets, "collate")
  }

  parsed <- parse_package(base_path, load_code)

  roclets <- str_c(roclets, "_roclet", sep = "")
  roc_out <- function(roclet) {
    roc <- get(roclet, mode = "function")()
    results <- roc_process(roc, parsed, base_path, options = options)
    roc_output(roc, results, base_path, options = options)    
  }
  invisible(unlist(lapply(roclets, roc_out)))
}

#' @rdname roxygenize
#' @export
roxygenise <- roxygenize

load_options <- function(base_path) {
  desc_path <- file.path(base_path, "DESCRIPTION")
  desc_opts <- read.dcf(desc_path, fields = "Roxygen")[[1, 1]]

  if (is.na(desc_opts)) list() else eval(parse(text = desc_opts))
}
