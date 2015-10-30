#' Process a package with the Rd, namespace and collate roclets.
#'
#' This is the workhorse function that uses roclets, the built-in document
#' tranformation functions, to build all documentation for a package.  See
#' the documentation for the individual roclets, \code{\link{rd_roclet}},
#' \code{\link{namespace_roclet}}, and for \code{\link{update_collate}},
#' for more details.
#'
#' Note that roxygen2 is a dynamic documentation system: it works using
#' by inspecting loaded objects in the package. This means that you must
#' be able to load the package in order to document it.
#' \code{\link{source_package}} provides a simple simulation of package
#' loading that works if you only have R files in your package. For more
#' complicated packages, I recommend using \code{devtools::document} which
#' does a much better job at simulating package install and load.
#'
#' @param package.dir Location of package top level directory. Default is
#'   working directory.
#' @param roclets Character vector of roclet names to use with package.
#'   This defaults to \code{NULL}, which will use the \code{roclets} fields in
#'   the list provided in the \code{Roxygen} DESCRIPTION field. If none are
#'   specified, defaults to \code{c("collate", "namespace", "rd")}.
#' @param load_code A function used to load all the R code in the package
#'   directory. It is called with the path to the package, and it should return
#'   an environment containing all the sourced code.
#' @param clean If \code{TRUE}, roxygen will delete all files previously
#'   created by roxygen before running each roclet.
#' @return \code{NULL}
#' @export
#' @importFrom stats setNames
roxygenize <- function(package.dir = ".",
                       roclets = NULL,
                       load_code = source_package,
                       clean = FALSE) {

  is_first <- first_time(package.dir)
  if (is_first) {
    message("First time using roxygen2. Upgrading automatically...")
  }

  base_path <- normalizePath(package.dir)
  man_path <- file.path(base_path, "man")
  dir.create(man_path, recursive = TRUE, showWarnings = FALSE)
  update_roxygen_version(base_path)

  options <- load_options(base_path)
  roclets <- roclets %||% options$roclets

  # Special case collate: it doesn't need to execute code, and must be run
  # first to ensure that code can be executed
  if ("collate" %in% roclets) {
    update_collate(base_path)
    roclets <- setdiff(roclets, "collate")
  }

  if (length(roclets) == 0)
    return(invisible())

  parsed <- parse_package(base_path, load_code)

  roclets <- paste0(roclets, "_roclet", sep = "")
  roc_out <- function(roclet) {
    roc <- get(roclet, mode = "function")()

    if (clean) {
      clean(roc, base_path)
    }
    results <- roc_process(roc, parsed, base_path, options = options)
    roc_output(roc, results, base_path, options = options, check = !is_first)
  }
  invisible(unlist(lapply(roclets, roc_out)))
}

#' @rdname roxygenize
#' @export
roxygenise <- roxygenize

#' Load options from DESCRIPTION.
#'
#' @param base_path Path to package.
#' @export
#' @keywords internal
load_options <- function(base_path = ".") {
  desc_path <- file.path(base_path, "DESCRIPTION")
  desc_opts <- read.dcf(desc_path, fields = "Roxygen")[[1, 1]]

  if (is.na(desc_opts)) {
    opts <- list()
  } else {
    opts <- eval(parse(text = desc_opts))
  }

  defaults <- list(
    wrap = FALSE,
    roclets = c("collate", "namespace", "rd")
  )

  unknown_opts <- setdiff(names(opts), names(defaults))
  if (length(unknown_opts) > 0) {
    warning("Unknown Roxygen options ", paste(unknown_opts, collapse = ", "),
            ".\nSupported options: ", paste(names(defaults), collapse = ", "))
  }

  utils::modifyList(defaults, opts)
}
