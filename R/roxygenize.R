#' Process a package with the Rd, namespace and collate roclets.
#'
#' This is the workhorse function that uses roclets, the built-in document
#' tranformation functions, to build all documentation for a package. See
#' the documentation for the individual roclets, [rd_roclet()],
#' [namespace_roclet()], and for [update_collate()],
#' for more details.
#'
#' Note that roxygen2 is a dynamic documentation system: it works by
#' inspecting loaded objects in the package. This means that you must
#' be able to load the package in order to document it.
#'
#' @param package.dir Location of package top level directory. Default is
#'   working directory.
#' @param roclets Character vector of roclet names to use with package.
#'   This defaults to `NULL`, which will use the `roclets` fields in
#'   the list provided in the `Roxygen` DESCRIPTION field. If none are
#'   specified, defaults to `c("collate", "namespace", "rd")`.
#' @param load_code A function used to load all the R code in the package
#'   directory. It is called with the path to the package, and it should return
#'   an environment containing all the sourced code.
#' @param clean If `TRUE`, roxygen will delete all files previously
#'   created by roxygen before running each roclet.
#' @return `NULL`
#' @export
#' @importFrom stats setNames
roxygenize <- function(package.dir = ".",
                       roclets = NULL,
                       load_code = env_package,
                       clean = FALSE) {

  base_path <- normalizePath(package.dir)
  is_first <- roxygen_setup(base_path)

  encoding <- desc::desc_get("Encoding", file = base_path)[[1]]
  if (!identical(encoding, "UTF-8")) {
    warning("roxygen2 requires Encoding: UTF-8", call. = FALSE)
  }

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

  roclets <- lapply(roclets, roclet_find)

  # Tokenise each file
  registry <- purrr::flatten(lapply(roclets, roclet_tags))
  blocks <- parse_package(base_path,
    env = NULL,
    registry = registry,
    global_options = options
  )

  if (clean) {
    purrr::walk(roclets, roclet_clean, base_path = base_path)
  }

  roclets <- lapply(roclets, roclet_preprocess,
    blocks = blocks,
    global_options = options,
    base_path = base_path
  )

  # Now load code
  env <- load_code(base_path)
  blocks <- lapply(blocks, block_set_env,
    env = env,
    registry = registry,
    global_options = options
  )

  results <- lapply(roclets, roclet_process,
    blocks = blocks,
    env = env,
    base_path = base_path,
    global_options = options
  )

  out <- purrr::map2(
    roclets, results,
    roclet_output,
    base_path = base_path,
    is_first = is_first
  )
  invisible(out)
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
    roclets = c("collate", "namespace", "rd"),
    markdown = markdown_global_default
  )

  unknown_opts <- setdiff(names(opts), names(defaults))
  if (length(unknown_opts) > 0) {
    warning("Unknown Roxygen options ", paste(unknown_opts, collapse = ", "),
            ".\nSupported options: ", paste(names(defaults), collapse = ", "))
  }

  utils::modifyList(defaults, opts)
}


roxygen_setup <- function(base_path) {
  is_first <- first_time(base_path)
  if (is_first) {
    message("First time using roxygen2. Upgrading automatically...")
  }

  man_path <- file.path(base_path, "man")
  dir.create(man_path, recursive = TRUE, showWarnings = FALSE)
  update_roxygen_version(base_path)

  is_first
}
