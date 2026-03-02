#' Process a package with the Rd, namespace and collate roclets
#'
#' This is the workhorse function that uses roclets, the built-in document
#' transformation functions, to build all documentation for a package. See
#' the documentation for the individual roclets, [rd_roclet()],
#' [namespace_roclet()], and for [update_collate()],
#' for more details.
#'
#' Note that roxygen2 is a dynamic documentation system: it works by
#' inspecting loaded objects in the package. This means that you must
#' be able to load the package in order to document it: see [load] for
#' details.
#'
#' @param package.dir Location of package top level directory. Default is
#'   working directory.
#' @param roclets Character vector of roclet names to use with package.
#'   The default, `NULL`, uses the roxygen `roclets` option,
#'   which defaults to `c("collate", "namespace", "rd")`.
#' @param load_code A function used to load all the R code in the package
#'   directory. The default, `NULL`, uses the strategy defined by
#'   the `load` roxygen option, which defaults to [load_pkgload()].
#'   See [load] for more details.
#' @param clean If `TRUE`, roxygen will delete all files previously
#'   created by roxygen before running each roclet.
#' @return `NULL`
#' @export
roxygenize <- function(
  package.dir = ".",
  roclets = NULL,
  load_code = NULL,
  clean = FALSE
) {
  base_path <- normalizePath(package.dir)
  is_first <- roxygen_setup(base_path)

  roxy_meta_load(base_path)
  # Load required packages for method registration
  packages <- roxy_meta_get("packages")
  lapply(packages, loadNamespace)

  roclets <- roclets %||% roxy_meta_get("roclets")

  # To load code, we need a up-to-date Collate field and NAMESPACE
  if ("collate" %in% roclets) {
    update_collate(base_path)
    roclets <- setdiff(roclets, "collate")
  }
  if ("namespace" %in% roclets) {
    update_namespace_imports(base_path)
  }

  if (length(roclets) == 0) {
    return(invisible())
  }

  roclets <- lapply(roclets, roclet_find)

  if (!is_interactive()) {
    withr::local_options(warn = 1)
  }

  # Now load code
  load_code <- find_load_strategy(load_code)
  env <- load_code(base_path)
  local_roxy_meta_set("env", env)

  # Tokenise each file
  blocks <- parse_package(base_path, env = NULL)

  if (clean) {
    purrr::walk(roclets, roclet_clean, base_path = base_path)
  }

  roclets <- lapply(
    roclets,
    roclet_preprocess,
    blocks = blocks,
    base_path = base_path
  )

  blocks <- lapply(blocks, block_set_env, env = env)

  results <- lapply(
    roclets,
    roclet_process,
    blocks = blocks,
    env = env,
    base_path = base_path
  )

  out <- purrr::map2(
    roclets,
    results,
    roclet_output,
    base_path = base_path,
    is_first = is_first
  )
  invisible(out)
}

#' @rdname roxygenize
#' @export
roxygenise <- roxygenize
