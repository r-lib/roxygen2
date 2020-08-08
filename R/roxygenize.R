#' Process a package with the Rd, namespace and collate roclets.
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
#' @importFrom stats setNames
roxygenize <- function(package.dir = ".",
                       roclets = NULL,
                       load_code = NULL,
                       clean = FALSE) {

  base_path <- normalizePath(package.dir, mustWork = TRUE)
  is_first <- roxygen_setup(base_path)

  encoding <- desc::desc_get("Encoding", file = base_path)[[1]]
  if (!identical(encoding, "UTF-8")) {
    warning("roxygen2 requires Encoding: UTF-8", call. = FALSE)
  }

  roxy_meta_load(base_path)

  # Load required packages for method registration
  packages <- roxy_meta_get("packages")
  lapply(packages, loadNamespace)

  roclets <- roclets %||% roxy_meta_get("roclets")
  # Special case collate: it doesn't need to execute code, and must be run
  # first to ensure that code can be executed
  if ("collate" %in% roclets) {
    update_collate(base_path)
    roclets <- setdiff(roclets, "collate")
  }

  if (length(roclets) == 0)
    return(invisible())

  roclets <- lapply(roclets, roclet_find)

  # Now load code
  load_code <- find_load_strategy(load_code)
  env <- load_code(base_path)
  roxy_meta_set("env", env)
  on.exit(roxy_meta_set("env", NULL), add = TRUE)

  # Tokenise each file
  blocks <- parse_package(base_path, env = NULL)

  if (clean) {
    purrr::walk(roclets, roclet_clean, base_path = base_path)
  }

  roclets <- lapply(roclets, roclet_preprocess,
    blocks = blocks,
    base_path = base_path
  )

  blocks <- lapply(blocks, block_set_env, env = env)
  # fixme: mm 08/06/2020
  # We create a list of all the S4-classes from the blocks and very manually appending it to
  # the env, which introduces duplication in the form of a 'secret property' whose
  # name has to be known at the receiving end.
  # Ideally this would
  # not be necessary because we could inspect the env where we need it with
  # s4_class_names <- methods::getClasses(where=env) 
  # Unfortunately s4 <- s4_class_names would contain too many
  # classes ,e.g. superclasse we can not document e.g.:atomic_vector,numLike,number Mnumeric 
  # if our classes inherited from them and 
  # env had been produced by pkglaod::load_all 
  # (even if the export_all flag is set to FALSE)
  # or even if we use a namespace env as returned by loadNamespace.
  # Furthermore the results of getNamespaceExports(ns_env) differ depending
  # on if ns_env had been the result of:
  # 1.) ns_env <- loadNamespace(pkg_name)  (Assuming pkg_name=pgkload::pkg_name(base_path)
  # 2.) ns_env <-  pkgload::pkg_ns(base_path)
  #
  # In case of our package TEE/SoilR-exp 
  # s4_class_names <- intersect(getNamespaceExports(ns_env),getClasses(where=ns_env))
  # results in 16 Classes for approach 1.) as opposed to 49 for approach 2)
  # 
  # At least the 49 classes are consistent with the result from the blocks, so we 
  # could remove this code
  #
  # It is however also important that the classes are documented.
  # otherwise the links will fail.

  s4_class_names = sapply(
    purrr::keep(
      blocks,
      .p=function(block){
          attr(block,"class")[1] == 'roxy_block_s4class'
      }
    ),
    function(block){
      attr(block$object$value,'className')[1]
    }
  )
  env$documented_s4_class_names=s4_class_names


  results <- lapply(roclets, roclet_process,
    blocks = blocks,
    env = env,
    base_path = base_path
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

roxy_warning <- function(..., file = NA, line = NA) {
  message <- paste0(
    if (!is.na(file)) paste0("[", file, ":", line, "] "),
    ...,
    collapse = " "
  )

  warning(message, call. = FALSE, immediate. = TRUE)
  NULL
}
