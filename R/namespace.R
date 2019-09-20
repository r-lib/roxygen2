# Processed first
ns_tags_import <- c(
  "import",
  "importFrom",
  "importClassesFrom",
  "importMethodsFrom",
  "useDynLib",
  "rawNamespace"
)
ns_tags <- c(
  ns_tags_import,
  "evalNamespace",
  "export",
  "exportClass",
  "exportMethod",
  "exportS3Method",
  "exportPattern"
)

#' Roclet: make `NAMESPACE`
#'
#' @description
#' This roclet automates the production of a `NAMESPACE` file, which controls
#' the functions imported and exported by your package, as described in
#' [Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html).
#'
#' The `NAMESPACE` is generated in two passes: the first generates only
#' import directives (because this can be computed without evaluating package
#' code), and the second generates everything (after the package has been
#' loaded).
#'
#' See `vignette("namespace")` for details.
#'
#' @family roclets
#' @export
#' @eval tag_aliases(roclet_tags.roclet_namespace)
#' @examples
#' # The most common namespace tag is @@export, which declares that a function
#' # is part of the external interface of your package
#' #' @export
#' foofy <- function(x, y, z) {
#' }
#'
#' # You'll also often find global imports living in a file called
#' # R/{package}-package.R.
#' #' @@importFrom magrittr %>%
#' #' @@import rlang
#' NULL
namespace_roclet <- function() {
  roclet("namespace")
}

#' @export
roclet_preprocess.roclet_namespace <- function(x,
                                               blocks,
                                               base_path,
                                               global_options = list()) {

  lines <- blocks_to_ns(blocks, env, tag_set = ns_tags_import)
  NAMESPACE <- file.path(base_path, "NAMESPACE")

  if (length(lines) == 0 && !made_by_roxygen(NAMESPACE)) {
    return(x)
  }

  results <- c(made_by("#"), lines)
  write_if_different(NAMESPACE, results, check = FALSE)

  invisible(x)
}

#' @export
roclet_process.roclet_namespace <- function(x,
                                            blocks,
                                            env,
                                            base_path,
                                            global_options = list()) {
  blocks_to_ns(blocks, env)
}

#' @export
roclet_tags.roclet_namespace <- function(x) {
  list(
    evalNamespace = tag_code,
    export = tag_words_line,
    exportClass = tag_words(1),
    exportS3Method = tag_words(min = 0, max = 2),
    exportMethod = tag_words(1),
    exportPattern = tag_words(1),
    import = tag_words(1),
    importClassesFrom = tag_words(2),
    importFrom = tag_words(2),
    importMethodsFrom = tag_words(2),
    rawNamespace = tag_code,
    useDynLib = tag_words(1)
  )
}

#' @export
roclet_output.roclet_namespace <- function(x, results, base_path, ...) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  results <- c(made_by("#"), results)

  # Always check for roxygen2 header before overwriting NAMESPACE (#436),
  # even when running for the first time
  write_if_different(NAMESPACE, results, check = TRUE)

  NAMESPACE
}

#' @export
roclet_clean.roclet_namespace <- function(x, base_path) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  if (made_by_roxygen(NAMESPACE)) {
    unlink(NAMESPACE)
  }
}

# NAMESPACE generation ----------------------------------------------------

blocks_to_ns <- function(blocks, env, tag_set = ns_tags) {
  lines <- map(blocks, block_to_ns, env = env, tag_set = tag_set)
  lines <- unlist(lines) %||% character()

  sort_c(unique(lines))
}

block_to_ns <- function(block, env, tag_set = ns_tags) {
  tags <- block_get_tags(block, tag_set)

  map(tags, function(tag) {
    exec(paste0("ns_", tag$tag), tag, block, env)
  })
}

ns_export <- function(tag, block, env) {
  if (identical(tag$val, "")) {
    # FIXME: check for empty exports (i.e. no name)
    default_export(block$object, block)
  } else {
    export(tag$val)
  }
}

ns_exportClass       <- function(tag, block, env) export_class(tag$val)
ns_exportMethod      <- function(tag, block, env) export_s4_method(tag$val)
ns_exportPattern     <- function(tag, block, env) one_per_line("exportPattern", tag$val)
ns_import            <- function(tag, block, env) one_per_line("import", tag$val)
ns_importFrom        <- function(tag, block, env) repeat_first("importFrom", tag$val)
ns_importClassesFrom <- function(tag, block, env) repeat_first("importClassesFrom", tag$val)
ns_importMethodsFrom <- function(tag, block, env) repeat_first("importMethodsFrom", tag$val)

ns_exportS3Method    <- function(tag, block, env) {
  obj <- block$object

  if (length(tag$val) < 2 && !inherits(obj, "s3method")) {
    roxy_tag_warning(tag,
      "`@exportS3Method` and `@exportS3Method generic` must be used with an S3 method"
    )
    return()
  }

  if (identical(tag$val, "")) {
    method <- attr(obj$value, "s3method")
  } else if (length(tag$val) == 1) {
    method <- c(tag$val, attr(obj$value, "s3method")[[2]])
  } else {
    method <- tag$val
  }

  export_s3_method(method)
}

ns_useDynLib         <- function(tag, block, env) {
  if (length(tag$val) == 1) {
    return(paste0("useDynLib(", auto_quote(tag$val), ")"))
  }

  if (any(grepl(",", tag$val))) {
    # If there's a comma in list, don't quote output. This makes it possible
    # for roxygen2 to support other NAMESPACE forms not otherwise mapped
    args <- paste0(tag$val, collapse = " ")
    paste0("useDynLib(", args, ")")
  } else {
    repeat_first("useDynLib", tag$val)
  }
}
ns_rawNamespace  <- function(tag, block, env) tag$val
ns_evalNamespace <- function(tag, block, env) {
  roxy_tag_eval(tag, env)
}

# Functions used by both default_export and ns_* functions
export           <- function(x) one_per_line("export", x)
export_class     <- function(x) one_per_line("exportClasses", x)
export_s4_method <- function(x) one_per_line("exportMethods", x)
export_s3_method <- function(x) {
  args <- paste0(auto_backtick(x), collapse = ",")
  paste0("S3method(", args, ")")
}

# Default export methods --------------------------------------------------

default_export <- function(x, block) UseMethod("default_export")
#' @export
default_export.s4class   <- function(x, block) export_class(x$value@className)
#' @export
default_export.s4generic <- function(x, block) export(x$value@generic)
#' @export
default_export.s4method  <- function(x, block) export_s4_method(x$value@generic)
#' @export
default_export.s3method  <- function(x, block) export_s3_method(attr(x$value, "s3method"))
#' @export
default_export.rcclass   <- function(x, block) export_class(x$value@className)
#' @export
default_export.default   <- function(x, block) export(x$alias)
#' @export
default_export.NULL      <- function(x, block) export(block_get_tag_value(block, "name"))


# Helpers -----------------------------------------------------------------

one_per_line <- function(name, x) {
  paste0(name, "(", auto_backtick(x), ")")
}
repeat_first <- function(name, x) {
  paste0(name, "(", auto_backtick(x[1]), ",", auto_backtick(x[-1]), ")")
}
