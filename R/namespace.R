ns_tags_import <- c('import', 'importFrom', 'importClassesFrom', 'importMethodsFrom', 'useDynLib', 'rawNamespace')
ns_tags_export <- c('export', 'exportClass', 'exportMethod', 'exportPattern', 'S3method')
ns_tags <- c(ns_tags_import, ns_tags_export, 'evalNamespace')

#' Roclet: make NAMESPACE.
#'
#' This roclet automates the production of a `NAMESPACE` file,
#' see Writing R Extensions.
#' (<https://cran.r-project.org/doc/manuals/R-exts.pdf>) for details.
#' The `NAMESPACE` is generated in two passes: the first generates only
#' import directives (because this can be computed without evaluating package
#' code), and the second generates everything (after the package has been
#' loaded).
#'
#' @family roclets
#' @export
#' @seealso `vignette("namespace", package = "roxygen2")`
#' @aliases export exportClass exportMethod exportPattern
#'   import importFrom importClassesFrom importMethodsFrom
#'   evalNamespace rawNamespace S3method useDynLib
namespace_roclet <- function() {
  roclet("namespace")
}

#' @export
roclet_preprocess.roclet_namespace <- function(x,
                                               blocks,
                                               base_path,
                                               global_options = list()) {

  lines <- unlist(lapply(blocks, block_to_ns, tag_set = ns_tags_import)) %||% character()
  lines <- sort_c(unique(lines))

  NAMESPACE <- file.path(base_path, "NAMESPACE")
  if (purrr::is_empty(lines) && !made_by_roxygen(NAMESPACE)) {
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

  ns <- unlist(lapply(blocks, block_to_ns, env = env)) %||%
    character()
  sort_c(unique(ns))
}

#' @export
roclet_tags.roclet_namespace <- function(x) {
  list(
    evalNamespace = tag_code,
    export = tag_words_line,
    exportClass = tag_words(1),
    exportMethod = tag_words(1),
    exportPattern = tag_words(1),
    import = tag_words(1),
    importClassesFrom = tag_words(2),
    importFrom = tag_words(2),
    importMethodsFrom = tag_words(2),
    rawNamespace = tag_code,
    S3method = tag_words(2, 2),
    useDynLib = tag_words(1)
  )
}

block_to_ns <- function(block, env, tag_set = ns_tags) {
  tags <- intersect(names(block), tag_set)
  lapply(tags, ns_process_tag, block = block, env = env)
}

ns_process_tag <- function(tag_name, block, env) {
  f <- if (tag_name == "evalNamespace") {
    function(tag, block) ns_evalNamespace(tag, block, env)
  } else {
    get(paste0("ns_", tag_name), mode = "function")
  }
  tags <- block[names(block) == tag_name]

  lapply(tags, f, block = block)
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

# Functions that take complete block and return NAMESPACE lines
ns_export <- function(tag, block) {
  if (identical(tag, "")) {
    # FIXME: check for empty exports (i.e. no name)
    default_export(attr(block, "object"), block)
  } else {
    export(tag)
  }
}
default_export <- function(x, block) UseMethod("default_export")
#' @export
default_export.s4class   <- function(x, block) export_class(x$value@className)
#' @export
default_export.s4generic  <- function(x, block) export(x$value@generic)
#' @export
default_export.s4method  <- function(x, block) export_s4_method(x$value@generic)
#' @export
default_export.s3method  <- function(x, block) export_s3_method(attr(x$value, "s3method"))
#' @export
default_export.rcclass   <- function(x, block) export_class(x$value@className)
#' @export
default_export.default   <- function(x, block) export(x$alias)
#' @export
default_export.NULL      <- function(x, block) export(block$name)

ns_S3method          <- function(tag, block) {
  block_warning(block, "@S3method is deprecated. Please use @export instead")
  export_s3_method(tag)
}
ns_exportClass       <- function(tag, block) export_class(tag)
ns_exportMethod      <- function(tag, block) export_s4_method(tag)
ns_exportPattern     <- function(tag, block) one_per_line("exportPattern", tag)
ns_import            <- function(tag, block) one_per_line("import", tag)
ns_importFrom        <- function(tag, block) repeat_first("importFrom", tag)
ns_importClassesFrom <- function(tag, block) repeat_first("importClassesFrom", tag)
ns_importMethodsFrom <- function(tag, block) repeat_first("importMethodsFrom", tag)
ns_useDynLib         <- function(tag, block) {
  if (length(tag) == 1) {
    return(paste0("useDynLib(", quote_if_needed(tag), ")"))
  }

  if (any(grepl(",", tag))) {
    # If there's a comma in list, don't quote output. This makes it possible
    # for roxygen2 to support other NAMESPACE forms not otherwise mapped
    args <- paste0(tag, collapse = " ")
    paste0("useDynLib(", args, ")")
  } else {
    repeat_first("useDynLib", tag)
  }
}
ns_rawNamespace  <- function(tag, block) tag
ns_evalNamespace <- function(tag, block, env) {
  block_eval(tag, block, env, "@evalNamespace")
}

# Functions used by both default_export and ns_* functions
export           <- function(x) one_per_line("export", x)
export_class     <- function(x) one_per_line("exportClasses", x)
export_s4_method <- function(x) one_per_line("exportMethods", x)
export_s3_method <- function(x) fun_args("S3method", x)

one_per_line <- function(name, x) {
  paste0(name, "(", quote_if_needed(x), ")")
}
repeat_first <- function(name, x) {
  paste0(name, "(", quote_if_needed(x[1]), ",", quote_if_needed(x[-1]), ")")
}
fun_args <- function(name, x) {
  if (any(grepl(",", x))) {
    # If there's a comma in list, don't quote output. This makes it possible
    # for roxygen2 to support other NAMESPACE forms not otherwise mapped
    args <- paste0(x, collapse = ", ")
  } else {
    args <- paste0(quote_if_needed(x), collapse = ",")
  }

  paste0(name, "(", args, ")")
}

quote_if_needed <- function(x) {
  needs_quotes <- !has.quotes(x) & !is.syntactic(x)
  x[needs_quotes] <- paste0('"', str_replace_all(x[needs_quotes], '(["\\\\])', "\\\\\\1"), '"')
  x
}
is.syntactic <- function(x) make.names(x) == x
has.quotes <- function(x) str_detect(x, "^('|\").*\\1$")
