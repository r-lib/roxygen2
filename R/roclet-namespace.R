#' @include tag-registry.R
NULL

register_tags(
  export = parse.words.line,
  exportClass = words_parser(1),
  exportMethod = words_parser(1),
  exportPattern = words_parser(1),
  import = words_parser(1),
  importClassesFrom = words_parser(2),
  importFrom = words_parser(2),
  importMethodsFrom = words_parser(2),
  rawNamespace = parse.code,
  S3method = words_parser(2, 2),
  useDynLib = words_parser(1)
)

ns_tags <- c('export', 'exportClass', 'exportMethod', 'exportPattern',
  'rawNamespace', 'S3method', 'import', 'importFrom', 'importClassesFrom',
  'importMethodsFrom', 'useDynLib')

#' Roclet: make NAMESPACE.
#'
#' This roclet automates the production of a \file{NAMESPACE} file,
#' see \cite{Writing R Extensions}
#' (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf}) for details.
#'
#' @family roclets
#' @export
#' @seealso \code{vignette("namespace", package = "roxygen2")}
#' @aliases export exportClass exportMethod S3method import importFrom
#'   importClassesFrom importMethodsFrom
namespace_roclet <- function() {
  new_roclet(list, "namespace")
}

#' @export
roc_process.namespace <- function(roclet, parsed, base_path, options = list()) {
  ns <- unlist(lapply(parsed$blocks, block_to_ns)) %||% character()
  sort_c(unique(ns))
}

block_to_ns <- function(block) {
  tags <- intersect(names(block), ns_tags)
  lapply(tags, ns_process_tag, partitum = block)
}

ns_process_tag <- function(tag_name, partitum) {
  f <- get(paste0("ns_", tag_name), mode = "function")
  tags <- partitum[names(partitum) == tag_name]

  lapply(tags, f, part = partitum)
}

#' @export
roc_output.namespace <- function(roclet, results, base_path, options = list(),
                                 check = TRUE) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  results <- c(made_by("#"), results)

  write_if_different(NAMESPACE, results, check = check)

  NAMESPACE
}

#' @export
clean.namespace <- function(roclet, base_path) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  if (made_by_roxygen(NAMESPACE)) {
    unlink(NAMESPACE)
  }
}

# Functions that take complete partitum and return NAMESPACE lines
ns_export <- function(tag, part) {
  if (identical(tag, "")) {
    # FIXME: check for empty exports (i.e. no name)
    default_export(part$object, part)
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

ns_S3method          <- function(tag, part) {
  warning("@S3method is deprecated. Please use @export instead",
    call. = FALSE)
  export_s3_method(tag)
}
ns_exportClass       <- function(tag, part) export_class(tag)
ns_exportMethod      <- function(tag, part) export_s4_method(tag)
ns_exportPattern     <- function(tag, part) one_per_line("exportPattern", tag)
ns_import            <- function(tag, part) one_per_line("import", tag)
ns_importFrom        <- function(tag, part) repeat_first("importFrom", tag)
ns_importClassesFrom <- function(tag, part) repeat_first("importClassesFrom", tag)
ns_importMethodsFrom <- function(tag, part) repeat_first("importMethodsFrom", tag)
ns_useDynLib         <- function(tag, part) {
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
ns_rawNamespace       <- function(tag, part) tag

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
