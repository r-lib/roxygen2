#' @include parse-registry.R
NULL

register.preref.parsers(parse.default, 'export')
register.preref.parsers(parse.words, 'exportClass', 'exportMethod',
  'exportPattern', 'S3method', 'import', 'importFrom', 'importClassesFrom',
  'importMethodsFrom', 'useDynLib')

ns_tags <- c('export', 'exportClass', 'exportMethod', 'exportPattern',
  'S3method', 'import', 'importFrom', 'importClassesFrom', 
  'importMethodsFrom', 'useDynLib')

#' Roclet: make NAMESPACE.
#'
#' This roclet automates the production of a \file{NAMESPACE} file,
#' see \cite{Writing R Extensions}
#' (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf}) for details.
#'
#' @section Tags:
#'
#' There are four tags for exporting objects from the package:
#'
#' \describe{
#'
#' \item{\code{@@export}}{Roxygen guesses the directive: \code{export} for
#'   functions, \code{exportMethod} for S4 methods, \code{S3method} for S3
#'   methods, \code{exportClass} for S4 classes.
#'
#'   This is the only directive you should need for documented function,
#'   the other directives are useful if you want to export (e.g.) methods
#'   but not document them.}
#'
#' \item{\code{@@export f g ...}}{overrides auto-detection and
#'   produces multiple export directives: \code{export(f)}, \code{export(g)}
#'   ...}
#'
#' \item{\code{@@exportClass x}}{produces \code{exportClasses(x)} directive.}
#'
#' \item{\code{@@exportMethod x}}{produces \code{exportMethods(x)} directive.}
#'
#' \item{\code{@@S3method generic class}}{produces
#'    \code{S3method(generic,class)} directive}
#'
#' }
#'
#' There are five tags for importing objects into the package:
#'
#' \describe{
#'
#' \item{\code{@@import package}}{produces \code{import(package) directive
#'   to import all functions from the given package}}
#'
#' \item{\code{@@importFrom package functiona functionb ...}}{produces
#'    multiple \code{importFrom(package, function)} directives to import
#'    selected functions from a package.}
#'
#' \item{\code{@@importClassesFrom package classa classb ...}}{produces
#'   multiple \code{importClassesFrom(package, class)} directives to import
#'   selected classes from a package.}
#'
#' \item{\code{@@importMethodsFrom package methoda methodb ...}}{produces
#'   multiple \code{importMethodsFrom(package, method)} directives to import
#'   selected methods from a package.}
#'
#' \item{\code{@@useDynLib package}}{produces a \code{useDynLib(package)}
#'   directive to import all compiled routines from the shared objects in
#'   the specified package}
#'
#' \item{\code{@@useDynLib paackage routinea routineb}}{produces multiple
#'   \code{useDynLib(package,routine)} directions to import specified
#'   compiled routines from a package.}
#' }
#'
#' Only unique directives are saved to the \file{NAMESPACE} file, so you can
#' repeat them as needed to maintain a close link between the functions where
#' they are needed and the namespace file..
#'
#' @family roclets
#' @examples
#' #' An example file, example.R, which imports
#' #' packages foo and bar
#' #' @@import foo bar
#' NULL
#'
#' #' An exportable function
#' #' @@export
#' fun <- function() {}
#'
#' roclet <- namespace_roclet()
#' \dontrun{roc_proc(roclet, "example.R")}
#' \dontrun{roc_out(roclet, "example.R", ".")}
#' @export
#' @aliases export exportClass exportMethod S3method import importFrom
#'   importClassesFrom importMethodsFrom
namespace_roclet <- function() {
  new_roclet(list, "namespace")
}

#' @S3method roc_process namespace
roc_process.namespace <- function(roclet, partita, base_path) {
  ns <- unlist(lapply(partita, ns_process_partitum))
  with_locale("C", sort(unique(ns)))
}

ns_process_partitum <- function(partitum) {
  tags <- intersect(names(partitum), ns_tags)
  unlist(lapply(tags, ns_process_tag, partitum = partitum))
}

ns_process_tag <- function(tag_name, partitum) {
  f <- match.fun(paste0("ns_", tag_name))
  tags <- partitum[names(partitum) == tag_name]
  
  lapply(tags, f, part = partitum)
}

#' @S3method roc_output namespace
roc_output.namespace <- function(roclet, results, base_path) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  
  old <- if (file.exists(NAMESPACE)) readLines(NAMESPACE) else ""
  
  if (!identical(results, old)) {
    cat("Updating namespace directives\n")
    writeLines(results, NAMESPACE)
  }
}

# Functions that take complete partitum and return NAMESPACE lines
ns_export <- function(tag, part) {
  if (!is.null.string(tag)) return(export(tag))
  # FIXME: check for empty exports (i.e. no name)
  
  default_export(part$object)
}
default_export <- function(x) UseMethod("default_export")
default_export.s4class   <- function(x) export_class(x$name)
default_export.s4generic <- function(x) export(x$name)
default_export.s4method  <- function(x) export_s4_method(x$name)
default_export.s3generic <- function(x) export(x$name)
default_export.s3method  <- function(x) export_s3_method(attr(x$value, "s3method"))
default_export.function  <- function(x) export(x$name)
default_export.data      <- function(x) export(x$name)
default_export.rcclass   <- function(x) {
  c(export(x$name), export_class(x$name))
}

ns_S3method          <- function(tag, part) export_s3_method(tag)
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
  x[needs_quotes] <- str_c('"', x[needs_quotes], '"')
  x
}
is.syntactic <- function(x) make.names(x) == x
has.quotes <- function(x) str_detect(x, "'|\"")
