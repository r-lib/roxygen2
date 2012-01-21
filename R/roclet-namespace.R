#' @include parse-registry.R
NULL

register.preref.parsers(parse.default, 'export')

register.preref.parsers(parse.value, 'exportClass', 'exportMethod',
  'exportPattern', 'S3method', 'import', 'importFrom', 'importClassesFrom',
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
  ns <- character()
  for (partitum in partita) {
    ns_one <- c( 
      process_tag(partitum, "export", ns_export),
      process_tag(partitum, "S3method", ns_S3method),
      process_tag(partitum, "importFrom", ns_collapse),
      process_tag(partitum, 'exportClass', ns_exportClass),
      process_tag(partitum, 'exportMethod', ns_exportMethod),
      process_tag(partitum, 'exportPattern', ns_default),
      process_tag(partitum, 'import', ns_default),
      process_tag(partitum, 'importClassesFrom', ns_collapse),
      process_tag(partitum, 'importMethodsFrom', ns_collapse),
      process_tag(partitum, 'useDynLib', ns_collapse)
    )
    ns <- c(ns, ns_one)
  }
  with_locale("C", sort(unique(ns)))
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


ns_directive <- function(tag, parms) {
  str_c(tag, "(", str_trim(parms), ")")
}

ns_default <- function(tag, parms, all) {
  ns_directive(tag, words(parms))
}
ns_collapse <- function(tag, parms, all) {
  params <- words(parms)
  if (length(params) == 1) {
    ns_directive(tag, params)
  } else {
    ns_directive(tag, str_c(params[1], ",", params[-1]))
  }
}

ns_exportClass <- function(tag, parms, all) {
  ns_directive('exportClasses', quote_if_needed(parms))
}
ns_exportMethod <- function(tag, parms, all) {
  ns_directive('exportMethods', quote_if_needed(parms))
}
ns_export <- function(tag, parms, all) {
  if (!is.null.string(parms)) {
    return(ns_directive('export', words(parms)))
  }
  
  if (all$src_type == "method") {
    ns_exportMethod(NULL, all$generic)
  } else if (all$src_type == "class") {
    ns_exportClass(NULL, all$src_name)
  } else if (all$src_type == "method") {
    ns_S3method(all$src_name)
  } else {
    name <- all$name %||% all$src_name
    if (is.null(name)) {
      warning('Empty export directive', call. = FALSE)
      NULL
    } else {
      ns_directive('export', quote_if_needed(name))
    }
  }
}
ns_S3method <- function(tag, parms, all) {
  params <- words(parms)
  if (length(params) != 2) {
    warning("Invalid @S3method: ", parms, call. = FALSE)
  }
  ns_directive("S3method", str_c(quote_if_needed(params), collapse = ","))
}


process_tag <- function(partitum, tag, f) {
  matches <- partitum[names(partitum) == tag]
  if (length(matches) == 0) return()
  
  unlist(lapply(matches, f, tag = tag, all = partitum), use.names = FALSE)
}
  
words <- function(x) {
  quote_if_needed(str_split(str_trim(x), "\\s+")[[1]])
}
is.syntactic <- function(x) make.names(x) == x
has.quotes <- function(x) str_detect(x, "'|\"")
quote_if_needed <- function(x) {
  needs_quotes <- !has.quotes(x) & !is.syntactic(x)
  x[needs_quotes] <- str_c('"', x[needs_quotes], '"')
  x
}

