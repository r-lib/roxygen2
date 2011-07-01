#' @include parse.R
NULL

register.preref.parsers(parse.default,
                        'export')

register.preref.parsers(parse.value,
                        'exportClass',
                        'exportMethod',
                        'exportPattern',
                        'S3method',
                        'import',
                        'importFrom',
                        'importClassesFrom',
                        'importMethodsFrom',
                        'useDynLib')

#' Namespace roclet builds NAMESPACE.
#' 
#' \cite{Writing R Extensions}
#' (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf}) for details.
#'
#' The namespace roclet supports the following tags:
#'
#' \tabular{ll}{
#' Roxygen tag \tab \file{NAMESPACE} equivalent\cr
#' \code{@@export} \tab \code{export}\cr
#' \code{@@exportClass} \tab \code{exportClasses}\cr
#' \code{@@exportMethod} \tab \code{exportMethod}\cr
#' \code{@@exportPattern} \tab \code{exportPattern}\cr
#' \code{@@S3method} \tab \code{S3method}\cr
#' \code{@@import} \tab \code{import}\cr
#' \code{@@importFrom} \tab \code{importFrom}\cr
#' \code{@@importClassesFrom} \tab \code{importClassesFrom}\cr
#' \code{@@importMethodsFrom} \tab \code{importMethodsFrom}\cr
#' }
#'
#' \enumerate{
#' \item \code{@@export}: May be specified with or without value;
#'                       if unadorned, roxygen will try to guess
#'                       the exported value by assignee, \code{setMethod},
#'                       \code{setClass}, etc. Otherwise,
#'                       \code{@@export f g ...}
#'                       translates to
#'                       \code{export(f, g, ...)}.
#' \item \code{@@exportClass}: Overrides \code{setClass}.
#' \item \code{@@exportMethod}: Overrides \code{setMethod} or \code{setGeneric}.
#' \item \code{@@exportPattern}: See \dQuote{1.6.2 Registering S3 methods} from
#'                               \cite{Writing R Extensions}.
#' \item \code{@@S3method}: Overrides the export of an S3 method.
#' \item \code{@@import}: See \dQuote{1.6.1 Specifying imports and exports}
#'                        from \cite{Writing R Extensions}.
#' \item \code{@@importFrom}: See \dQuote{1.6.1 Specifying imports and exports}
#'                            from \cite{Writing R Extensions}.
#' \item \code{@@importClassesFrom}: See \dQuote{1.6.6 Name spaces with formal
#'                                   classes and methods} from \cite{Writing R
#'                                   Extensions}.
#' \item \code{@@importMethodsFrom}: See \dQuote{1.6.6 Name spaces with formal
#'                                   classes and methods} from \cite{Writing R
#'                                   Extensions}.
#' }
#'
#' @param package.dir the package's top directory
#' the \var{outfile}
#' @return Namespace roclet
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
#' \dontrun{roclet$parse('example.R')}
#' @export
#' @aliases namespace_roclet exportClass exportMethod
#' exportPattern S3method import importFrom importClassesFrom
#' importMethodsFrom export
namespace_roclet <- function(package.dir) {

  directive <- function(tag, parms) {
    str_c(tag, "(", str_trim(parms), ")")
  }
  
  default <- function(tag, parms, all) {
    directive(tag, words(parms))
  }
  exportClass <- function(tag, parms, all) {
    directive('exportClasses', parms)
  }
  exportMethod <- function(tag, parms, all) {
    directive('exportMethods', parms)
  }
  export <- function(tag, parms, all) {
    if (!is.null.string(parms)) {
      return(directive('export', words(parms)))
    }
    
    if (!is.null(all$S4method)) {
      exportMethod(NULL, all$S4method)
    } else if (!is.null(all$S4class)) {
      exportClass(NULL, all$S4class)
    } else if (!is.null(all$S4generic)){
      exportMethod(NULL, all$S4generic)
    } else if (!is.null(all$method)) {
      directive("S3method", str_c(unlist(all$method), collapse = ","))
    } else {
      name <- all$name %||% all$assignee
      if (is.null(name)) {
        warning('Empty export directive')
      } else {
        directive('export', quote_if_needed(name))
      }
    }
  }
  S3method <- function(tag, parms, all) {
    params <- words(parms)
    if (length(params) != 2) {
      warning("Invalid @S3method: ", parms, call. = FALSE)
    }
    directive(tag, str_c(params, collapse = ","))
  }
  importFrom <- function(tag, parms) {
    params <- words(parms)
    directive(tag, str_c(params[1], ",", params[-1]))
  }

  process <- function(partita) {
    ns <- character()
    for (partitum in partita) {
      ns_one <- c( 
        process_tag(partitum, "export", export),
        process_tag(partitum, "S3method", S3method),
        process_tag(partitum, "importFrom", importFrom),
        process_tag(partitum, 'export', export),
        process_tag(partitum, 'exportClass', exportClass),
        process_tag(partitum, 'exportMethod', exportMethod),
        process_tag(partitum, 'exportPattern', default),
        process_tag(partitum, 'import', default),
        process_tag(partitum, 'importClassesFrom', default),
        process_tag(partitum, 'importMethodsFrom', default),
        process_tag(partitum, 'useDynLib', default)
      )
      ns <- c(ns, ns_one)
    }
    ns
  }
  
  process_tag <- function(partitum, tag, f) {
    params <- partitum[[tag]]
    if (is.null(params)) return()
    
    f(tag, params, partitum)
  }
  
  output <- function(results) { 
    NAMESPACE <- file.path(package.dir, "NAMESPACE")
    
    new <- sort(unique(results))
    old <- readLines(NAMESPACE)
    
    if (!identical(new, old)) {
      message("Updating namespace directives")
      writeLines(new, NAMESPACE)
    }
  }
  
  make.roclet(package.dir, process, output)

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

