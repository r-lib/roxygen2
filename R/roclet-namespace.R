#' @include roxygen.R
#' @include parse.R
#' @include roclet.R
#' @include string.R
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

#' Make a namespace roclet which parses the given files and writes a list of
#' namespace directives to a given file or standard out; see
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
#' @param roxygen.dir where to create roxygen output; defaults to
#' \file{package.roxygen}.
#' @param outfile whither to send output; blank string means standard out
#' @param verbose whether to anounce what we're doing with
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
#' roclet <- make.namespace.roclet()
#' \dontrun{roclet$parse('example.R')}
#' @export
#' @aliases make.namespace.roclet exportClass exportMethod
#' exportPattern S3method import importFrom importClassesFrom
#' importMethodsFrom export
make.namespace.roclet <- function(package.dir, roxygen.dir, outfile = NULL,
                                  verbose=TRUE) {
  if (is.null(outfile)) {
    outfile <- file.path(roxygen.dir, NAMESPACE.FILE)
  }

  namespace <- character()

  exportee <- NULL
  pre.parse <- function(partitum) {
    exportee <<- list(name=partitum$name,
                      assignee=partitum$assignee,
                      S4method=partitum$S4method,
                      S4generic=partitum$S4method,
                      S4class=partitum$S4class)
  }

  post.files <- function() { 
    new <- sort(unique(namespace))
    old <- readLines(outfile)
    
    if (!identical(new, old)) {
      message("Updating namespace directives")
      writeLines(new, outfile)
    }
  }

  record.directive <- function(proc, parms) {
    namespace <<- c(namespace, str_c(proc, "(", str_trim(parms), ")"))
  }
  
  parse.default <- function(proc, parms) {
    record.directive(proc, words(parms))
  }
  parse.exportClass <- function(proc, parms) {
    record.directive('exportClasses', parms)
  }
  parse.exportMethod <- function(proc, parms) {
    record.directive('exportMethods', parms)
  }
  parse.export <- function(proc, parms) {
    if (is.null.string(parms)) {
      if (!is.null(exportee$S4method))
        parse.exportMethod(NULL, exportee$S4method)
      else if (!is.null(exportee$S4class))
        parse.exportClass(NULL, exportee$S4class)
      else if (!is.null(exportee$S4generic))
        parse.exportMethod(NULL, exportee$S4generic)
      else {
        exportee <- first.non.null(exportee$name,
                                   exportee$assignee)
        if (is.null(exportee))
          warning('Empty export directive')
        else
          record.directive('export', quote_if_needed(exportee))
      }
    } else {
      record.directive('export', words(parms))
    }
  }
  
  parse.S3method <- function(proc, parms) {
    params <- words(parms)
    if (length(params) != 2) {
      warning("Invalid @S3method: ", parms, call. = FALSE)
    }
    record.directive(proc, str_c(params, collapse = ","))
  }
  parse.importFrom <- function(proc, parms) {
    params <- words(parms)
    record.directive(proc, str_c(params[1], ",", params[-1]))
  }


  roclet <- make.roclet(package.dir, roxygen.dir, 
    parse.default = parse.default,
    pre.parse = pre.parse,
    post.files = post.files)

  roclet$register.parser('S3method', parse.S3method)
  roclet$register.parser('importFrom', parse.importFrom)
  roclet$register.parser('export', parse.export)
  roclet$register.parser('exportClass', parse.exportClass)
  roclet$register.parser('exportMethod', parse.exportMethod)
  roclet$register.default.parsers('exportPattern',
    'import',
    'importClassesFrom',
    'importMethodsFrom',
    'useDynLib')

  roclet
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

