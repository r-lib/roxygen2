#' @include roxygen.R
#' @include parse.R
#' @include roclet.R
#' @include string.R
roxygen()

#' Make a namespace roclet which parses the given files writes a list of
#' namespace directives to standard out.
#'
#' Contains the member function \code{parse} which parses the result
#' of \code{parse.files}.
#'
#' @return Namespace roclet
make.namespace.roclet <- function() {
  parse.directive <- function(proc, parms)
    cat(sprintf('%s(%s)\n', proc, strmap(Identity, ', ', parms)))
  
  exportee <- NULL

  pre.parse <- function(partitum)
    exportee <<- list(name=partitum$name,
                      assignee=partitum$assignee,
                      S4method=partitum$S4method,
                      S4generic=partitum$S4method,
                      S4class=partitum$S4class)

  roclet <- make.roclet(parse.directive,
                        pre.parse)

  parse.exportClass <- function(proc, parms)
    parse.directive('exportClasses', parms)

  roclet$register.parser('exportClass', parse.exportClass)

  parse.exportMethod <- function(proc, parms)
    parse.directive('exportMethods', parms)

  roclet$register.parser('exportMethod', parse.exportMethod)

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
          parse.directive('export', exportee)
      }
    } else {
      parse.directive('export', parms)
    }
  }

  roclet$register

  roclet$register.parser('export', parse.export)

  roclet$register.default.parsers('exportPattern',
                                  'S3method',
                                  'import',
                                  'importFrom',
                                  'importClassesFrom',
                                  'importMethodsFrom')

  roclet
}
