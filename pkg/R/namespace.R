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
  
  roclet <- make.roclet(parse.directive)

  roclet$register.parser('exportClass',
                         function(proc, parms)
                         parse.directive('exportClasses', parms))
  roclet$register.parser('exportMethod',
                         function(proc, parms)
                         parse.directive('exportMethods', parms))

  roclet$register.default.parsers('export',
                                  'exportPattern',
                                  'S3method',
                                  'import',
                                  'importFrom',
                                  'importClassesFrom',
                                  'importMethodsFrom')

  roclet
}
