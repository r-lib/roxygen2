#' @include roclet.R
#' @include string.R
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
