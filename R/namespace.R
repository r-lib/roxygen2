#' @include roclet.R
#' @include string.R
make.namespace.roclet <- function() {
  parse.directive <- function(proc, parms)
    cat(sprintf('%s(%s)\n', proc, strmap(Identity, ', ', parms)))
  
  roclet <- roclet(parse.directive)

  roclet$register.parser('exportClass',
                                   function(proc, parms)
                                   default.parse('exportClasses', parms))
  roclet$register.parser('exportMethod',
                                   function(proc, parms)
                                   default.parse('exportMethods', parms))

  roclet$register.default.parser('export')
  roclet$register.default.parser('exportPattern')
  roclet$register.default.parser('S3method')
  roclet$register.default.parser('import')
  roclet$register.default.parser('importFrom')
  roclet$register.default.parser('importClassesFrom')
  roclet$register.default.parser('importMethodsFrom')

  roclet
}
