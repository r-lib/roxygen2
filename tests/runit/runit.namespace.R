check.namespace.output <- Curry(check.output,
                                make.roclet=make.namespace.roclet)

test.export <- function()
  check.namespace.output("#' @export
                          a <- 2",
                         output='export(a)')

test.export.override <- function()
  check.namespace.output("#' @export b
                          a <- 2",
                         output='export(b)')

test.export.S4class <- function()
  check.namespace.output("#' @export
                          setClass('a')",
                         output='exportClasses(a)')

test.override.S4class <- function()
  check.namespace.output("#' @exportClass b
                          setClass('a')",
                         output='exportClasses(b)')

test.export.S4method <- function()
  check.namespace.output("#' @export
                          setMethod('b', 'a')",
                         output='exportMethods(b)')

test.override.S4method <- function()
  check.namespace.output("#' @exportMethod c
                          setMethod('b', 'a')",
                         output='exportMethods(c)')

test.keys <- function()
  check.namespace.output("#' @exportPattern test
                          #' @S3method test test
                          #' @import test
                          #' @importFrom test test
                          #' @importClassesFrom test test
                          #' @importMethodsFrom test test
                          roxygen()",
                         output=c("exportPattern(test)",
                           "S3method(test, test)",
                           "import(test)",
                           "importFrom(test, test)",
                           "importClassesFrom(test, test)",
                           "importMethodsFrom(test, test)"))
