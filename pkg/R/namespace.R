#' @include string.R
#' @include functional.R
#' @include list.R
#' Revamp to use noop; relevata-model unnecessary.
namespace <- function(partita) {
  parse.default <- function(proc, parms)
    cat(sprintf('%s(%s)\n', proc, strmap(Identity, ', ', parms)))
  
  parsers <- list(exportClass=function(proc, parms)
                  parse.default('exportClasses', parms),
                  exportMethod=function(proc, parms)
                  parse.default('exportMethods', parms),
                  export=parse.default,
                  exportPattern=parse.default,
                  S3method=parse.default,
                  import=parse.default,
                  importFrom=parse.default,
                  importClassesFrom=parse.default,
                  importMethodsFrom=parse.default)

  parse.noop <- function(procedure, parameters) NULL

  parser <- function(procedure)
    if (is.null(f <- parsers[[procedure]])) parse.noop else f

  for (partitum in partita)
    for (key.value in key.values(partitum)) {
      key <- car(key.value)
      do.call(parser(key), list(key, cdr(key.value)))
    }
}
