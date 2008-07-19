#' @include string.R
namespace <- function(partita) {
  parse.default <- function(procedure, parameters)
    sprintf('%s(%s)', procedure, gsub(' +', ', ', parameters))
  
  parse.exportClass <- function(procedure, parameters)
    parse.default('exportClasses', parameters)

  parse.exportMethod <- function(procedure, parameters)
    parse.default('exportMethods', parameters)

  parsers <- list(exportClass=parse.exportClass,
                  exportMethod=parse.exportMethod)

  relevators <- c('export',
                  'exportClass',
                  'exportMethod',
                  'exportPattern',
                  'S3method',
                  'import',
                  'importFrom',
                  'importClassesFrom',
                  'importMethodsFrom')
  relevata <-
    Reduce(append,
           Map(function(partitum)
               Filter(function(key.value) car(key.value) %in% relevators,
                      zip.list(attributes(partitum)$names,
                               partitum)),
               partita),
           NULL)
  directives <-
    Map(function(relevatum) {
      procedure <- car(relevatum)
      parameters <- cadr(relevatum)
      parser <- parsers[[procedure]]
      if (is.null(parser))
        parse.default(procedure, parameters)
      else
        parser(procedure, parameters)
    },
        relevata)
  for (directive in directives)
    cat(directive, "\n")
}
