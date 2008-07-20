#' @include list.R
roclet <- function(parse.default) {
  roclet <- new.env(parent=emptyenv())

  roclet$parsers <- list()

  roclet$register.parser <- function(key, parser)
    roclet$parsers[[key]] <- parser

  roclet$register.default.parser <- function(key)
    roclet$parsers[[key]] <- parse.default

  roclet$parse <- function(...) {
    key.values <- function(partitum)
      zip.list(attributes(partitum)$names, partitum)
    
    parse.noop <- function(key, value) NULL

    parser <- function(key)
      if (is.null(f <- roclet$parsers[[key]])) parse.noop else f

    for (partitum in parse.files(...))
      for (key.value in key.values(partitum)) {
        key <- car(key.value)
        do.call(parser(key), list(key, cdr(key.value)))
      }
  }

  roclet
}
