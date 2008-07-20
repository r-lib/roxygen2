#' @include list.R
make.roclet <- function(parse.default,
                        pre.parse=NULL,
                        post.parse=NULL,
                        pre.files=NULL,
                        post.files=NULL) {
  roclet <- new.env(parent=emptyenv())

  roclet$parsers <- list()

  roclet$register.parser <- function(key, parser)
    roclet$parsers[[key]] <- parser

  roclet$register.parsers <- function(parser, ...)
    for (key in c(...))
      roclet$register.parser(key, parser)

  roclet$register.default.parser <- function(key)
    roclet$parsers[[key]] <- parse.default

  roclet$register.default.parsers <- function(...)
    for (parser in c(...))
      roclet$register.default.parser(parser)

  roclet$parse <- function(...) {
    key.values <- function(partitum)
      zip.list(attributes(partitum)$names, partitum)
    
    parse.noop <- function(key, value) NULL

    parser <- function(key)
      if (is.null(f <- roclet$parsers[[key]])) parse.noop else f

    maybe.call <- function(proc, ...)
      if (!is.null(proc))
        do.call(proc, list(...))

    maybe.call(pre.files)
    for (partitum in parse.files(...)) {
      maybe.call(pre.parse, partitum)
      for (key.value in key.values(partitum)) {
        key <- car(key.value)
        do.call(parser(key), c(key, cdr(key.value)))
      }
      maybe.call(post.parse, partitum)
    }
    maybe.call(post.files)
  }

  structure(roclet, class='roclet')
}

assign.parent <- function(var, value, env)
  assign(var, value, envir=parent.env(env))
