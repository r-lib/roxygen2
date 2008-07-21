#' @include list.R
roxygen()

#' Abstract roclet that serves as a rudimentary API.
#'
#' Contains the following member functions:
#' \item{register.parser}{takes \code{key} and \code{parser}}
#' \item{register.parsers}{takes \code{parser} and \code{keys}}
#' \item{register.default.parser}{takes a \code{key}}
#' \item{register.default.parsers}{take \code{parsers}}
#' \item{parse}{parses material returned by \code{parse.files}}
#'
#' @param parse.default the default parser taking \code{key}
#' and \code{value}
#' @param pre.parse a callback function taking a list of parsed
#' elements; called before processing a file
#' @param post.parse a callback function taking a list of parsed
#' elements; called after processing a file
#' @param pre.files a callback function with no arguments;
#' called before any file has been parsed
#' @param post.files a callback function with no arguments;
#' called after every file has been parsed
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

#' Assign a variable in the parent environment when \code{<<-}
#' doesn't see to work.
#' @param var string of the variable to assign
#' @param value value to be assigned
#' @param env environment of the assignment (\code{environment()})
#' @return NULL
assign.parent <- function(var, value, env)
  assign(var, value, envir=parent.env(env))
