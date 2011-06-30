#' @include roxygen.R
#' @include list.R
#' @include parse.R
NULL

#' Abstract roclet that serves as a rudimentary API.
#'
#' Contains the following member functions:
#' \itemize{
#'   \item \code{register.parser}: takes \code{key} and \code{parser}
#'   \item \code{register.parsers}: takes \code{parser} and \code{keys}
#'   \item \code{register.default.parser}: takes a \code{key}
#    \item \code{register.default.parsers}: take \code{parsers}
#'   \item \code{parse} parses material contained in files
#' }
#'
#' @param parse.default the default parser taking \code{key} and \code{value}
#' @param pre.parse a callback function taking a list of parsed elements;
#'   called before processing a file
#' @param post.parse a callback function taking a list of parsed elements;
#'   called after processing a file
#' @param pre.files a callback function with no arguments; called before any
#'   file has been parsed
#' @param post.files a callback function with no arguments; called after every
#'   file has been parsed
#' @export
make.roclet <- function(package.dir,
                        roxygen.dir,
                        parse.default = NULL,
                        pre.parse=NULL,
                        post.parse=NULL,
                        pre.files=NULL,
                        post.files=NULL) {
  roclet <- new.env(parent=emptyenv())

  roclet$parsers <- list()

  #' Register parser in the parser table.
  #' @param key key upon which to register
  #' @param parser the parser to register
  #' @return \code{NULL}
  roclet$register.parser <- function(key, parser)
    roclet$parsers[[key]] <- parser

  #' Register many parsers at once.
  #' @param parser the parser to register
  #' @param \dots the keys under which to register
  #' @return \code{NULL}
  roclet$register.parsers <- function(parser, ...)
    for (key in c(...))
      roclet$register.parser(key, parser)

  #' Register a default parser.
  #' @param key key upon which to register
  #' @return \code{NULL}
  roclet$register.default.parser <- function(key)
    roclet$parsers[[key]] <- parse.default

  #' Register many default parsers.
  #' @param \dots the keys under which to register
  #' @return \code{NULL}
  roclet$register.default.parsers <- function(...)
    for (parser in c(...))
      roclet$register.default.parser(parser)

  roclet$parse <- function(paths) {
    parsed <- parse.files(paths)
    roclet$parse.parsed(parsed)
  }
    
    
  roclet$parse.dir <- function() {
    r.dir <- file.path(package.dir, R.DIR)
    files <- as.list(list.files(r.dir,
                                pattern='\\.(R|r)$',
                                recursive=TRUE,
                                full.names=TRUE,
                                all.files=TRUE))
    roclet$parse(files)
  }

  #' Parse material contained in files.
  #' @param partita the parsed elements
  #' (from e.g. \code{parse.files})
  #' @return \code{NULL}
  roclet$parse.parsed <- function(partita) {
    key.values <- function(partitum)
      zip.list(names(partitum), partitum)
    
    parse.noop <- function(key, value) NULL

    parser <- function(key)
      if (is.null(f <- roclet$parsers[[key]])) parse.noop else f

    maybe.call <- function(proc, ...)
      if (!is.null(proc))
        do.call(proc, list(...))

    maybe.call(pre.files)
    for (partitum in partita) {
      maybe.call(pre.parse, partitum)
      for (key.value in key.values(partitum)) {
        key <- key.value[[1]]
        do.call(parser(key), c(key, key.value[-1]))
      }
      maybe.call(post.parse, partitum)
    }
    maybe.call(post.files)
  }

  structure(roclet, class='roclet')
}

#' Find the first non-null argument.
#' @param \dots the arguments
#' @return The first non-null argument
first.non.null <- function(...)
  append(NULL, c(...))[[1]]

#' Pluck name from a hierarchy of candidates; viz. name,
#' assignee, S4class, S4method, S4generic.
#' @param partitum the parsed elements
#' @return The guessed name (possibly \code{NULL})
guess.name <- function(partitum)
  first.non.null(partitum$name,
                 partitum$assignee,
                 partitum$S4class,
                 partitum$S4method,
                 partitum$S4generic)

#' Extract the source code from parsed elements
#' @param partitum the parsed elements
#' @return The lines of source code
src.lines <- function(partitum) {
    srcfile <- srcfile(partitum$srcref$filename)
    first.line <- partitum$srcref$lloc[[1]]
    last.line <- partitum$srcref$lloc[[3]]
    getSrcLines(srcfile, first.line, last.line)
}

#' Extract the expression from the parse tree.
#' @param partitum the parsed elements
#' @return the extracted expression
#' @export
expression.from.partitum <- function(partitum)
  parse(text=src.lines(partitum))

