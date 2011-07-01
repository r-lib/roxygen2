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
make.roclet <- function(package.dir, process, output) {
  roclet <- new.env(parent = emptyenv())

  # roclet$parsers <- list()
  # 
  # #' Register parser in the parser table.
  # #' @param key key upon which to register
  # #' @param parser the parser to register
  # #' @return \code{NULL}
  # roclet$register.parser <- function(key, parser)
  #   roclet$parsers[[key]] <- parser
  # 
  # #' Register many parsers at once.
  # #' @param parser the parser to register
  # #' @param \dots the keys under which to register
  # #' @return \code{NULL}
  # roclet$register.parsers <- function(parser, ...)
  #   for (key in c(...))
  #     roclet$register.parser(key, parser)
  # 
  # #' Register a default parser.
  # #' @param key key upon which to register
  # #' @return \code{NULL}
  # roclet$register.default.parser <- function(key)
  #   roclet$parsers[[key]] <- parse.default
  # 
  # #' Register many default parsers.
  # #' @param \dots the keys under which to register
  # #' @return \code{NULL}
  # roclet$register.default.parsers <- function(...)
  #   for (parser in c(...))
  #     roclet$register.default.parser(parser)

  roclet$parse <- function(paths) {
    parsed <- parse.files(paths)
    contents <- Filter(function(x) length(x) > 1, parsed)
    results <- process(contents)
    output(results)
  }

  roclet$parse.dir <- function(path = file.path(package.dir, "R")) {
    files <- as.list(dir(path, pattern = '\\.(R|r)$', full.names = TRUE))
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
