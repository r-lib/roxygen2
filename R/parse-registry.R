if (!exists("preref.parsers")) {
  # Preref parser table
  preref.parsers <- new.env(parent=emptyenv())
}

#' Register parsers.
#'
#' @param key the key upon which to register
#' @param parser the parser callback to register;
#' a function taking \code{key} and \code{expression}
#' @return \code{NULL}
#' @export
#' @keywords internal
#' @rdname register-parser
register.preref.parser <- function(key, parser) {
  preref.parsers[[key]] <- parser
}

#' Register many parsers at once.
#'
#' @param parser the parser to register
#' @param \dots the keys upon which to register
#' @return \code{NULL}
#' @export
#' @keywords internal
#' @rdname register-parsers
register.preref.parsers <- function(parser, ...) {
  for (key in c(...)) {
    register.preref.parser(key, parser)
  }
}
