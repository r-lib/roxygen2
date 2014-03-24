if (!exists("preref.parsers")) {
  # Preref parser table
  preref.parsers <- new.env(parent=emptyenv())
}

#' Register parsers.
#'
#' @param key,... Tag name(s).
#' @param parser Parser callback to register, a function with arguments
#'   \code{key} and \code{expression}.
#' @return \code{NULL}
#' @export
#' @keywords internal
#' @rdname register-parser
register.preref.parser <- function(key, parser) {
  preref.parsers[[key]] <- parser
}

#' @export
#' @rdname register-parser
register.preref.parsers <- function(parser, ...) {
  for (key in c(...)) {
    register.preref.parser(key, parser)
  }
}
