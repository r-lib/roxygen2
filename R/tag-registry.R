tags <- new.env(parent = emptyenv())

parse_tag <- function(x) {
  stopifnot(is.roxygen_tag(x))

  if (!(x$tag %in% ls(tags))) {
    return(tag_warning(x, "unknown tag"))
  }

  tags[[x$tag]](x)
}

#' Register tag parsers.
#'
#' @param ... List of name-parser pairs
#' @return \code{NULL}
#' @export
#' @keywords internal
#' @rdname register-parser
#' @usage NULL
register.preref.parser <- function(key, parser) {
  warning("Deprecaated: please use register_tag instead", call. = FALSE)
  tags[[key]] <- parser
}

#' @export
#' @rdname register-parser
#' @usage NULL
register.preref.parsers <- function(parser, ...) {
  warning("Deprecaated: please use register_tag instead", call. = FALSE)
  for (key in c(...)) {
    register.preref.parser(key, parser)
  }
}

#' @export
#' @rdname register-parser
register_tags <- function(...) {
  list2env(list(...), tags)
}
