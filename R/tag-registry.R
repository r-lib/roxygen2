#' @include tag-parsers.R
NULL

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
#' Use this if you're building your own roclet that uses new tags.
#'
#' @param ... List of name-parser pairs
#' @export
#' @keywords internal
register_tags <- function(...) {
  list2env(list(...), tags)
}
