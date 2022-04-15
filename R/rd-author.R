
#' @export
merge.rd_section_author <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  # Remove duplicated authors, e.g. when using @rdname
  rd_section(x$type, unique(c(x$value, y$value)))
}
