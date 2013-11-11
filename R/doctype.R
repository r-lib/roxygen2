default_doctype <- function(x) UseMethod("default_doctype")

#' @export
default_doctype.data <- function(x) "data"

#' @export
default_doctype.default <- function(x) NULL
