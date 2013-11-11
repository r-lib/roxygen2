default_doctype <- function(x) UseMethod("default_doctype")

#' @export
default_doctype.data <- function(x) "data"

#' @export
default_doctype.default <- function(x) NULL

#' @export
default_doctype.s4class <- function(x) "class"

#' @export
default_doctype.s4method <- function(x) "methods"