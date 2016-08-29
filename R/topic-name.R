default_topic_name <- function(x) UseMethod("default_topic_name")

#' @export
default_topic_name.s4method <- function(x) {
  sig <- paste0(x$value@defined, collapse = ",")
  paste0(x$value@generic, ",", sig, "-method")
}

#' @export
default_topic_name.s4class <- function(x) {
  paste0(x$value@className, "-class")
}

#' @export
default_topic_name.rcclass <- function(x) {
  paste0(x$value@className, "-class")
}

#' @export
default_topic_name.rcmethod <- function(x) {
  x@name
}

#' @export
default_topic_name.default <- function(x) {
  if (length(x$alias) == 1) return(x$alias)
  default_name(x)
}
