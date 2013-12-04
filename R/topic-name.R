default_topic_name <- function(x) UseMethod("default_topic_name")

#' @export
default_topic_name.s4method <- function(x) {
  sig <- str_c(x$value@defined, collapse = ",")
  str_c(x$value@generic, ",", sig, "-method")
}

#' @export
default_topic_name.s4class <- function(x) {
  str_c(x$value@className, "-class")
}

#' @export
default_topic_name.rcclass <- function(x) {
  str_c(x$value@className, "-class")
}

#' @export
default_topic_name.default <- function(x) {
  if (length(x$name) != 1) return()
  
  x$name
}
