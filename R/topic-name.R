topic_name <- function(x) UseMethod("topic_name")

#' @export
topic_name.s4method <- function(x) {
  sig <- str_c(x$value@defined, collapse = ",")
  str_c(x$value@generic, ",", sig, "-method")
}

#' @export
topic_name.s4class <- function(x) {
  str_c(x$value@className, "-class")
}

#' @export
topic_name.rcclass <- function(x) {
  str_c(x$value@className, "-ref-class")
}

#' @export
topic_name.default <- function(x) {
  if (length(x$name) != 1) return()
  
  x$name
}
