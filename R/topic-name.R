topic_name <- function(x) UseMethod("topic_name")

#' @S3method topic_name s4method
topic_name.s4method <- function(x) {
  sig <- str_c(x$value@defined, collapse = ",")
  str_c(x$value@generic, ",", sig, "-method")
}

#' @S3method topic_name s4class
topic_name.s4class <- function(x) {
  str_c(x$S4class, "-class")
}

#' @S3method topic_name rcclass
topic_name.rcclass <- function(x) {
  browser()
  str_c(x$def@className, "-ref-class")
}

#' @S3method topic_name default
topic_name.default <- function(x) {
  if (length(x$name) == 1) x$name
}
