default_topic_name <- function(x) UseMethod("default_topic_name")

#' @export
default_topic_name.s4method <- function(x) {
  sig <- x$value@defined

  # Trailing ANY's are dropped from the signature and need to be
  # re-added - see https://github.com/klutometis/roxygen/issues/460
  g_nargs <- length(methods::getGeneric(x$value@generic)@signature)
  m_nargs <- length(x$value@defined)
  if (m_nargs < g_nargs) {
    sig <- c(sig, rep("ANY", g_nargs - m_nargs))
  }

  sig <- paste0(sig, collapse = ",")
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
