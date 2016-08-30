#' Default format for data
#'
#' This function is called to generate the default "Format" section for each
#' data object.  The default implementation will return the class and dimension
#' information.
#'
#' @param x A data object
#' @return A \code{character} value with valid \code{Rd} syntax, or \code{NULL}.
#'
#' @export
default_data_format <- function(x) {
  UseMethod("default_data_format")
}

#' @export
default_data_format.default <- function(x) {
  paste0("An object of class ", format_classes(x), " ", format_dim(x), ".")
}

format_classes <- function(x) {
  classes <- paste0("\\code{", class(x), "}")

  base_classes <- NULL
  if (length(classes) > 1L) {
    base_classes <- paste0(
      " (inherits from ",
      paste(classes[-1L], collapse = ", "),
      ")")
  }

  paste0(classes[[1L]], base_classes)
}

format_dim <- function(x) {
  if (length(dim(x)) == 2L) {
    paste0("with ", nrow(x), " rows and ", ncol(x), " columns")
  } else if (length(dim(x)) > 2L) {
    paste0("of dimension ", paste(dim(x), collapse = " x "))
  } else {
    paste0("of length ", length(x))
  }
}
