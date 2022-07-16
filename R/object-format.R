#' Default format for data
#'
#' This function is called to generate the default "Format" section for each
#' data object. The default implementation will return the class and dimension
#' information.
#'
#' @param x A data object
#' @return A `character` value with valid `Rd` syntax, or `NULL`.
#' @keywords internal
#' @export
object_format <- function(x) {
  UseMethod("object_format")
}

#' @export
object_format.default <- function(x) {
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

# helpers -----------------------------------------------------------------

# used for testing
call_to_format <- function(code, env = pkg_env()) {
  obj <- call_to_object(!!enexpr(code), env)
  object_format(obj$value)
}
