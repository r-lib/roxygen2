#' Given argument list, produce usage string for it.
#' 
#' Adapted from \code{\link{prompt}}.
#'
#' @param f function, or name of function, as string
#' @return a string
usage <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- deparse(arg, backtick = TRUE, width.cutoff = 500L)
    
    paste("=", paste(text, collapse = "\n"), sep = "")
  }

  arg_values <- vapply(args, arg_to_text, character(1))
  
  paste(names(args), arg_values, collapse = ", ", sep = "")
}

# Just in case we do need more escaping
# default <- gsub("\\\\", "\\\\\\\\", cadr(name.default))
# default <- gsub("([%])", "\\\\\\1", default)
