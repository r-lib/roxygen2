"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Given argument list, produce usage string for it.
# 
# Adapted from \code{\link{prompt}}.
#
# @param f function, or name of function, as string
# @return a string
usage <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- deparse(arg, backtick = TRUE, width.cutoff = 500L)
    text <- str_replace_all(text, fixed("%"), "\\%")
    
    paste(" = ", paste(text, collapse = "\n"), sep = "")
  }

  arg_values <- vapply(args, arg_to_text, character(1))
  
  results <- paste(names(args), arg_values, sep = "")
  cuml <- cumsum(nchar(results))
  results <- split(results, cuml %/% 60)
  paste(lapply(results, paste, collapse = ", "), collapse = ", \n  ")
}

# Does the string contain no matter, but very well [:space:]?
# @param string the string to check
# @return TRUE if the string contains words, otherwise FALSE
is.null.string <- function(string) {
  str_length(str_trim(string)) == 0
}
