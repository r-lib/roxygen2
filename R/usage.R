usage_s4_method <- function(x) {
  signature <- str_c(as.character(x@defined), collapse = ",")
  formals <- formals(x@.Data)  
  
  str_c("\\S4method{", x@generic, "}{", signature, "}(",
    args_string(usage_args(formals)), ")")
}


# Given argument list, produce usage specification for it.
#
# Adapted from \code{\link{prompt}}.
#
# @param f function, or name of function, as string
# @return a string
usage_args <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- deparse(arg, backtick = TRUE, width.cutoff = 500L)
    text <- str_replace_all(text, fixed("%"), "\\%")
    text <- str_replace_all(text, fixed(" "), "\u{A0}")
    Encoding(text) <- "UTF-8"
    
    text
  }
  vapply(args, arg_to_text, character(1))
}

args_string <- function(x) {
  missing_arg <- x == ""
  sep <- ifelse(!missing_arg, "\u{A0}=\u{A0}", "")
  
  str_c(names(x), sep, x, collapse = ", ")
}

