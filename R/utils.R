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
    text <- str_replace_all(text, fixed(" "), "\u{A0}")
    Encoding(text) <- "UTF-8"    
    
    str_c("\u{A0}=\u{A0}", paste(text, collapse = "\n"))
  }

  arg_values <- vapply(args, arg_to_text, character(1))
  
  paste(names(args), arg_values, collapse = ", ", sep = "")
}

# Does the string contain no matter, but very well [:space:]?
# @param string the string to check
# @return TRUE if the string contains words, otherwise FALSE
is.null.string <- function(string) {
  str_length(str_trim(string)) == 0
}


subs <- matrix(ncol = 2, byrow = T, c(
  '[]', 'sub',
  '<-', 'set',
  '!', 'not',
  '"', 'quote',
  '#', 'hash',
  '$', 'cash',
  '%', 'grapes',
  '&', 'and',
  '|', 'or',
  "'", 'single-quote',
  '(', 'open-paren',
  ')', 'close-paren',
  '*', 'star',
  '+', 'plus',
  ',', 'comma',
  '/', 'slash',
  ':', 'colon',
  ';', 'semi-colon',
  '<', 'less-than',
  '=', 'equals',
  '>', 'greater-than',
  '?', 'p',
  '@', 'at',
  '[', 'open-brace',
  '\\', 'backslash',
  ']', 'close-brace',
  '^', 'hat',
  '`', 'tick',
  '{', 'open-curly',
  '}', 'close',
  '~', 'twiddle'
))
subs[, 2] <- str_c("-", subs[, 2])

nice_name <- function(x) {
  for(i in seq_len(nrow(subs))) {
    x <- str_replace_all(x, fixed(subs[i, 1]), subs[i, 2])
  }
  x <- str_replace(x, "-+", "-")
  x
}


roxygen_stop <- function(..., srcref = NULL) {
  stop(..., srcref_location(srcref), call. = FALSE)
}

roxygen_warning <- function(..., srcref = NULL) {
  warning(..., srcref_location(srcref), call. = FALSE)
}

srcref_location <- function(srcref = NULL) {
  if (is.null(srcref)) return()
  str_c(" in block ", basename(srcref$filename), ":", srcref$lloc[1])
}
