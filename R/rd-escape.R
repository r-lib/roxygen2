# A simple object to represent rd escaped text.
rd <- function(x) {
  structure(x, class = "rd")
}

#' @export
c.rd <- function(...) {
  rd(unlist(lapply(list(...), escape), use.names = FALSE))
}

#' @export
print.rd <- function(x, ...) {
  out <- paste0("<rd> ", x, collapse = "\n")
  cat(out)
}

escape <- function(x) UseMethod("escape")
#' @export
escape.rd <- function(x) x
#' @export
escape.character <- function(x) {
  # wrap_string uses \u{A0}, the unicode non-breaking space, which
  # is not necessarily valid in windows locales. useBytes is a quick
  # hack to fix the problem.
  x1 <- gsub("\\", "\\\\", x, fixed = TRUE, useBytes = TRUE)
  x2 <- gsub("%", "\\%", x1, fixed = TRUE, useBytes = TRUE)

  rd(x2)
}

# Works like escape, but unescapes special rd example commands
escape_examples <- function(x) {
  gsub("\\\\dont", "\\dont", escape(x))
}

escape_preformatted <- function(x) {
  x1 <- escape(x)
  x2 <- gsub("{", "\\{", x1, fixed = TRUE)
  x3 <- gsub("}", "\\}", x2, fixed = TRUE)

  rd(x3)
}


# Works like paste, but automatically escapes all input variables,
# but not literal strings
build_rd <- function(..., collapse = NULL, sep = "") {
  args <- dots(...)
  env <- parent.frame()

  escaped <- lapply(args, function(arg) {
    if (is.character(arg)) return(arg)

    escape(eval(arg, env))
  })

  string <- do.call("paste", c(escaped, list(collapse = collapse, sep = sep)))
  rd(string)
}
