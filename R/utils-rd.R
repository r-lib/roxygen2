# Output ------------------------------------------------------------------

# A simple object to represent rd escaped text.
rd <- function(x) {
  structure(x, class = "rd")
}

#' @export
c.rd <- function(...) {
  rd(NextMethod())
}

#' @export
print.rd <- function(x, ...) {
  out <- paste0("<rd> ", x, collapse = "\n")
  cat(out)
}

escape <- function(x) UseMethod("escape")
escape.rd <- function(x) x
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

dots <- function(...) {
  eval(substitute(alist(...)))
}

# Translate a field and values into an Rd macro.
# Multiple values get their own braces.
rd_macro <- function(field, ..., space = FALSE) {
  if (space) {
    values <- paste0("\n", paste0(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }

  paste0("\\", field, paste0("{", values, "}", collapse = ""))
}


# Input -------------------------------------------------------------------

get_rd <- function(topic, package = NULL) {
  help_call <- substitute(help(t, p), list(t = topic, p = package))
  top <- eval(help_call)
  if (length(top) == 0) return(NULL)

  internal_f("utils", ".getHelpFile")(top)
}

get_tags <- function(rd, tag) {
  Filter(function(x) identical(attr(x, "Rd_tag"), tag), rd)
}

rd2text <- function(x) {
  chr <- as.character(structure(x, class = "Rd"), deparse = TRUE)
  paste(chr, collapse = "")
}

