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
#' @export
escape.NULL <- function(x) NULL
#' @export
escape.rd <- function(x) x
#' @export
escape.character <- function(x) {
  # wrap_usage uses \u{A0}, the unicode non-breaking space, which
  # is not necessarily valid in windows locales. useBytes is a quick
  # hack to fix the problem.
  x1 <- gsub(r"(\)", r"(\\)", x, fixed = TRUE, useBytes = TRUE)
  x2 <- gsub("%", r"(\%)", x1, fixed = TRUE, useBytes = TRUE)

  rd(x2)
}

# Works like paste, but automatically escapes all input variables,
# but not literal strings
build_rd <- function(..., collapse = NULL, sep = "") {
  args <- dots(...)
  env <- parent.frame()

  escaped <- lapply(args, function(arg) {
    if (is.character(arg)) {
      return(arg)
    }

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

get_tags <- function(rd, tag) {
  Filter(\(x) identical(attr(x, "Rd_tag"), tag), rd)
}

# helpers -----------------------------------------------------------------

parse_rd <- function(x) {
  con <- textConnection(x)
  on.exit(close(con), add = TRUE)

  tryCatch(
    tools::parse_Rd(con, fragment = TRUE, encoding = "UTF-8"),
    warning = function(cnd) NULL
  )
}

# Generated in .onLoad()
as_character_rd <- NULL
make_as_character_rd <- function() {
  # "as.character.Rd" appears to a few commands in TWOARGS
  # this code hacks the body of the function to add it
  fn <- internal_f("tools", "as.character.Rd")

  body <- body(fn)
  idx <- purrr::detect_index(
    body,
    \(x) is_call(x, "<-", 2) && is_symbol(x[[2]], "TWOARG")
  )
  if (idx == 0) {
    return(fn)
  }

  body[[idx]][[3]] <- call_modify(
    body[[idx]][[3]],
    "\\href",
    "\\ifelse",
    "\\if"
  )
  body(fn) <- body
  fn
}

has_topic <- function(topic, package) {
  tryCatch(
    {
      out <- exec("help", topic, package, .env = global_env())
      inherits(out, "dev_topic") ||
        (inherits(out, "help_files_with_topic") && length(out) == 1)
    },
    error = function(c) FALSE
  )
}
