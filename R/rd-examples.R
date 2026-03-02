#' @export
roxy_tag_parse.roxy_tag_examples <- function(x) {
  tag_examples(x)
}
#' @export
roxy_tag_parse.roxy_tag_examplesIf <- function(x) {
  lines <- unlist(strsplit(x$raw, "\r?\n"))

  condition <- lines[1]
  tryCatch(
    suppressWarnings(parse(text = condition)),
    error = function(err) {
      warn_roxy_tag(x, "condition failed to parse", parent = err)
    }
  )

  x$raw <- paste(
    c(
      paste0("\\dontshow{if (", condition, ") withAutoprint(\\{ # examplesIf}"),
      lines[-1],
      "\\dontshow{\\}) # examplesIf}"
    ),
    collapse = "\n"
  )

  tag_examples(x)
}
#' @export
roxy_tag_parse.roxy_tag_example <- function(x) {
  x <- tag_value(x)

  nl <- str_count(x$val, "\n")
  if (any(nl) > 0) {
    warn_roxy_tag(
      x,
      c(
        "must be a single line",
        i = "Do you want @examples?"
      )
    )
    return()
  }

  x
}

#' @export
roxy_tag_rd.roxy_tag_examples <- function(x, base_path, env) {
  rd_section("examples", x$val)
}
#' @export
roxy_tag_rd.roxy_tag_examplesIf <- function(x, base_path, env) {
  rd_section("examples", x$val)
}
#' @export
roxy_tag_rd.roxy_tag_example <- function(x, base_path, env) {
  path <- file.path(base_path, x$val)
  if (!file.exists(path)) {
    warn_roxy_tag(x, "{.path {path}} doesn't exist")
    return()
  }

  code <- read_lines(path)
  rd_section("examples", escape_examples(code))
}

#' @export
format.rd_section_examples <- function(x, ...) {
  value <- paste0(x$value, collapse = "\n")
  rd_macro(x$type, value, space = TRUE)
}

#' Escape examples
#'
#' This documentation topic is used primarily for testing and to record
#' our understanding of the `\example{}` escaping rules.
#' See <https://developer.r-project.org/parseRd.pdf> for the details provided
#' by R core.
#'
#' @keywords internal
#' @export
#' @examples
#' # In examples we automatically escape Rd comments (%):
#' 100 %% 30
#' # even if they are in strings
#' "50%"
#'
#' # and \\ and \v inside of strings and symbols
#' "\v" # vertical tab
#' "\\"
#' # but not comments: \l \v
#'
#' # other string escapes are left as is
#' "\""
#' "\n"
#'
#' # Otherwise, backslashes and parentheses are left as is. This
#' # means that you need to escape unbalanced parentheses, which typically only
#' # occur in \dontshow{}:
#' \dontshow{if (FALSE) \{ }
#' print("Hello")
#' \dontshow{ \} }
#'
#' # You also need to escape backslashes in infix operators and comments
#' # (this is generally rare)
#' `%\\%` <- function(x, y) x + y
#' 10 %\\% 20
#' # \\\\ (renders as two backslashes)
escape_examples <- function(x) {
  x <- paste0(x, collapse = "\n")
  rd(escapeExamples(x))
}
