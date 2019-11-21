#' @export
roxy_tag_parse.roxy_tag_examples <- function(x) {
  tag_examples(x)
}
#' @export
roxy_tag_parse.roxy_tag_example <- function(x) {
  x <- tag_value(x)

  nl <- str_count(x$val, "\n")
  if (any(nl) > 0) {
    roxy_tag_warning(x, "spans multiple lines. Do you want @examples?")
    return()
  }

  x
}

#' @export
roxy_tag_rd.roxy_tag_examples <- function(x, base_path, env) {
  rd_section("examples", x$val)
}
#' @export
roxy_tag_rd.roxy_tag_example <- function(x, base_path, env) {
  path <- file.path(base_path, x$val)
  if (!file.exists(path)) {
    roxy_tag_warning(x, "'", path, "' doesn't exist")
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
#'
#' @keywords internal
#' @examples
#' # The only thing that must be escaped in examples is Rd comments:
#' 100 %% 30
#' # even if they are in strings
#' "50%"
#'
#' # Otherwise, backslashes and braces can be left as is
#' "\""
#' "{"
#' # It looks like you could use Rd tags in comments, but these are
#' # not actually parsed
#' 1 # \link{mean}
#'
#' # The only place that a backslash can occur in R code is as part of
#' # an infix operator or in a non-syntactic name. If you do either of those
#' # things, you'll need to escape it yourself.
#' `%\\\\%` <- function(x, y) x + y
#' 10 %\\% 20
#'
#' # You must escape braces if they are unbalanced, which typically
#' # only occurs in \dontshow{}:
#' \dontshow{if (FALSE) \{ }
#' print("Hello")
#' \dontshow{ \} }
#'
#' # Otherwise, you _can_ escape them, but there's little point.
#' # The following two lines are equivalent
#' f <- function() { NULL }
#' f <- function() \{ NULL \}
escape_examples <- function(x) {
  x <- paste0(x, collapse = "\n")
  x <- gsub("%", "\\%", x, fixed = TRUE, useBytes = TRUE)
  rd(x)
}
