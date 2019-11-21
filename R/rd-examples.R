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
#' @keywords internal
#' @examples
#' # Rd comments are escaped automatically
#' 100 %% 30
#' "50%"
#'
#' # Braces are left as is
#' 1 # \link{mean}
#' "{"
#'
#' # As are backslashes
#' "\""
#'
#' \dontshow{if (FALSE) \{ }
#' print("Hello")
#' \dontshow{ \} }
escape_examples <- function(x) {
  x <- paste0(x, collapse = "\n")
  x <- gsub("%", "\\%", x, fixed = TRUE, useBytes = TRUE)
  rd(x)
}
