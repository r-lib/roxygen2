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

# We take out the \dontshow{} etc. commands, because these should be
# left as is, to allow the user to escape braces for example:
# #' @examples
# #' \\dontshow{ \{ }
# #' # Hidden!
# #' \\dontshow{ \} }
#
# Otherwise, it works like escape, but unescapes special rd example commands.
# Also unescapes quotes because they must already be in strings and hence
# don't need an additional layer of quoting.
escape_examples <- function(x) {
  ex_tags <- c("\\dontshow", "\\dontrun", "\\donttest", "\\testonly")
  rd_tags <- find_fragile_rd_tags(x, ex_tags)
  x <- x0 <- protect_rd_tags(x, rd_tags)

  attr(x, "roxygen-markdown-subst") <- NULL
  x <- escape(x)
  x <- gsub("\\\\dont", "\\dont", x, fixed = TRUE)
  x <- gsub("\\\\'", "\\'", x, fixed = TRUE)
  x <- gsub('\\\\"', '\\"', x, fixed = TRUE)

  rd(unescape_rd_for_md(x, x0))
}
