# Without sections --------------------------------------------------------

#' @export
roxy_tag_parse.roxy_tag_author <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_author <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_author <- function(x, ...) {
  format_collapse(x, ...)
}
#' @export
merge.rd_section_author <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  # Remove duplicated authors, e.g. when using @rdname
  rd_section(x$type, unique(c(x$value, y$value)))
}


#' @export
roxy_tag_parse.roxy_tag_format <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_format <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_format <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_note <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_note <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_note <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_references <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_references <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_references <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_return <- function(x) tag_markdown(x)
#' @export
roxy_tag_parse.roxy_tag_returns <- roxy_tag_parse.roxy_tag_return
#' @export
roxy_tag_rd.roxy_tag_return <- function(x, base_path, env) {
  rd_section("value", x$val)
}
#' @export
roxy_tag_rd.roxy_tag_returns <- roxy_tag_rd.roxy_tag_return
#' @export
format.rd_section_value <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_seealso <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_seealso <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_seealso <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_source <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_source <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_source <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_title <- function(x) {
  if (str_count(x$raw, "\n\n") >= 1) {
    warn_roxy_tag(x, "must be a single paragraph")
  }

  tag_markdown(x)
}
#' @export
roxy_tag_rd.roxy_tag_title <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_title <- function(x, ...) {
  format_first(x, ...)
}

# With sections -----------------------------------------------------------

#' @export
roxy_tag_parse.roxy_tag_description <- function(x) {
  tag_markdown_with_sections(x)
}
#' @export
roxy_tag_rd.roxy_tag_description <- function(x, base_path, env) {
  rd_section_markdown(x$tag, x$val)
}
#' @export
format.rd_section_description <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_details <- function(x) {
  tag_markdown_with_sections(x)
}
#' @export
roxy_tag_rd.roxy_tag_details <- function(x, base_path, env) {
  rd_section_markdown(x$tag, x$val)
}
#' @export
format.rd_section_details <- function(x, ...) {
  format_collapse(x, ...)
}

rd_section_markdown <- function(name, value) {
  # Any additional components are sections
  if (length(value) > 1) {
    titles <- names(value)
    value <- unname(value)

    name <- c(name, rep("section", length(value) - 1))
    value <- c(
      list(value[[1]]),
      map2(titles[-1], value[-1], \(x, y) list(title = x, content = y))
    )

    if (value[[1]] == "") {
      name <- name[-1]
      value <- value[-1]
    }
  }

  map2(name, value, rd_section)
}
