
#' @export
roxy_tag_parse.roxy_tag_author <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_author <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_author <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_concept <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_concept <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_concept <- function(x, ...) {
  format_rd(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_docType <- function(x) tag_name(x)
#' @export
roxy_tag_rd.roxy_tag_docType <- function(x, base_path, env) {
  roxy_field("docType", x$val)
}
#' @export
format.roxy_field_docType <- function(x, ...) {
  format_first(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_encoding <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_encoding <- function(x, base_path, env) {
  roxy_field(x$tag, x$val)
}
#' @export
format.roxy_field_encoding <- function(x, ...) {
  format_first(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_keywords <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_keywords <- function(x, base_path, env) {
  roxy_field("keyword", str_split(x$val, "\\s+")[[1]])
}
#' @export
format.roxy_field_keyword <- function(x, ...) {
  format_rd(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_format <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_format <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_format <- function(x, ...) {
  format_first(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_return <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_return <- function(x, base_path, env) {
  roxy_field_markdown("value", x$val)
}
