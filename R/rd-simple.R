#' @export
roxy_tag_parse.roxy_tag_concept <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_concept <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_concept <- function(x, ...) {
  format_rd(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_docType <- function(x) tag_name(x)
#' @export
roxy_tag_rd.roxy_tag_docType <- function(x, base_path, env) {
  rd_section("docType", x$val)
}
#' @export
format.rd_section_docType <- function(x, ...) {
  format_first(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_encoding <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_encoding <- function(x, base_path, env) {
  rd_section(x$tag, x$val)
}
#' @export
format.rd_section_encoding <- function(x, ...) {
  format_first(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_keywords <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_keywords <- function(x, base_path, env) {
  rd_section("keyword", str_split(x$val, "\\s+")[[1]])
}
#' @export
format.rd_section_keyword <- function(x, ...) {
  format_rd(x, ...)
}
