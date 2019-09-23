#' @export
roxy_tag_parse.roxy_tag_description <- function(x) tag_markdown_with_sections(x)
#' @export
roxy_tag_rd.roxy_tag_description <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_description <- function(x, ...) {
  format_collapse(x, ...)
}


#' @export
roxy_tag_parse.roxy_tag_details <- function(x) tag_markdown_with_sections(x)
#' @export
roxy_tag_rd.roxy_tag_details <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_details <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_note <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_note <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_note <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_references <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_references <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_references <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_return <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_return <- function(x, base_path, env) {
  roxy_field_markdown("value", x$val)
}
#' @export
format.roxy_field_value <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_seealso <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_seealso <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_seealso <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_source <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_source <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_source <- function(x, ...) {
  format_collapse(x, ...)
}

#' @export
roxy_tag_parse.roxy_tag_title <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_title <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}
#' @export
format.roxy_field_title <- function(x, ...) {
  format_first(x, ...)
}
