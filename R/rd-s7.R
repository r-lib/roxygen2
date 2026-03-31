#' @export
roxy_tag_parse.roxy_tag_prop <- function(x) {
  tag_two_part(x, "a property name", "a description")
}
#' @export
roxy_tag_rd.roxy_tag_prop <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  rd_section(x$tag, value)
}
#' @export
format.rd_section_prop <- function(x, ...) {
  rd_section_description("Additional properties", names(x$value), x$value)
}
