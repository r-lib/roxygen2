#' @export
roxy_tag_parse.roxy_tag_field <- function(x) tag_name_description(x)
#' @export
roxy_tag_rd.roxy_tag_field <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  rd_section(x$tag, value)
}
#' @export
format.rd_section_field <- function(x, ...) {
  rd_section_description("Fields", names(x$value), x$value)
}

#' @export
roxy_tag_parse.roxy_tag_slot <- function(x) tag_name_description(x)
#' @export
roxy_tag_rd.roxy_tag_slot <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  rd_section(x$tag, value)
}
#' @export
format.rd_section_slot <- function(x, ...) {
  rd_section_description("Slots", names(x$value), x$value)
}
