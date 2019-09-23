#' @export
roxy_tag_parse.roxy_tag_field <- function(x) tag_name_description(x)
#' @export
roxy_tag_rd.roxy_tag_field <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  roxy_field(x$tag, value)
}
#' @export
format.roxy_field_field <- function(x, ...) {
  roxy_field_description("Fields", names(x$value), x$value)
}

#' @export
roxy_tag_parse.roxy_tag_slot <- function(x) tag_name_description(x)
#' @export
roxy_tag_rd.roxy_tag_slot <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  roxy_field(x$tag, value)
}
#' @export
format.roxy_field_slot <- function(x, ...) {
  roxy_field_description("Slots", names(x$value), x$value)
}
