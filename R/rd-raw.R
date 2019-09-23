#' @export
roxy_tag_parse.roxy_tag_evalRd <- function(x) tag_code(x)
#' @export
roxy_tag_rd.roxy_tag_evalRd <- function(x, base_path, env) {
  roxy_field("rawRd", roxy_tag_eval(x, env))
}

#' @export
roxy_tag_parse.roxy_tag_rawRd <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_rawRd <- function(x, base_path, env) {
  roxy_field(x$tag, x$val)
}
#' @export
format.roxy_field_rawRd <- function(x, ...) {
  paste(x$value, collapse = "\n")
}
