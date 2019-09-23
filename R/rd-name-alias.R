#' @export
roxy_tag_parse.roxy_tag_aliases <- function(x) tag_value(x)
#' @export
format.roxy_field_alias <- function(x, ...) {
  x$value <- str_replace_all(x$value, fixed("%"), "\\%")
  format_rd(x, ..., sort = FALSE)
}

#' @export
roxy_tag_parse.roxy_tag_name <- function(x) tag_value(x)
#' @export
format.roxy_field_name <- function(x, ...) {
  x$value <- str_replace_all(x$value, fixed("%"), "\\%")
  format_first(x, ...)
}

topic_add_name_aliases <- function(topic, block, name) {
  tags <- block_get_tags(block, "aliases")

  if (length(tags) == 0) {
    aliases <- character()
  } else {
    vals <- map_chr(tags, "val")
    aliases <- unlist(str_split(vals, "\\s+"))
  }

  if (any(aliases == "NULL")) {
    # Don't add default aliases
    aliases <- setdiff(aliases, "NULL")
  } else {
    aliases <- c(name, block$object$alias, aliases)
  }
  aliases <- unique(aliases)

  topic$add_simple_field("name", name)
  topic$add_simple_field("alias", aliases)
}
