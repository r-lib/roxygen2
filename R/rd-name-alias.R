#' @export
roxy_tag_parse.roxy_tag_aliases <- function(x) tag_words(x, min = 1)
#' @export
format.rd_section_alias <- function(x, ...) {
  x$value <- gsub("%", "\\%", x$value, fixed = TRUE)
  format_rd(x, ..., sort = FALSE)
}

#' @export
roxy_tag_parse.roxy_tag_name <- function(x) tag_value(x)
#' @export
format.rd_section_name <- function(x, ...) {
  x$value <- gsub("%", "\\%", x$value, fixed = TRUE)
  format_first(x, ...)
}

topic_add_name_aliases <- function(topic, block, name) {
  tags <- block_get_tags(block, "aliases")

  if (length(tags) == 0) {
    aliases <- character()
  } else {
    aliases <- unlist(map(tags, \(x) x[["val"]]))
  }

  if (any(aliases == "NULL")) {
    # Don't add default aliases
    aliases <- setdiff(aliases, "NULL")
  } else {
    aliases <- c(name, block$object$alias, aliases)
  }
  aliases <- unique(aliases)

  topic$add(rd_section("name", name))
  topic$add(rd_section("alias", aliases))
}
