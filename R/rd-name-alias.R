#' @export
roxy_tag_parse.roxy_tag_aliases <- function(x) tag_value(x)
#' @export
format.rd_section_alias <- function(x, ...) {
  x$value <- str_replace_all(x$value, fixed("%"), "\\%")
  format_rd(x, ..., sort = FALSE)
}

#' @export
roxy_tag_parse.roxy_tag_name <- function(x) tag_value(x)
#' @export
format.rd_section_name <- function(x, ...) {
  x$value <- str_replace_all(x$value, fixed("%"), "\\%")
  format_first(x, ...)
}

topic_add_name_aliases <- function(topic, block, name, explicit = FALSE) {
  tags <- block_get_tags(block, "aliases")

  if (length(tags) == 0) {
    aliases <- character()
  } else {
    vals <- map_chr(tags, \(x) x[["val"]])
    aliases <- unlist(str_split(vals, "\\s+"))
  }

  if (any(aliases == "NULL")) {
    # Don't add default aliases
    aliases <- setdiff(aliases, "NULL")
  } else {
    aliases <- c(name, block$object$alias, aliases)
  }
  aliases <- unique(aliases)

  topic$add(rd_section_name(name, explicit))
  topic$add(rd_section("alias", aliases))
}

rd_section_name <- function(name, explicit = FALSE) {
  sec <- rd_section("name", name)
  sec$explicit <- explicit
  sec
}

#' @export
merge.rd_section_name <- function(x, y, ...) {
  # Prefer an explicit @name over an inferred object name
  if (y$explicit && !x$explicit) {
    rd_section_name(c(y$value, x$value), explicit = TRUE)
  } else {
    rd_section_name(c(x$value, y$value), explicit = x$explicit)
  }
}
