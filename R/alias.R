process_alias <- function(partitum, name, alias) {
  parts <- partitum[names(partitum) == "aliases"]

  if (length(parts) == 0) {
    aliases <- character()
  } else {
    aliases <- str_split(str_trim(unlist(parts, use.names = FALSE)), "\\s+")[[1]]
  }

  if (any(aliases == "NULL")) {
    # Don't add default aliases
    aliases <- aliases[aliases != "NULL"]
  } else {
    aliases <- unique(c(name, alias, aliases))
  }

  new_tag("alias", aliases)
}
