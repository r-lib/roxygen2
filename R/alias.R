# Prefer explicit \code{@@usage} to a \code{@@formals} list.
alias_tag <- function(partitum, name) {
  parts <- partitum[names(partitum) == "aliases"]
  
  if (length(parts) == 0) {
    aliases <- character()
  } else {
    aliases <- str_split(str_trim(unlist(parts, use.names = FALSE)), "\\s+")[[1]]    
  }
  
  if (any(aliases == "NULL")) {
    aliases <- aliases[aliases != "NULL"]
  } else {
    aliases <- c(name, aliases)
  }
  
  new_tag("alias", aliases)
}
