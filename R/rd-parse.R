get_rd <- function(topic, package = NULL) {
  help_call <- substitute(help(t, p), list(t = topic, p = package))
  top <- eval(help_call)
  utils:::.getHelpFile(top)
}

rd_tag <- function(x) attr(x, "Rd_tag")

has_tag <- function(rd, tag) {
  Filter(function(x) rd_tag(x) == tag, rd)
}

rd2rd <- function(x) {
  paste(unlist(tools:::as.character.Rd(x)), collapse = "")
}

# rd_arguments(get_rd("mean"))
rd_arguments <- function(rd) {
  arguments <- has_tag(rd, "\\arguments")[[1]]
  items <- has_tag(arguments, "\\item")
  
  values <- lapply(items, function(x) rd2rd(x[[2]]))
  params <- vapply(items, function(x) rd2rd(x[[1]]), character(1))
  
  setNames(values, params)
}

