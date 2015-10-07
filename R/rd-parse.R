get_rd <- function(topic, package = NULL) {
  help_call <- substitute(help(t, p), list(t = topic, p = package))
  top <- eval(help_call)
  if (length(top) == 0) return(NULL)
  
  internal_f("utils", ".getHelpFile")(top)
}

# get_rd should parse Rd into a rd_file so I don't need to maintain
# two parallel apis

get_tags <- function(rd, tag) {
  rd_tag <- function(x) attr(x, "Rd_tag")

  Filter(function(x) rd_tag(x) == tag, rd)
}

rd2rd <- function(x) {
  chr <- internal_f("tools", "as.character.Rd")(x)
  paste(unlist(chr), collapse = "")
}

# rd_arguments(get_rd("mean"))
rd_arguments <- function(rd) {
  arguments <- get_tags(rd, "\\arguments")[[1]]
  items <- get_tags(arguments, "\\item")

  values <- lapply(items, function(x) rd2rd(x[[2]]))
  # Everything else seems to be escaped already, apart from comments
  values <- lapply(values, function(x) gsub("%", "\\%", x, fixed = TRUE))

  params <- vapply(items, function(x) rd2rd(x[[1]]), character(1))

  setNames(values, params)
}
