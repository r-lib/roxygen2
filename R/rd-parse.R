get_rd <- function(topic, package = NULL) {
  help_call <- substitute(help(t, p), list(t = topic, p = package))
  top <- eval(help_call)
  
  if (length(top) == 0) return(list())
  
  utils:::.getHelpFile(top)
}

# get_rd should parse Rd into a rd_file so I don't need to maintain
# two parallel apis

get_tags <- function(rd, tag) {
  rd_tag <- function(x) attr(x, "Rd_tag")

  Filter(function(x) rd_tag(x) == tag, rd)
}

rd2rd <- function(x) {
  paste(unlist(tools:::as.character.Rd(x)), collapse = "")
}

# rd_arguments(get_rd("mean"))
rd_arguments <- function(rd) {
  arguments <- get_tags(rd, "\\arguments")[[1]]
  items <- get_tags(arguments, "\\item")
  
  values <- lapply(items, function(x) rd2rd(x[[2]]))
  params <- vapply(items, function(x) rd2rd(x[[1]]), character(1))
  
  setNames(values, params)
}

rd_title <- function(rd) {
  as.character(get_tags(rd, "\\title")[[1]])
}

rd_description <- function(rd) {
  tags <- get_tags(rd, "\\description")[[1]]
  str_trim(str_c(as.character(tags), collapse = ""))
}

method_summary <- function(generic) {
  is_default <- function(x) inherits(x, "derivedDefaultMethod")
  methods <- Filter(Negate(is_default), findMethods(generic))

  rd <- lapply(methods, lookup_rd)
  links <- vapply(methods, link, character(1))
  
  # Only display if title / description different to generic / class ?
  
}

link <- function(x) {
  str_c("\\code{\\link[", link_text(x), "]{", topic_name(x), "}}")
}

setGeneric("link_text", function(x) {
  standardGeneric("link_text")
})
setMethod("link_text", signature(x = "MethodDefinition"), function(x) {
  sig <- x@defined
  str_c(x@generic, "(", str_c(sig@names, " = ", sig), ")")
})

lookup_rd <- function(x, package = NULL) {
  get_rd(topic_name(x), package = package)
}
