# Parse a srcref
parse.srcref <- function(ref, env) {
  srcfile <- attributes(ref)$srcfile
  srcref <- list(srcref = 
    list(filename = srcfile$filename, lloc = as.vector(ref)))

  # Get code from source and parse to extract first call
  lines <- getSrcLines(srcfile, ref[[1]], ref[[3]])
  call <- parse(text = lines)[[1]]
  if (!is.call(call)) return(srcref)

  call <- standardise_call(call, env)
  name <- as.character(call[[1]])
  if (length(name) > 1) return(srcref)
  
  # Dispatch to registered srcref parsers based on function name
  parser <- find_parser(name)
  if (is.null(parser)) return(srcref)
  
  c(srcref, parser(call, env))
}

find_parser <- function(name) {
  parser_name <- paste0("parser_", name)
  if (!exists(parser_name)) return(NULL)
  
  match.fun(parser_name)
}

standardise_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  
  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)
  
  match.call(f, call)
}

`parser_=` <- function(call, env) {
  assignee <- as.character(call[[2]])
  
  # If it doesn't exist (any more), don't document it.
  if (!exists(assignee, env)) return()
  value <- get(assignee, env)
  
  out <- list(assignee = as.character(assignee))
  out$fun <- is.function(value)
  
  if (out$fun) {
    out$type <- "function"
    out$formals <- formals(value)
    out$object <- object("function", assignee, value)
  } else if (inherits(value, "refObjectGenerator")) {
    out$type <- "rcclass"
    out$object <- object("rcclass", assignee, value)
  } else {
    out$type <- "data"
    out$object <- object("data", assignee, value)
    if (is.null(out$docType)) out$docType <- "data"
    out$str <- str_c(capture.output(str(value, max.level = 1)),
      collapse = "\n")
  }
  out
}
`parser_<-` <- `parser_=`

parser_setClass <- function(call, env) {
  name <- as.character(call$Class)
  value <- getClass(name)
  list(
    S4class = name,
    type = "s4class",
    object = object("s4class", name, value)
  )
}

parser_setGeneric <- function(call, env) {
  name <- as.character(call$name)
  value <- getGeneric(name, where = env)
  
  list(
    fun = TRUE,
    assignee = name,
    S4generic = name, 
    formals = formals(value),
    type = "s4generic",
    object = object("s4generic", name, value)
  )
}

parser_setMethod <- function(call, env) {
  name <- as.character(call$f)
  value <- getMethod(name, eval(call$signature), where = env)
  
  list(
    fun = TRUE,
    type = "s4method",
    S4method = name, # for namespace roclet
    value = value,
    object = object("s4method", name, value)
  )  
}
