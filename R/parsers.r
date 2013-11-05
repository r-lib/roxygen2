#' @include parse-registry.R
NULL

register.srcref.parsers(function(call, env) {
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
}, '<-', '=')

register.srcref.parser('setClass', function(call, env) {
  name <- as.character(call$Class)
  value <- getClass(name)
  list(
    S4class = name,
    type = "s4class",
    object = object("s4class", name, value)
  )
})

register.srcref.parser('setGeneric', function(call, env) {
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
})

register.srcref.parser('setMethod', function(call, env) {
  name <- as.character(call$f)
  value <- getMethod(name, eval(call$signature), where = env)
  
  list(
    fun = TRUE,
    type = "s4method",
    S4method = name, # for namespace roclet
    value = value,
    object = object("s4method", name, value)
  )
  
})

