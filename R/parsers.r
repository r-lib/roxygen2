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
    out$formals <- formals(value)
  } else if (inherits(value, "refObjectGenerator")) {
    # Reference class
  } else {
    if (is.null(out$docType)) out$docType <- "data"
    out$str <- str_c(capture.output(str(value, max.level = 1)),
      collapse = "\n")
  }
  out
}, '<-', '=')


register.srcref.parser('setClass', function(call, env) {
  list(
    S4class = as.character(call$Class),
    type = "s4class"
  )
})

register.srcref.parser('setGeneric', function(call, env) {
  name <- as.character(call$name)
  f <- getGeneric(name, where = env)
  
  list(
    fun = TRUE,
    assignee = name,
    S4generic = name, 
    formals = formals(f),
    type = "s4generic"
  )
})

register.srcref.parser('setMethod', function(call, env) {
  name <- as.character(call$f)
  val <- getMethod(name, eval(call$signature), where = env)
  
  list(
    fun = TRUE,
    type = "s4method",
    S4method = name, # for namespace roclet
    value = val
  )
})
