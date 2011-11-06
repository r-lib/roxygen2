#' @include parse-registry.R

parse_assignment <- function(call, env) {
  assignee <- as.character(call[[2]])
  
  # If it doesn't exist (any more), don't document it.
  if (!exists(assignee, env)) return()
  value <- get(assignee, env)
  
  out <- list(assignee = as.character(assignee))
  if (length(assignee) == 1) {
     out$src_name <- out$assignee
  }
  out$src_alias <- out$src_name

  if (is.function(value)) {
    out$docType <- "function"
    out$formals <- formals(value)
  } else {
    out$docType <- "data"
    out$format <- str_c(capture.output(str(value, max.level = 1)), 
      collapse = "\n")
    out$usage <- out$src_name
  }
  out
}

parse_class <- function(call, env) {
  name <- as.character(call$Class)
  class <- getClass(name, where = env)

  # class?classRepresentation
  list(
    docType = "class",
    src_name = name,
    alias = str_c("class-", name),
    extends = showExtends(class@contains, printTo = FALSE),
    slots = class@slots
  )
}

parse_generic <- function(call, env) {
  name <- as.character(call$name)
  f <- getGeneric(name, where = env)
  
  list(
    docType = "function",
    name = name,
    alias = c(name, str_c("methods-", name))
  )
}

parse_method <- function(call, env) {
  name <- as.character(call$f)
  f <- getMethod(name, where = env)
  
  # class?MethodDefinition
  list(
    docType = "method",
    src_name = name,
    alias = str_c("method-", f@target, str_c(f@signature, collapse = ","))
    type = "S4-method",
    inheritParams = f@generic,
  )
})

register.srcref.parser('<-', parse_assignment)
register.srcref.parser('=', parse_assignment)
register.srcref.parser('setClass', parse_class)
register.srcref.parser('setGeneric', parse_generic)
register.srcref.parser('setMethod', parse_method)
# register.srcref.parser('setReplaceMethod', parse_method)
