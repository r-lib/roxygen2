#' @include parse-registry.R

parse_assignment <- function(call, env) {
  assignee <- as.character(call[[2]])
  
  # If it doesn't exist (any more), don't document it.
  if (!exists(assignee, env)) return()

  out <- list(
    src_name = assignee,
    src_alias = assignee)

  value <- get(assignee, env)
  if (is.function(value)) {
    out$src_type <- "function"
    out$formals <- formals(value)
  } else if (inherits(value, "refObjectGenerator")) {
    out$src_type <- "ref-class"
  } else {
    out$src_type <- "data"
    out$str <- str_c(capture.output(str(value, max.level = 1)), 
      collapse = "\n")
  }

  out
}

parse_class <- function(call, env) {
  name <- as.character(call$Class)
  class <- getClass(name, where = env)

  # class?classRepresentation
  list(
    src_type = "class",
    src_name = name,
    src_alias = c(name, str_c(name, "-class")),
    extends = showExtends(class@contains, printTo = FALSE),
    slots = class@slots
  )
}

parse_generic <- function(call, env) {
  name <- as.character(call$name)
  f <- getGeneric(name, where = env)
  
  list(
    src_type = "function",
    src_name = topic_name(f),
    src_alias = c(name, str_c(name, "-methods")),
    formals = formals(f@.Data)
  )
}

parse_method <- function(call, env) {
  name <- as.character(call$f)
  f <- getMethod(name, eval(call$signature), where = env)

  pkg <- attr(f@generic, "package")
  if (pkg == "roxygen_test") {
    inherit <- f@generic
  } else {
    inherit <- str_c(pkg, "::", f@generic)
  }

  # class?MethodDefinition
  list(
    src_type = "method",
    src_name = topic_name(f),
    src_alias = topic_name(f),
    generic = f@generic,
    formals = formals(f@.Data),
    signature = as.character(f@defined),
    inheritParams = inherit
  )
}

register.srcref.parser('<-', parse_assignment)
register.srcref.parser('=', parse_assignment)
register.srcref.parser('setClass', parse_class)
register.srcref.parser('setGeneric', parse_generic)
register.srcref.parser('setMethod', parse_method)
# register.srcref.parser('setReplaceMethod', parse_method)

setGeneric("topic_name", function(x) {
  standardGeneric("topic_name")
})
setMethod("topic_name", signature(x = "MethodDefinition"), function(x) {
  str_c(str_c(c(x@generic, x@defined), collapse = ","), "-method")
})
setMethod("topic_name", signature(x = "standardGeneric"), function(x) {
  x@generic
})
setMethod("topic_name", signature(x = "nonstandardGenericFunction"), function(x) {
  x@generic
})

