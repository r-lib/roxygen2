object_from_call <- function(call, env, block) {
  if (is.null(call)) return()

  # Special case: you can refer to other objects as strings
  if (is.character(call)) {
    value <- find_data(call, env)
    value <- standardise_obj(call, value, env, block)

    return(object(value, call))
  }

  if (!is.call(call)) return()

  call <- standardise_call(call, env)
  name <- as.character(call[[1]])

  if (length(name) > 1) return(NULL)

  # Dispatch to registered srcref parsers based on function name
  parser <- find_parser(name)
  if (is.null(parser)) return(NULL)

  parser(call, env, block)
}

find_data <- function(name, env) {
  ns <- env_namespace(env)
  if (is.null(ns)) {
    get(name, envir = env)
  } else {
    getExportedValue(name, ns = ns)
  }
}

# Find namespace associated with environment
env_namespace <- function(env) {
  env_name <- attr(env, "name")
  if (is.null(env_name)) return(NULL)
  if (!grepl(":", env_name)) return(NULL)

  ns_name <- gsub("package:", "", env_name)
  ns <- NULL
  try(ns <- asNamespace(ns_name), silent = TRUE)
  if (is.null(ns)) return(NULL)

  ns
}

find_parser <- function(name) {
  if (name %in% c("=", "<-", "<<-")) name <- "assignment"

  parser_name <- paste0("parser_", name)
  if (!exists(parser_name)) return(NULL)

  get(parser_name, mode = "function")
}

standardise_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))

  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)

  match.call(f, call)
}

parser_assignment <- function(call, env, block) {
  name <- as.character(call[[2]])

  # If it's a compound assignment like x[[2]] <- ignore it
  if (length(name) > 1)  return()

  # If it doesn't exist (any more), don't document it.
  if (!exists(name, env)) return()

  value <- get(name, env)
  value <- standardise_obj(name, value, env, block)

  object(value, name)
}


parser_setClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- methods::getClass(name, where = env)

  object(value)
}

parser_setRefClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- methods::getClass(name, where = env)

  object(value)
}

parser_setGeneric <- function(call, env, block) {
  name <- as.character(call$name)
  value <- methods::getGeneric(name, where = env)

  object(value)
}

parser_setMethod <- function(call, env, block) {
  name <- as.character(call$f)
  value <- methods::getMethod(name, eval(call$signature), where = env)
  value@.Data <- extract_method_fun(value@.Data)

  object(value)
}

parser_setReplaceMethod <- function(call, env, block) {
  name <- paste0(as.character(call$f), "<-")
  value <- methods::getMethod(name, eval(call[[3]]), where = env)
  value@.Data <- extract_method_fun(value@.Data)

  object(value)
}
