object_from_call <- function(call, env, block) {
  if (is.null(call)) return()
  if (!is.call(call)) return()

  call <- standardise_call(call, env)
  name <- as.character(call[[1]])

  if (length(name) > 1) return(NULL)

  # Dispatch to registered srcref parsers based on function name
  parser <- find_parser(name)
  if (is.null(parser)) return(NULL)

  parser(call, env, block)
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


#' @importFrom methods getClass
parser_setClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- getClass(name, where = env)

  object(value)
}

#' @importFrom methods getRefClass
parser_setRefClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- getClass(name, where = env)

  object(value)
}

#' @importFrom methods getGeneric
parser_setGeneric <- function(call, env, block) {
  name <- as.character(call$name)
  value <- getGeneric(name, where = env)

  object(value, name = name)
}

#' @importFrom methods getMethod
parser_setMethod <- function(call, env, block) {
  name <- as.character(call$f)
  value <- getMethod(name, eval(call$signature), where = env)
  value@.Data <- extract_method_fun(value@.Data)

  object(value, name = name)
}
