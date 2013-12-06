object_from_call <- function(call, env, block) {
  if (is.null(call)) return()
  
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
  assignee <- as.character(call[[2]])
  
  # If it's a compound assignment like x[[2]] <- ignore it
  if (length(assignee) > 1)  return()
  
  # If it doesn't exist (any more), don't document it.
  if (!exists(assignee, env)) return()
  value <- get(assignee, env)
  
  if (inherits(value, "refObjectGenerator")) {
    class <- as.character(value@className)
    object("rcclass", class, getRefClass(class), alias = assignee)
  } else if (inherits(value, "classGeneratorFunction")) {
    class <- as.character(value@className)
    object("s4class", class, getClass(class), alias = assignee)
  } else if (is.function(value)) {
    method <- unlist(block$method, use.names = FALSE)
    value <- add_s3_metadata(value, assignee, env, method)
    if (is.s3generic(value)) {
      objtype <- "s3generic"
    } else if (is.s3method(value)) {
      objtype <- "s3method"
    } else if (is.function(value)) {
      objtype <- "function"
    }
    
    object(objtype, assignee, value)
  } else {
    object("data", assignee, value)
  }
}

#' @importFrom methods getClass
parser_setClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- getClass(name)

  object("s4class", name, value)
}

#' @importFrom methods getRefClass
parser_setRefClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- getRefClass(name)
  
  object("rcclass", name, value)
}

#' @importFrom methods getGeneric
parser_setGeneric <- function(call, env, block) {
  name <- as.character(call$name)
  value <- getGeneric(name, where = env)
  
  object("s4generic", name, value)
}

#' @importFrom methods getMethod
parser_setMethod <- function(call, env, block) {
  name <- as.character(call$f)
  value <- getMethod(name, eval(call$signature), where = env)
  # When a generic has ... and a method adds new arguments, the S4 method
  # wraps the definition inside another function which has the same arguments
  # as the generic. We replace it with the real definition so that formals etc
  # are correct.
  value@.Data <- eval(call$definition, env)
  
  object("s4method", name, value)
}
