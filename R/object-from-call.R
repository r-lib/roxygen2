object_from_call <- function(call, env, block, file) {
  if (is.null(call)) return()

  # Special case: you can refer to other objects as strings
  if (is.character(call)) {
    value <- find_data(call, env, file)
    value <- standardise_obj(call, value, env, block)

    return(object(value, call))
  }

  if (!is.call(call)) return()

  call <- standardise_call(call, env)

  name <- call[[1]]
  if (is_symbol(call[[1]])) {
    name <- as.character(call[[1]])
  } else if (is_call(call[[1]], "::", n = 2) && is_symbol(call[[1]][[2]], "methods")) {
    name <- as.character(call[[1]][[3]])
  } else {
    return(NULL)
  }

  # Dispatch to registered srcref parsers based on function name
  parser <- find_parser(name)
  if (is.null(parser)) return(NULL)

  parser(call, env, block)
}

find_data <- function(name, env, file) {
  if (identical(name, "_PACKAGE")) {
    return(find_data_for_package(env, file))
  }

  ns <- env_namespace(env)
  if (is.null(ns)) {
    get(name, envir = env)
  } else {
    getExportedValue(name, ns = ns)
  }
}

find_data_for_package <- function(env, file) {
  pkg_path <- dirname(dirname(file))
  desc <- read.description(file.path(pkg_path, "DESCRIPTION"))

  structure(
    list(
      desc = desc,
      path = pkg_path
    ),
    class = "package"
  )
}

# Find namespace associated with environment
env_namespace <- function(env) {
  ns <- NULL
  try(ns <- asNamespace(env), silent = TRUE)
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

parser_setClassUnion <- function(call, env, block) {
  name <- as.character(call$name)
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

`parser_::` <- function(call, env, block) {
  pkg <- as.character(call[[2]])
  fun <- as.character(call[[3]])

  object(list(pkg = pkg, fun = fun), alias = fun, type = "import")
}

parser_setMethodS3 <- function(call, env, block) {
  # R.methodsS3::setMethodS3(name, class, ...)
  method <- as.character(call[[2]])
  class <- as.character(call[[3]])
  name <- paste(method, class, sep=".")
  value <- standardise_obj(name, get(name, env), env, block)
  object(value, name)
}

parser_setConstructorS3 <- function(call, env, block) {
  # R.oo::setConstructorS3(name, ...)
  name <- as.character(call[[2]])
  value <- standardise_obj(name, get(name, env), env, block)
  object(value, name)
}


# helpers -----------------------------------------------------------------

# When a generic has ... and a method adds new arguments, the S4 method
# wraps the definition inside another function which has the same arguments
# as the generic. This function figures out if that's the case, and extracts
# the original function if so.
#
# It's based on expression processing based on the structure of the
# constructed method which looks like:
#
# function (x, ...) {
#   .local <- function (x, ..., y = 7) {}
#   .local(x, ...)
# }
extract_method_fun <- function(x) {
  fun <- x@.Data

  method_body <- body(fun)
  if (!is_call(method_body, "{")) return(fun)
  if (length(method_body) < 2) return(fun)

  first_line <- method_body[[2]]
  if (!is_call(first_line, name = "<-", n = 2)) return(fun)
  if (!identical(first_line[[2]], quote(`.local`))) return(fun)

  eval(first_line[[3]])
}

