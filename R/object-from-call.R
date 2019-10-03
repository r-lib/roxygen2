object_from_call <- function(call, env, block, file) {
  if (is.character(call)) {
    if (identical(call, "_PACKAGE")) {
      parser_package(call, env, block, file)
    } else {
      parser_data(call, env, file)
    }
  } else if (is.call(call)) {
    call <- call_standardise(call, env)
    name <- deparse(call[[1]])
    switch(name,
      "=" = ,
      "<-" = ,
      "<<-" = parser_assignment(call, env, block),
      "::" = parser_import(call, env, block),

      "methods::setClass" = ,
      "setClass" = parser_setClass(call, env, block),
      "methods::setClassUnion" = ,
      "setClassUnion" = parser_setClassUnion(call, env, block),
      "methods::setRefClass" = ,
      "setRefClass" = parser_setRefClass(call, env, block),
      "methods::setGeneric" = ,
      "setGeneric" = parser_setGeneric(call, env, block),
      "methods::setMethod" = ,
      "setMethod" = parser_setMethod(call, env, block),
      "methods::setReplaceMethod" = ,
      "setReplaceMethod" = parser_setReplaceMethod(call, env, block),

      "R.methodsS3::setMethodS3" = ,
      "setMethodS3" = parser_setMethodS3(call, env, block),

      "R.oo::setConstructorS3" = ,
      "setConstructorS3" = parser_setConstructorS3(call, env, block),
      NULL
    )
  } else {
    NULL
  }
}

object_from_name <- function(name, env, block) {
  value <- get(name, env)
  if (inherits(value, "R6ClassGenerator")) {
    type <- "r6class"
  } else if (methods::is(value, "refObjectGenerator")) {
    value <- methods::getClass(as.character(value@className), where = env)
    type <- "rcclass"
  } else if (methods::is(value, "classGeneratorFunction")) {
    value <- methods::getClass(as.character(value@className), where = env)
    type <- "s4class"
  } else if (methods::is(value, "MethodDefinition")) {
    # S4 methods need munging to get real function def
    value@.Data <- extract_method_fun(value@.Data)
    type <- "s4method"
  } else if (methods::is(value, "standardGeneric")) {
    type <- "s4generic"
  } else if (is.function(value)) {
    # Potential S3 methods/generics need metadata added
    method <- block_get_tag_value(block, "method")
    value <- add_s3_metadata(value, name, env, method)
    if (inherits(value, "s3generic")) {
      type <- "s3generic"
    } else if (inherits(value, "s3method")) {
      type <- "s3method"
    } else {
      type <- "function"
    }
  } else {
    type <- "data"
  }

  object(value, name, type)
}

# Parsers for individual calls --------------------------------------------

parser_data <- function(call, env, block) {
  if (isNamespace(env)) {
    value <- getExportedValue(call, ns = asNamespace(env))
  } else {
    value <- get(call, envir = env)
  }
  object(value, call, type = "data")
}

parser_package <- function(call, env, block, file) {
  pkg_path <- dirname(dirname(file))
  desc <- read.description(file.path(pkg_path, "DESCRIPTION"))

  value <- list(
    desc = desc,
    path = pkg_path
  )
  object(value, call, type = "package")
}

parser_assignment <- function(call, env, block) {
  name <- as.character(call[[2]])

  # If it's a compound assignment like x[[2]] <- ignore it
  if (length(name) > 1) {
    return()
  }

  # If it doesn't exist (any more), don't document it.
  if (!exists(name, env)) {
    return()
  }

  object_from_name(name, env, block)
}

parser_setClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- methods::getClass(name, where = env)

  object(value, NULL, "s4class")
}

parser_setClassUnion <- function(call, env, block) {
  name <- as.character(call$name)
  value <- methods::getClass(name, where = env)

  object(value, NULL, "s4class")
}

parser_setRefClass <- function(call, env, block) {
  name <- as.character(call$Class)
  value <- methods::getClass(name, where = env)

  object(value, NULL, "rcclass")
}

parser_setGeneric <- function(call, env, block) {
  name <- as.character(call$name)
  value <- methods::getGeneric(name, where = env)

  object(value, NULL, "s4generic")
}

parser_setMethod <- function(call, env, block) {
  name <- as.character(call$f)
  value <- methods::getMethod(name, eval(call$signature), where = env)
  value@.Data <- extract_method_fun(value@.Data)

  object(value, NULL, "s4method")
}

parser_setReplaceMethod <- function(call, env, block) {
  name <- paste0(as.character(call$f), "<-")
  value <- methods::getMethod(name, eval(call[[3]]), where = env)
  value@.Data <- extract_method_fun(value@.Data)

  object(value, NULL, "s4method")
}

parser_import <- function(call, env, block) {
  pkg <- as.character(call[[2]])
  fun <- as.character(call[[3]])

  object(list(pkg = pkg, fun = fun), alias = fun, type = "import")
}

parser_setMethodS3 <- function(call, env, block) {
  # R.methodsS3::setMethodS3(name, class, ...)
  method <- as.character(call[[2]])
  class <- as.character(call[[3]])
  name <- paste(method, class, sep = ".")

  method <- block_get_tag_value(block, "method")
  value <- add_s3_metadata(get(name, env), name, env, method)

  object(value, name, "s3method")
}

parser_setConstructorS3 <- function(call, env, block) {
  # R.oo::setConstructorS3(name, ...)
  name <- as.character(call[[2]])
  object(get(name, env), name, "function")
}

# helpers -----------------------------------------------------------------

# @param override Either NULL to use default, or a character vector of length 2
add_s3_metadata <- function(val, name, env, override = NULL) {
  if (!is.null(override)) {
    return(s3_method(val, override))
  }

  if (is_s3_generic(name, env)) {
    class(val) <- c("s3generic", "function")
    return(val)
  }

  method <- find_generic(name, env)
  if (is.null(method)) {
    val
  } else {
    s3_method(val, method)
  }
}

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
extract_method_fun <- function(fun) {
  method_body <- body(fun)
  if (!is_call(method_body, "{")) return(fun)
  if (length(method_body) < 2) return(fun)

  first_line <- method_body[[2]]
  if (!is_call(first_line, name = "<-", n = 2)) return(fun)
  if (!identical(first_line[[2]], quote(`.local`))) return(fun)

  local_fun <- eval(first_line[[3]])
  if (!is.function(local_fun)) return(fun)

  local_fun
}

#' Constructors for S3 object to represent R objects.
#'
#' These objects are usually created by the parsers, but it is also
#' useful to generate them by hand for testing.
#'
#' @param value The object itself.
#' @param alias Alias for object being documented, in case you create a
#'   generator function with different name.
#' @export
#' @keywords internal
object <- function(value, alias, type) {
  structure(
    list(
      alias = alias,
      value = value,
      methods = if (type == "rcclass") rc_methods(value),
      topic = object_topic(value, alias, type)
    ),
    class = c(type, "object")
  )
}

#' @export
format.object <- function(x, ...) {
  c(
    paste0("<", class(x)[1], "> ", x$name),
    paste0("  $topic ", x$topic),
    if (!is.null(x$alias)) paste0("  $alias ", x$alias)
  )
}

#' @export
print.object <- function(x, ...) {
  cat_line(format(x, ...))
}

object_topic <- function(value, alias, type) {
  switch(type,
    s4method = paste0(value@generic, ",", paste0(value@defined, collapse = ","), "-method"),
    s4class = paste0(value@className, "-class"),
    s4generic = value@generic,
    rcclass = paste0(value@className, "-class"),
    r6class = alias,
    rcmethod = value@name,
    s3generic = alias,
    s3method = alias,
    import = alias,
    `function` = alias,
    package = alias,
    data = alias,
    stop("Unsupported type '", type, "'", call. = FALSE)
  )
}

call_to_object <- function(code, env = pkg_env(), file = NULL) {
  code <- enexpr(code)

  eval(code, envir = env)
  if (is_call(code, "{")) {
    call <- code[[length(code)]]
  } else {
    call <- code
  }
  object_from_call(call, env, block = NULL, file = file)
}
