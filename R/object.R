#' Constructors for S3 object to represent R objects.
#'
#' These objects are usually created by the parsers, but it is also
#' useful to generate them by hand for testing.
#'
#' @param subclass This is an abstract class so this must be provided.
#'   Currently this is one of "function", "s4generic", "s4class", "s4method",
#'   "rcclass", or "data".
#' @param alias Alias for object being documented, in case you create a
#'   Name of the object being documented
#' @param value The object itself.
#' @param ... optional additional fields used by subclasses
#' @param alias Used for \code{\link{setClass}} and \code{\link{setRefClass}}
#'   to capture the name of the created object.
#' @export
#' @keywords internal
object <- function(value, name = NULL) {
  type <- obj_type(value)

  # S4/RC class with generator function
  alias <- name
  if (type %in% c("s4class", "rcclass")) {
    name <- as.character(value@className)
  } else if (type %in% "rcmethod") {
    name <- value@name
  }

  structure(
    list(
      name = name,
      alias = alias,
      value = value,
      methods = if (type == "rcclass") rc_methods(value)
    ),
    class = c(type, "object")
  )
}

#' @export
print.object <- function(x, ...) {
  cat("<", class(x)[1], "> ", x$name,
    if (!is.null(x$alias)) paste0(" (", x$alias, ")"), "\n",
    sep = ""
  )
}

# Take object created by assignment and standardise
standardise_obj <- function(name, value, env = emptyenv(), block = list()) {
  if (is_generator(value)) {
    # S4 and RC generators need to be converted to their classes
    getClass(as.character(value@className), where = env)
  } else if (inherits(value, "MethodDefinition")) {
    # S4 methods need munging to get real function def
    value@.Data <- extract_method_fun(value@.Data)
    value
  } else if (is.function(value)) {
    # Potential S3 methods/generics need metadata added
    method <- unlist(block$method, use.names = FALSE)
    add_s3_metadata(value, name, env, method)
  } else {
    value
  }
}

is_generator <- function(x) {
  is(x, "refObjectGenerator") || is(x, "classGeneratorFunction")
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
extract_method_fun <- function(x) {
  fun <- x@.Data

  method_body <- body(fun)
  if (!is.call(method_body)) return(fun)
  if (!identical(method_body[[1]], quote(`{`))) return(fun)

  first_line <- method_body[[2]]
  if (!identical(first_line[[1]], quote(`<-`))) return(fun)
  if (!identical(first_line[[2]], quote(`.local`))) return(fun)

  first_line[[3]]
}

# Consistent naming scheme for R object classes --------------------------------
# (s3/s4/rc x generic/class/method, function, data)

obj_type <- function(x) UseMethod("obj_type")

#' @export
obj_type.s3generic <- function(x) "s3generic"
#' @export
obj_type.s3method <- function(x) "s3method"

#' @export
obj_type.classRepresentation <- function(x) "s4class"
#' @export
obj_type.genericFunction <- function(x) "s4generic"
#' @export
obj_type.MethodDefinition <- function(x) "s4method"

#' @export
obj_type.refClassRepresentation <- function(x) "rcclass"
#' @export
obj_type.refMethodDef <- function(x) "rcmethod"

#' @export
obj_type.function <- function(x) "function"
#' @export
obj_type.default <- function(x) "data"
