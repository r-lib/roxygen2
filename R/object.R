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
object <- function(value, alias = NULL, type = obj_type(value)) {
  structure(
    list(
      alias = alias,
      value = value,
      methods = if (type == "rcclass") rc_methods(value)
    ),
    class = c(type, "object")
  )
}
default_name <- function(x) UseMethod("default_name")
default_name.s4class <-   function(x) x$value@className
default_name.s4generic <- function(x) x$value@generic
default_name.s4method <-  function(x) x$value@generic
default_name.rcclass <-   function(x) x$value@className
default_name.rcmethod <-  function(x) x$value@name
default_name.s3generic <- function(x) browser()
default_name.s3method <-  function(x) attr(x$value, "s3method")
default_name.function <-   function(x) x$alias
default_name.default <-   function(x) NULL

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
    methods::getClass(as.character(value@className), where = env)
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
  methods::is(x, "refObjectGenerator") || methods::is(x, "classGeneratorFunction")
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
  if (!is.call(first_line)) return(fun)
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
obj_type.package <- function(x) "package"
#' @export
obj_type.default <- function(x) "data"
