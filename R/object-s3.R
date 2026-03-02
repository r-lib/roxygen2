#' Determine if a function is an S3 generic or S3 method.
#'
#' @description
#' `is_s3_generic` compares name to `.knownS3Generics` and
#' `.S3PrimitiveGenerics`, then looks at the function body to see if it
#' calls [UseMethod()].
#'
#' `is_s3_method` builds names of all possible generics for that function
#' and then checks if any of them actually is a generic.
#'
#' @param name Name of function.
#' @param env Base environment in which to look for function definition.
#' @keywords internal
#' @export
is_s3_generic <- function(name, env = parent.frame()) {
  if (name == "") {
    return(FALSE)
  }
  if (!exists(name, envir = env, mode = "function")) {
    return(FALSE)
  }

  f <- get(name, envir = env, mode = "function")
  if (inherits(f, "groupGenericFunction")) {
    return(TRUE)
  }

  ns_name <- tryCatch(getNamespaceName(environment(f)), error = function(e) "")
  if (identical(unname(.knownS3Generics[name]), ns_name)) {
    return(TRUE)
  }

  if (is.primitive(f)) {
    known_generics <- c(
      names(.knownS3Generics),
      internal_f("tools", ".get_internal_S3_generics")()
    )
    return(name %in% known_generics)
  }

  calls_use_method(body(f))
}

calls_use_method <- function(x) {
  # Base cases
  if (missing(x)) {
    return(FALSE)
  }
  if (!is.call(x)) {
    return(FALSE)
  }

  if (identical(x[[1]], quote(UseMethod))) {
    return(TRUE)
  }
  if (length(x) == 1) {
    return(FALSE)
  }
  # Recursive case: arguments to call
  for (arg in as.list(x[-1])) {
    if (calls_use_method(arg)) return(TRUE)
  }

  FALSE
}

#' @rdname is_s3_generic
#' @export
is_s3_method <- function(name, env = parent.frame()) {
  !is.null(find_generic(name, env))
}

is.s3method <- function(x) inherits(x, "s3method")
is.s3generic <- function(x) inherits(x, "s3generic")
is.s3 <- function(x) inherits(x, c("s3method", "s3generic"))

find_generic <- function(name, env = parent.frame()) {
  pieces <- str_split(name, fixed("."))[[1]]
  n <- length(pieces)

  # No . in name, so can't be method
  if (n == 1) {
    return(NULL)
  }

  for (i in seq_len(n - 1)) {
    generic <- paste0(pieces[seq_len(i)], collapse = ".")
    class <- paste0(pieces[(i + 1):n], collapse = ".")

    if (is_s3_generic(generic, env)) return(c(generic, class))
  }
  NULL
}

s3_method <- function(f, method) {
  stopifnot(is.function(f))
  stopifnot(length(method) == 2, is.character(method))

  class(f) <- c("s3method", "function")
  attr(f, "s3method") <- method

  f
}

s3_method_info <- function(x) {
  stopifnot(is.s3(x))
  attr(x, "s3method")
}
