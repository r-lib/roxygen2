# Prefer explicit \code{@@usage} to a \code{@@formals} list.
process_usage <- function(partitum) {
  if (is.null(partitum$usage)) {
    usage <- wrap_string(default_usage(partitum$object))
  } else if (partitum$usage == "NULL") {
    usage <- NULL
  } else {
    # Treat user input as already escaped, otherwise they have no way
    # to enter \S4method etc.
    usage <- rd(partitum$usage)
  }
  new_tag("usage", usage)
}

wrap_string <- function(x) UseMethod("wrap_string")
wrap_string.NULL <- function(x) return(x)
wrap_string.default <- function(x) {
  y <- wrapString(x)
  y <- gsub("\u{A0}", " ", y, useBytes = TRUE)
  Encoding(y) <- "UTF-8"
  class(y) <- class(x)
  y
}

default_usage <- function(x) {
  UseMethod("default_usage")
}

#' @export
default_usage.default <- function(x) NULL

#' @export
default_usage.NULL <- function(x) NULL

#' @export
default_usage.data <- function(x) x$alias

#' @export
default_usage.function <- function(x) {
  function_usage(x$alias, formals(x$value), identity)
}

#' @export
default_usage.s3generic <- default_usage.function

#' @export
default_usage.s3method <- function(x) {
  method <- attr(x$value, "s3method")
  s3method <- function(name) {
    build_rd("\\method{", name, "}{", method[2], "}")
  }
  function_usage(method[1], formals(x$value), s3method)
}

#' @export
default_usage.s4generic <- function(x) {
  function_usage(x$value@generic, formals(x$value), identity)
}


#' @export
default_usage.s4method <- function(x) {
  s4method <- function(name) {
    classes <- as.character(x$value@defined)
    needs_backtick <- !is.syntactic(classes)
    classes[needs_backtick] <- paste0("`", classes[needs_backtick], "`")

    build_rd("\\S4method{", name, "}{", paste0(classes, collapse = ","), "}")
  }
  function_usage(x$value@generic, formals(x$value), s4method)
}

#' @export
default_usage.s4class <- function(x) NULL

#' @export
default_usage.rcclass <- function(x) NULL


# Usage:
# replacement, infix, regular
# function, s3 method, s4 method, data

function_usage <- function(name, formals, format_name = identity) {
  arglist <- args_string(usage_args(formals))
  if (is_replacement_fun(name)) {
    name <- str_replace(name, fixed("<-"), "")
    formals$value <- NULL

    arglist <- args_string(usage_args(formals))
    build_rd(format_name(name), "(", arglist, ") <- value")
  } else if (is_infix_fun(name) && identical(format_name, identity)) {
    # If infix, and regular function, munge format
    arg_names <- names(formals)

    build_rd(arg_names[1], " ", format_name(name), " ", arg_names[2])
  } else {
    # Quote non-syntactic names if no special formatting
    if (identical(format_name, identity)) {
      name <- quote_if_needed(name)
    }

    build_rd(format_name(name), "(", arglist, ")")
  }

}

is_replacement_fun <- function(name) {
  str_detect(name, fixed("<-"))
}
is_infix_fun <- function(name) {
  str_detect(name, "^%.*%$")
}

# Given argument list, produce usage specification for it.
#
# Adapted from \code{\link{prompt}}.
#
# @param f function, or name of function, as string
# @return a string
usage_args <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- deparse(arg, backtick = TRUE, width.cutoff = 500L)
    text <- paste0(text, collapse = "\n")
    Encoding(text) <- "UTF-8"

    text
  }
  vapply(args, arg_to_text, character(1))
}

args_string <- function(x) {
  missing_arg <- x == ""
  sep <- ifelse(!missing_arg, "\u{A0}=\u{A0}", "")

  arg_names <- names(x)
  needs_backtick <- !is.syntactic(arg_names)
  arg_names[needs_backtick] <- paste0("`", arg_names[needs_backtick], "`")

  paste0(arg_names, sep, x, collapse = ", ")
}
