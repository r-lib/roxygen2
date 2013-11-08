# Prefer explicit \code{@@usage} to a \code{@@formals} list.
usage_tag <- function(partitum) {
  usage <- partitum$usage %||% default_usage(partitum$object)
  new_tag("usage", usage)
}

default_usage <- function(x) {
  UseMethod("default_usage")
}

#' @export
default_usage.NULL <- function(x) NULL

#' @export
default_usage.data <- function(x) x$name

#' @export
default_usage.function <- function(x) {
  function_usage(x$name, formals(x$value), identity)
}

#' @export
default_usage.s3method <- function(x) {
  method <- attr(x$value, "s3method")
  s3method <- function(name) {
    paste0("\\S3method{", name, "}{", method[2], "}")
  }
  function_usage(method[1], formals(x$value), s3method)
}

#' @export
default_usage.s4method <- function(x) {
  s4method <- function(name) {
    signature <- str_c(as.character(x$value@defined), collapse = ",")
    paste0("\\S4method{", name, "}{", signature, "}")
  }
  function_usage(x$name, formals(x$value), s4method)
}

# Usage:
# replacement, infix, regular
# function, s3 method, s4 method, data

function_usage <- function(name, formals, format_name = identity) {
  arglist <- args_string(usage_args(formals))
  if (is_replacement_fun(name)) {
    name <- str_replace(name, fixed("<-"), "")
    formals$value <- NULL
    
    arglist <- args_string(usage_args(formals))
    str_c(format_name(name), "(", arglist, ") <- value")
  } else if (is_infix_fun(name) && identical(format_name, identity)) {
    # If infix, and regular function, munge format
    arg_names <- names(formals)
    name <- str_replace_all(name, fixed("%"), "\\%")
    
    str_c(arg_names[1], " ", format_name(name), " ", arg_names[2])
  } else {
    str_c(format_name(name), "(", arglist, ")")
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
    text <- str_replace_all(text, fixed("%"), "\\%")
    text <- str_replace_all(text, fixed(" "), "\u{A0}")
    Encoding(text) <- "UTF-8"
    
    text
  }
  vapply(args, arg_to_text, character(1))
}

args_string <- function(x) {
  missing_arg <- x == ""
  sep <- ifelse(!missing_arg, "\u{A0}=\u{A0}", "")
  
  str_c(names(x), sep, x, collapse = ", ")
}
