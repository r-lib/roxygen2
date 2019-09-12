object_usage <- function(x) {
  UseMethod("object_usage")
}

object_usage.default <- function(x) {
  NULL
}

object_usage.data <- function(x) {
  rd(x$alias)
}

object_usage.function <- function(x) {
  function_usage(x$alias, formals(x$value), identity)
}

object_usage.s3generic <- object_usage.function

object_usage.s3method <- function(x) {
  method <- attr(x$value, "s3method")
  s3method <- function(name) {
    build_rd("\\method{", name, "}{", auto_backtick(method[2]), "}")
  }
  function_usage(method[1], formals(x$value), s3method)
}

object_usage.s4generic <- function(x) {
  function_usage(x$value@generic, formals(x$value), identity)
}

object_usage.s4method <- function(x) {
  s4method <- function(name) {
    classes <- auto_backtick(as.character(x$value@defined))
    build_rd("\\S4method{", name, "}{", paste0(classes, collapse = ","), "}")
  }
  function_usage(x$value@generic, formals(x$value), s4method)
}

# Function usage ----------------------------------------------------------

# Usage:
# replacement, infix, regular
# function, s3 method, s4 method, data

function_usage <- function(name, formals, format_name = identity) {
  arglist <- args_string(usage_args(formals))
  if (is_replacement_fun(name) && !is_infix_fun(name)) {
    name <- str_replace(name, fixed("<-"), "")
    formals$value <- NULL

    arglist <- args_string(usage_args(formals))
    build_rd(format_name(name), "(", arglist, ")\u{A0}<-\u{A0}value")
  } else if (is_infix_fun(name) && identical(format_name, identity)) {
    # If infix, and regular function, munge format
    arg_names <- names(formals)

    build_rd(arg_names[1], " ", format_name(name), " ", arg_names[2])
  } else {
    # Quote non-syntactic names if no special formatting
    if (identical(format_name, identity)) {
      name <- auto_quote(name)
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

usage_args <- function(args) {
  is.missing.arg <- function(arg) {
    is.symbol(arg) && deparse(arg) == ""
  }
  arg_to_text <- function(arg) {
    if (is.missing.arg(arg)) return("")
    text <- enc2utf8(deparse(arg, backtick = TRUE, width.cutoff = 500L))
    text <- paste0(text, collapse = "\n")
    Encoding(text) <- "UTF-8"

    text
  }
  map_chr(args, arg_to_text)
}

args_string <- function(x) {
  sep <- ifelse(x != "", "\u{A0}=\u{A0}", "")
  arg_names <- auto_backtick(names(x))
  paste0(arg_names, sep, x, collapse = ", ")
}

# wrapping ----------------------------------------------------------------

wrap_usage <- function(x, width = 80L) {
  if (is.null(x)) {
    return(NULL)
  }

  y <- wrapUsage(x, width = as.integer(width))
  y <- gsub("\u{A0}", " ", y, useBytes = TRUE)
  Encoding(y) <- "UTF-8"
  class(y) <- class(x)
  y
}

# helpers -----------------------------------------------------------------

# used for testing
call_to_usage <- function(code, env = pkg_env()) {
  obj <- call_to_object(!!enexpr(code), env)
  gsub("\u{A0}", " ", as.character(object_usage(obj)))
}
