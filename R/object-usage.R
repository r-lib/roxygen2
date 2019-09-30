#' @export
roxy_tag_parse.roxy_tag_usage <- function(x) {
  tag_value(x)
}

# Prefer explicit \code{@@usage} to a \code{@@formals} list.
topic_add_usage <- function(topic, block, old_usage = FALSE) {
  tag <- block_get_tag(block, "usage")

  if (is.null(tag)) {
    usage <- object_usage(block$object, old_usage = old_usage)
  } else if (tag$val == "NULL") {
    usage <- NULL
  } else {
    # Treat user input as already escaped, otherwise they have no way
    # to enter \S4method etc.
    usage <- rd(tag$val)
  }
  topic$add_simple_field("usage", usage)
}

#' @export
format.rd_section_usage <- function(x, ...) {
  rd_macro(x$type, build_rd(x$value, collapse = "\n\n"), space = TRUE)
}

# object_usage ------------------------------------------------------------

object_usage <- function(x, old_usage = FALSE) {
  UseMethod("object_usage")
}

object_usage.default <- function(x, old_usage = FALSE) {
  NULL
}

object_usage.data <- function(x, old_usage = FALSE) {
  rd(x$alias)
}

object_usage.function <- function(x, old_usage = FALSE) {
  function_usage(x$alias, formals(x$value), identity, old_usage = old_usage)
}

object_usage.s3generic <- object_usage.function

object_usage.s3method <- function(x, old_usage = FALSE) {
  method <- attr(x$value, "s3method")
  s3method <- function(name) {
    build_rd("\\method{", name, "}{", auto_backtick(method[2]), "}")
  }
  function_usage(method[1], formals(x$value), s3method, old_usage = old_usage)
}

object_usage.s4generic <- function(x, old_usage = FALSE) {
  function_usage(x$value@generic, formals(x$value), identity, old_usage = old_usage)
}

object_usage.s4method <- function(x, old_usage = FALSE) {
  s4method <- function(name) {
    classes <- auto_backtick(as.character(x$value@defined))
    build_rd("\\S4method{", name, "}{", paste0(classes, collapse = ","), "}")
  }
  function_usage(x$value@generic, formals(x$value), s4method, old_usage = old_usage)
}

# Function usage ----------------------------------------------------------

# Usage:
# replacement, infix, regular
# function, s3 method, s4 method, data

function_usage <- function(name, formals, format_name = identity, old_usage = FALSE) {
  if (is_replacement_fun(name) && !is_infix_fun(name)) {
    name <- str_replace(name, fixed("<-"), "")
    formals$value <- NULL

    wrap_usage(name, format_name, formals, suffix = " <- value", old_usage = old_usage)
  } else if (is_infix_fun(name) && identical(format_name, identity)) {
    # If infix, and regular function, munge format
    arg_names <- names(formals)
    build_rd(arg_names[1], " ", format_name(name), " ", arg_names[2])
  } else {
    wrap_usage(name, format_name, formals, old_usage = old_usage)
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
  arg_names <- escape(auto_backtick(names(x)))
  paste0(arg_names, sep, escape(x))
}

args_call <- function(call, args) {
  paste0(call, "(", paste0(args, collapse = ", "), ")")
}

#' @param name Function name
#' @param format_name Single argument that returns formatted function name
#' @param formals List of function formals
#' @param suffix Optional suffix, used for replacement functions
#' @noRd
wrap_usage <- function(name, format_name, formals, suffix = NULL, width = 80L, old_usage = FALSE) {
  # Quote non-syntactic names if no special formatting
  if (identical(format_name, identity)) {
    name <- auto_quote(name)
  }

  args <- args_string(usage_args(formals))

  # Do we need any wrapping?
  bare <- args_call(name, args)
  if (nchar(bare, type = "width") < width) {
    out <- args_call(format_name(name), args)
  } else if (old_usage) {
    x <- args_call(format_name(name), args)
    out <- wrapUsage(x, width = as.integer(width))
  } else {
    args <- paste0("  ", args)
    args <- map_chr(args, wrapUsage, width = 95, indent = 4)
    out <- paste0(format_name(name), "(\n", paste0(args, collapse = ",\n"), "\n)")
  }

  out <- gsub("\u{A0}", " ", out, useBytes = TRUE)
  Encoding(out) <- "UTF-8"

  rd(paste0(out, suffix))
}

# helpers -----------------------------------------------------------------

# used for testing
call_to_usage <- function(code, old_usage = FALSE, env = pkg_env()) {
  obj <- call_to_object(!!enexpr(code), env)
  gsub("\u{A0}", " ", as.character(object_usage(obj, old_usage = old_usage)))
}
