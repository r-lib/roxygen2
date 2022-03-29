#' @export
roxy_tag_parse.roxy_tag_usage <- function(x) {
  x <- tag_value(x)
  x$val <- rd(x$val)
  x
}

#' @export
roxy_tag_rd.roxy_tag_usage <- function(x, base_path, env) {
  if (identical(x$val, rd("NULL"))) {
    usage <- NULL
  } else {
    usage <- x$val
  }
  rd_section("usage", usage)
}

#' @export
format.rd_section_usage <- function(x, ...) {
  rd_macro(x$type, build_rd(x$value, collapse = "\n\n"), space = TRUE)
}

# object_usage ------------------------------------------------------------

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
    paste0("\\method{", name, "}{", auto_backtick(method[2]), "}")
  }
  function_usage(method[1], formals(x$value), s3method)
}

object_usage.s4generic <- function(x) {
  function_usage(x$value@generic, formals(x$value), identity)
}

object_usage.s4method <- function(x) {
  s4method <- function(name) {
    classes <- auto_backtick(as.character(x$value@defined))
    paste0("\\S4method{", name, "}{", paste0(classes, collapse = ","), "}")
  }
  function_usage(x$value@generic, formals(x$value), s4method)
}

# Function usage ----------------------------------------------------------

# Usage:
# replacement, infix, regular
# function, s3 method, s4 method, data

function_usage <- function(name, formals, format_name = identity) {
  if (is_replacement_fun(name) && !is_infix_fun(name)) {
    name <- str_replace(name, fixed("<-"), "")
    if (identical(format_name, identity)) {
      name <- auto_backtick(name)
    }
    name <- gsub("%", "\\%", name, fixed = TRUE)
    formals$value <- NULL

    wrap_usage(name, format_name, formals, suffix = " <- value")
  } else if (is_infix_fun(name) && identical(format_name, identity)) {
    # If infix, and regular function, munge format
    arg_names <- names(formals)
    build_rd(arg_names[1], " ", format_name(name), " ", arg_names[2])
  } else {
    if (identical(format_name, identity)) {
      name <- auto_backtick(name)
    }
    name <- gsub("%", "\\%", name, fixed = TRUE)
    wrap_usage(name, format_name, formals)
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
wrap_usage <- function(name, format_name, formals, suffix = NULL, width = 80L) {
  args <- args_string(usage_args(formals))

  # Do we need any wrapping?
  bare <- args_call(name, args)
  if (nchar(bare, type = "width") < width) {
    out <- args_call(format_name(name), args)
  } else if (roxy_meta_get("old_usage", FALSE)) {
    x <- args_call(format_name(name), args)
    out <- wrapUsage(x, width = as.integer(width), indent = 2)
  } else {
    args <- paste0("  ", args)
    args <- map_chr(args, wrapUsage, width = 90, indent = 4)
    out <- paste0(format_name(name), "(\n", paste0(args, collapse = ",\n"), "\n)")
  }

  out <- gsub("\u{A0}", " ", out, useBytes = TRUE)
  Encoding(out) <- "UTF-8"

  rd(paste0(out, suffix))
}

# helpers -----------------------------------------------------------------

# used for testing
call_to_usage <- function(code, env = pkg_env()) {
  obj <- call_to_object(!!enexpr(code), env)
  gsub("\u{A0}", " ", as.character(object_usage(obj)))
}
