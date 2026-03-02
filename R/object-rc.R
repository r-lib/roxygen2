#' @export
roxy_tag_rd.roxy_tag_.methods <- function(x, base_path, env) {
  desc <- lapply(x$val, \(x) docstring(x$value@.Data))
  usage <- map_chr(x$val, function(x) {
    function_usage(x$value@name, formals(x$value@.Data))
  })

  has_docs <- !map_lgl(desc, is.null)
  desc <- desc[has_docs]
  usage <- usage[has_docs]

  rd_section("rcmethods", setNames(desc, usage))
}
#' @export
format.rd_section_rcmethods <- function(x, ...) {
  rd_section_description("Methods", names(x$value), x$value)
}

# Extract all methods from an RC definition, returning a list of "objects".
rc_methods <- function(obj) {
  stopifnot(methods::is(obj, "refClassRepresentation"))

  parents <- obj@refSuperClasses
  parent_methods <- unlist(lapply(parents, function(x) {
    methods::getRefClass(x)$methods()
  }))
  method_names <- sort_c(setdiff(ls(envir = obj@refMethods), parent_methods))
  methods <- mget(method_names, envir = obj@refMethods)

  lapply(methods, object, alias = NULL, type = "rcmethod")
}

add_rc_metadata <- function(val, name, class) {
  class(val) <- c("rcmethod", "function")
  attr(val, "rcclass") <- class
  attr(val, "rcmethod") <- name

  val
}

get_method <- function(obj, method_name) {
  eval(call("$", quote(obj), as.name(method_name)))
}

# Modified from methods:::.refMethodDoc - a function has a doc string
# if it's a call to {, with more than 1 element, and the first element is
# a character vector.
docstring <- function(f) {
  stopifnot(is.function(f))
  if (is.primitive(f)) {
    return(NULL)
  }

  b <- body(f)
  if (length(b) <= 2 || !identical(b[[1]], quote(`{`))) {
    return(NULL)
  }

  first <- b[[2]]
  if (!is.character(first)) {
    return(NULL)
  }
  if (first == "") {
    return(NULL)
  }

  trim_docstring(first)
}

# Implementation converted from
# http://www.python.org/dev/peps/pep-0257/#handling-docstring-indentation
trim_docstring <- function(docstring) {
  if (docstring == "") {
    return("")
  }

  # Convert tabs to spaces (using four spaces for tabs)
  # and split into a vector of lines:
  lines <- strsplit(gsub("\t", "    ", docstring), "\n")[[1]]
  if (length(lines) == 1) {
    return(strip(lines))
  }

  # Determine minimum indentation (first line doesn't count):
  indent <- min(leadingSpaces(lines[-1]))

  # Remove indentation (first line is special):
  trimmed <- c(
    strip(lines[1]),
    substr(lines[-1], indent + 1, 1000L)
  )

  # Return a single string:
  string <- paste0(trimmed, collapse = "\n")

  # Strip off trailing and leading blank lines:
  gsub("^\n+|\n+$", "", string)
}

strip <- function(x) gsub("^ +| + $", "", x)
