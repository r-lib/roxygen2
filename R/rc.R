# Extract all methods from an RC definition, returning a list of "objects".
rc_methods <- function(obj) {
  stopifnot(is(obj, "refClass"))
  if (is(obj, "refObjectGenerator")) {
    obj <- obj$def
  }
  
  base_methods <- getRefClass("envRefClass")$methods()
  method_names <- setdiff(ls(envir = obj@refMethods), base_methods)
  methods <- mget(method_names, envir = obj@refMethods)
  
  object_from_method <- function(f) {
    object("rcmethod", f@name, f)
  }
  lapply(methods, object_from_method)
}

get_method <- function(obj, method_name) {
  eval(call("$", quote(obj), as.name(method_name)))
}

# Modified from methods:::.refMethodDoc - a function has a doc string
# if it's a call to {, with more than 1 element, and the first element is
# a character vector.
docstring <- function(f) {
  stopifnot(is.function(f))
  if (is.primitive(f)) return(NULL)
  
  b <- body(f)
  if (!identical(b[[1]], quote(`{`)) || length(b) <= 2) return(NULL)
  
  first <- b[[2]]
  if (!is.character(first)) return(NULL)
  
  trim_docstring(first)
}

# Implementation converted from 
# http://www.python.org/dev/peps/pep-0257/#handling-docstring-indentation
trim_docstring <- function(docstring) {
  if (docstring == "") return(NULL)
  
  # Convert tabs to spaces (using four spaces for tabs)
  # and split into a vector of lines:
  lines <- strsplit(gsub("\t", "    ", docstring), "\n")[[1]]
  if (length(lines) == 1) return(strip(lines))
  
  # Determine minimum indentation (first line doesn't count):
  stripped <- gsub("^ +", "", lines)
  indent <- min(nchar(lines[-1]) - nchar(stripped[-1]))
  
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
