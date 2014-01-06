# Extract all methods from an RC definition, returning a list of "objects".
rc_methods <- function(obj) {
  stopifnot(is(obj, "refClass"))
  if (is(obj, "refObjectGenerator")) {
    obj <- obj$def
  }
  
  base_methods <- getRefClass("envRefClass")$methods()
  method_names <- setdiff(ls(env = obj@refMethods), base_methods)
  methods <- mget(method_names, env = obj@refMethods)
  
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
  
  b[[2]]
}

