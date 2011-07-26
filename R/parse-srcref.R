
# Parse a srcref
parse.srcref <- function(ref, env) {
  srcfile <- attributes(ref)$srcfile
  srcref <- list(srcref = 
    list(filename = srcfile$filename, lloc = as.vector(ref)))

  # Get code from source and parse to extract first call
  lines <- getSrcLines(srcfile, ref[[1]], ref[[3]])
  call <- parse(text = lines)[[1]]
  
  if (!is.call(call)) {
    return(c(srcref, list(value = deparse(call))))
  }

  # Dispatch to registered srcref parsers based on call
  name <- as.character(call[[1]])
  if (length(name) > 1) return(srcref)
  parser <- srcref.parsers[[name]]
  if (is.null(parser)) return(srcref)
  
  f <- eval(call[[1]], env)
  # If not a primitive function, use match.call so argument handlers
  # can use argument names
  if (!is.primitive(f)) {
    call <- match.call(eval(call[[1]], env), call)
  }
  c(srcref, parser(call, env))
}
