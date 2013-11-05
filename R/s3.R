#' Determine if a function is an S3 generic or S3 method.
#'
#' @description
#' \code{is_s3_generic} compares name to \code{.knownS3Generics} and
#' \code{.S3PrimitiveGenerics}, then uses \code{\link[codetools]{findGlobals}}
#' to see if the functionion calls \code{\link{UseMethod}}.
#'
#' \code{is_s3_method} builds names of all possible generics for that function
#' and then checks if any of them actually is a generic.
#'
#' @param name name of function.
#' @param env environment to search in.
#' @export
#' @importFrom memoise memoise
#' @importFrom codetools findGlobals
is_s3_generic <- function(name, env = parent.frame()) {
  if (name == "") return(FALSE)
  if (!exists(name, envir = env)) return(FALSE)
  
  f <- get(name, envir = env)
  if (!is.function(f)) return(FALSE)
  
  if (is.primitive(f)) {
    known_generics <- c(names(.knownS3Generics),
      tools:::.get_internal_S3_generics())
    return(name %in% known_generics)
  }
  
  uses <- findGlobals(f, merge = FALSE)$functions
  any(uses == "UseMethod")
}

is_s3_method <- function(name, env = parent.frame()) {
  !is.null(find_generic(name, env))
}

is.s3method <- function(x) inherits(x, "s3method")
is.s3generic <- function(x) inherits(x, "s3generic")
is.s3 <- function(x) inherits(x, c("s3method", "s3generic"))

find_generic <- memoise(function(name, env = parent.frame()) {
  pieces <- str_split(name, fixed("."))[[1]]
  n <- length(pieces)
  
  # No . in name, so can't be method
  if (n == 1) return(NULL)
  
  for(i in seq_len(n - 1)) {
    generic <- str_c(pieces[seq_len(i)], collapse = ".")
    class <- str_c(pieces[(i + 1):n], collapse = ".")
    
    if (is_s3_generic(generic, env)) return(c(generic, class))
  }
  NULL
})

all_s3_methods <- memoise(function(env = parent.frame()) {
  names <- ls(envir = env)
  results <- compact(lapply(names, find_generic, env = env))
  if (length(results) == 0) return()
  
  t(simplify2array(results))
})


add_s3_metadata <- function(val, name, env) {
  if (!is.function(val)) return(val)
  
  if (is_s3_generic(name, env)) {
    class(val) <- "s3generic"
    return(val)
  }
  
  method <- find_generic(name, env)
  if (is.null(method)) return(val)
  
  class(val) <- "s3method"
  attr(val, "s3method") <- method
  attr(val, "s3env") <- env
  
  val
}

s3_method_info <- function(x) {
  stopifnot(is.s3(x))
  attr(x, "s3method")
}
