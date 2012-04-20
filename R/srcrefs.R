#' @include parse-registry.R

parse_assignment <- function(call, env) {
  assignee <- as.character(call[[2]])
  
  # If it doesn't exist (any more), don't document it.
  if (!exists(assignee, env)) return()

  out <- list(
    src_name = assignee,
    src_alias = assignee)

  value <- get(assignee, env)
  if (is.function(value)) {
    out$src_type <- "function"
    out$formals <- formals(value)
  } else if (inherits(value, "refObjectGenerator")) {
    out$src_type <- "ref-class"
  } else {
    out$src_type <- "data"
    out$str <- str_c(capture.output(str(value, max.level = 1)), 
      collapse = "\n")
  }

  out
}

parse_class <- function(call, env) {
  name <- as.character(call$Class)
  class <- getClass(name, where = env)

  # Default alias is A-class
  aliases <- str_c(name, "-class")
  # add plain name to aliases only if there is no function named like the class, 
  # which would typically be a constructor method. 
  if( is.null(getFunction(name, where = env, mustFind=FALSE)) ){
	  aliases <- c(name, aliases)
  }
  
  # class?classRepresentation
  # - Default rdname should be A-class.rd and not duplicate alias if a 
  # function exists with the same name as the class (see above). 
  # NB: name is added to aliases in roclet_rd_one by:
  # add_tag(rd, new_tag("alias", partitum$name %||% partitum$src_alias))
  #
  list(
    src_type = "class",
    src_name = str_c(name, "-class"), 
    src_alias = aliases,
    extends = showExtends(class@contains, printTo = FALSE),
    slots = class@slots
  )
}

parse_generic <- function(call, env) {
  name <- as.character(call$name)
  f <- getGeneric(name, where = env)
  
  list(
    src_type = "function",
    src_name = topic_name(f),
    src_alias = c(name, str_c(name, "-methods")),
    formals = formals(f@.Data)
  )
}

parse_method <- function(call, env, replace=FALSE) {

  name <- as.character(call$f)
  sig <- eval(call$signature)

  # handle replacement methods
  if( replace ){
	  name <- str_c(name, '<-')
	  sig <- eval(call[[3]])
  }
  
  f <- getMethod(name, sig, where = env)
  pkg <- attr(f@generic, "package")
  if (pkg == "roxygen_test") {
    inherit <- f@generic
  } else {
    inherit <- str_c(pkg, "::", f@generic)
  }

  # class?MethodDefinition
  list(
    src_type = "method",
    src_name = topic_name(f),
    src_alias = topic_name(f),
    generic = f@generic,
    formals = formals(f@.Data),
    signature = method_signature(f),
    inheritParams = inherit
  )
}

register.srcref.parser('<-', parse_assignment)
register.srcref.parser('=', parse_assignment)
register.srcref.parser('setClass', parse_class)
register.srcref.parser('setGeneric', parse_generic)
register.srcref.parser('setMethod', parse_method)
register.srcref.parser('setReplaceMethod', function(...) parse_method(..., replace=TRUE))

# Computes S4 Method Signatures
# 
# This function corrects the issue with generic defined within the package, for which
# the function getMethod does not detail arguments that are not specified in the 
# signature as objects of implicit class 'ANY'.
# Arguments '...' are also correctly _not_ taken into account for the signature
#
method_signature <- function(x){
	# check for the case where not all arguments get tagged as class 'ANY'
	sig <- as.character(x@defined)
	if( is.function(x@.Data) && length(args <- formalArgs(x@.Data)) != length(sig) ){
		
		# remove possible argument '...'
		args <- args[args != '...']
		l <- length(args) - length(sig)
		if( l > 0L ){
			# append correct number of 'ANY'
			sig <- c(sig, setNames(rep('ANY', l), tail(args, l)))
		}else if( l < 0L )
			warning("roxygen::topic_name - Unexpectedly unable to infer signature for method "
					, x@generic, ",", sig, call.=FALSE, immediate.=TRUE)
	}
	sig
}

setGeneric("topic_name", function(x) {
  standardGeneric("topic_name")
})
setMethod("topic_name", signature(x = "MethodDefinition"), function(x) {
  str_c(str_c(c(x@generic, method_signature(x)), collapse = ","), "-method")
})
setMethod("topic_name", signature(x = "standardGeneric"), function(x) {
  x@generic
})
setMethod("topic_name", signature(x = "nonstandardGenericFunction"), function(x) {
  x@generic
})