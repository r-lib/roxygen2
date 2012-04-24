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

  # class?classRepresentation
  # Default @name should be CLASSNAME-class and not CLASSNAME, as it can clash 
  # with a function with te same name (e.g. a constructor function), which 
  # must have this @name.
  # This is achieved via $src_topic which takes precedence over $src_name for default 
  # @name ($src_name is used in several places as the R access name for the class)
  # Access via ?CLASSNAME is ensured by appropriate default @alias.
  topic <- topic_name(class)
  list(
    src_type = "class",
    src_name = name, 
    src_alias = c(name, topic),
	src_topic = topic,
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
	# inherit from the generic defined within the package
	# which is uniquely identified by its topic_name
    inherit <- topic_name(getGeneric(f@generic, where = env))
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
setMethod("topic_name", signature(x = "classRepresentation"), function(x) {
  str_c(x@className, "-class")
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