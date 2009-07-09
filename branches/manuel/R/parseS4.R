# NOTE: Most of the parsers require full specification, e.g.,
# representation=representation(...) or signature=signature(object="numeric")

cdr.expression <- function(expression)
  cdr(preorder.flatten.expression(expression))

parseS4.class <- function(expression) {
  formals <- list(representation=
                  cdr.expression(expression$representation))
  if ( !is.null(expression$contains) )
    formals <- append(formals,
                      list(contains=
                           cdr.expression(expression$contains)))
  if ( !is.null(expression$prototype) )
    formals <- append(formals,
                      list(prototype=
                           cdr.expression(expression$prototype)))
  formals
}

parseS4.method <- function(expression) {
  # Heuristic that the first unnamed language
  # object is the definition:
  def <- which(sapply(expression, is.call) & names(expression) == '')[1]

  formals <- list(signature=
                  cdr.expression(expression$signature),
                  definition=
                  parse.formals(expression[c(def, def+1)])[[1]])
  
  formals
}
