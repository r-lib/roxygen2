# NOTE: Most of the parsers require full specification, e.g.,
# representation=representation(...) or signature=signature(object="numeric")

cdrpfe <- function(expression)
  cdr(preorder.flatten.expression(expression))

parseS4.class <- function(expression) {
  formals <- list()

  if ( !is.null(expression$representation) ) {
    formals$representation <- cdrpfe(expression$representation)

    if ( !is.na(i <- match('VIRTUAL', formals$representation)) ) {
      formals$virtual <- TRUE
      formals$representation[i] <- NULL
    }
  }
  
  if ( !is.null(expression$contains) )
    formals <- append(formals,
                      list(contains=
                           cdrpfe(expression$contains)))
  if ( !is.null(expression$prototype) )
    formals <- append(formals,
                      list(prototype=
                           cdrpfe(expression$prototype)))
  formals
}

parseS4.method <- function(expression) {
  # Heuristic that the first unnamed language
  # object is the definition:
  def <- which(sapply(expression, is.call) & names(expression) == '')[1]

  formals <- list(definition=
                  parse.formals(expression[c(def, def+1)])[[1]])

  if ( !is.null(expression$signature) )
    formals <- append(formals,
                      list(signature=
                           cdrpfe(expression$signature)))
  
  formals
}

