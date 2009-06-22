
parseS4.class <- function(expression) {
  formals <- list(representation=
                  cdr(preorder.flatten.expression(expression$representation)))
  if ( !is.null(expression$contains) )
    formals <- append(formals, list(contains=expression$contains))
  if ( !is.null(expression$prototype) )
    formals <- append(formals,
                      list(prototype=
                           cdr(preorder.flatten.expression(expression$prototype))))
  formals
}
