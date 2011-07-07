#' Pre-specify a procedures named parameters, returning a new procedure.
#'
#' Thanks, Byron Ellis.
#' \url{https://stat.ethz.ch/pipermail/r-devel/2007-November/047318.html}
#' @param FUN the function to be curried
#' @param \dots the determining parameters
#' @return A new function partially determined
#' @export
Curry <- function(FUN,...) {
  .orig = list(...);
  function(...) do.call(FUN,c(.orig,list(...)))
}

#' Negate a function; borrowed from src/library/base/R/funprog.R for
#' pre-2.7 Rs.
#' @param f the function to be negated
#' @return The negated function
Negate <- function(f)
  function(...) ! match.fun(f)(...)

#' Compose an arbitrary number of functions.
#
#' My Happy Hacking keyboard gave out during the writing of this
#' procedure; moment of silence, please.
#' @param \dots the functions to be composed
#' @return A composed function
#' @callGraphPrimitives
#' @callGraphDepth 3
#' @export
Compose <- function(...) {
  fs <- list(...)
  function(...) Reduce(function(x, f) f(x),
                       fs,
                       ...)
}

#' Identity function.
#'
#' Is concatenation benign?
#' @param \dots tautological arguments
#' @return The tautologized arguments, concatenated
Identity <- function(...) c(...)
