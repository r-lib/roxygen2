#' Test an integer for primality with Fermat's little theorem.
#'
#' Fermat's little theorem states that if \eqn{n} is a prime
#' number and \eqn{a} is any positive integer less than \eqn{n},
#' then \eqn{a} raised to the \eqn{n}th power is congruent to
#' \eqn{a\ modulo\ n}{a modulo n}.
#'
#' @param n the integer to test for primality
#' @return Whether the integer passes the Fermat test
#'   for a randomized \eqn{0 < a < n}
#' @callGraphPrimitives
#' @note \code{fermat.test} doesn't work for integers above
#'   approximately fifteen because modulus loses precision.
#' @references
#' \url{http://en.wikipedia.org/wiki/Fermat's_little_theorem}
#' @author Peter Danenberg \email{pcd@@roxygen.org}
fermat.test <- function(n) {
  a <- floor(runif(1, min=1, max=n))
  a ^ n %% n == a
}
