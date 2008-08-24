#' Test an integer for primality with Fermat's Little Theorem.
#'
#' Fermat's Little Theorem states that if \eqn{n} is a prime number and
#' \eqn{a} is any positive integer less than \eqn{n}, then \eqn{a} raised
#' to the \eqn{n}th power is congruent to \eqn{a modulo n}.
#'
#' @author Peter Danenberg \email{pcd@@roxygen.org}
#' @param n the integer to test for primality
#' @return Whether the integer passes the Fermat test
#'   for a randomized 0 < a < n
fermat.test <- function(n) {
  a <- floor(runif(1, min=1, max=n))
  isTRUE(a ^ n %% n == a)
}

#' Check an integer for pseudo-primality to an arbitrary precision.
#'
#' A number is pseudo-prime if it is probably prime, the basis of
#' which is the probabilistic Fermat test; if it passes two such
#' tests, the chances are better than 3 out of 4 that \eqn{n} is prime.
#'
#' @author Peter Danenberg \email{pcd@@roxygen.org}
#' @param n the integer to test for pseudoprimality.
#' @param times the number of Fermat tests to perform
#' @return Whether the number is pseudoprime
#' @export
#' @seealso \code{\link{fermat.test}}
#' @references Abelson, Hal; Jerry Sussman, and Julie Sussman.
#'   Structure and Interpretation of Computer Programs. Cambridge:
#'   MIT Press, 1984.
#' @keywords pseudoprime fermat
#' @examples
#'   ## Will be true most of the time
#'   is.pseudoprime(13, 4)
is.pseudoprime <- function(n, times) {
  if (times == 0) TRUE
  else if (fermat.test(n)) is.fermat.prime(n, times - 1)
  else FALSE
}
