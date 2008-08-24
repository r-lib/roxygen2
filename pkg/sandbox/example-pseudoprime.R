#' Test an integer for primality with Fermat's Little Theorem.
#'
#' Fermat's Little Theorem states that if \eqn{n} is a prime number and
#' \eqn{a} is any positive integer less than \eqn{n}, then a raised to the
#' \eqn{n}th power is congruent to \eqn{a modulo n}.
#'
#' @param n the integer to test for primality
#' @return Whether the integer passes the Fermat test
#' for a randomized 0 < a < n
#' @note Can't test for primes above 15 because of loss of precision
#' in modulus.
#' @TODO Use the \code{gmp} library so we can test for bigger
#' primes.
#' @author Peter Danenberg \email{pcd@@roxygen.org}
fermat.test <- function(n) {
  a <- floor(runif(1, min=1, max=n))
  isTRUE(a ^ n %% n == a)
}

#' Check an integer for pseudo-primality to an arbitrary precision.
#'
#' A number is pseudo-prime if it is probably prime, the basis of
#' which is the probabilistic Fermat test; if it passes two such
#' tests, the chances are better than 3 out of 4 that n is prime.
#'
#' @param n the integer to test for pseudoprimality.
#' @param times the number of Fermat tests to perform
#' @return Whether the number is pseudoprime
#' @seealso \code{\link{fermat.test}}
#' @export
#' @examples
#' ## Will be true most of the time
#' is.pseudoprime(13, 4)
#' @references Abelson, Hal; Jerry Sussman, and Julie Sussman.
#' Structure and Interpretation of Computer Programs. Cambridge:
#' MIT Press, 1984.
#' @keywords pseudoprime fermat
#' @author Peter Danenberg \email{pcd@@roxygen.org}
is.pseudoprime <- function(n, times) {
  if (times == 0) TRUE
  else if (fermat.test(n)) is.fermat.prime(n, times - 1)
  else FALSE
}
