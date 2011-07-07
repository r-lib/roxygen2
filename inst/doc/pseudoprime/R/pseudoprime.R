#' @include fermat.R
roxygen()

#' Check an integer for pseudo-primality to an arbitrary
#' precision.
#'
#' A number is pseudo-prime if it is probably prime, the basis
#' of which is the probabilistic Fermat test; if it passes two
#' such tests, the chances are better than 3 out of 4 that
#' \eqn{n} is prime.
#'
#' @param n the integer to test for pseudoprimality.
#' @param times the number of Fermat tests to perform
#' @return Whether the number is pseudoprime
#' @export
#' @seealso \code{\link{fermat.test}}
#' @references Abelson, Hal; Jerry Sussman, and Julie Sussman.
#'   Structure and Interpretation of Computer Programs.
#'   Cambridge: MIT Press, 1984.
#' @author Peter Danenberg \email{pcd@@roxygen.org}
#' @examples
#' is.pseudoprime(13, 4)  # TRUE most of the time
is.pseudoprime <- function(n, times) {
  if (times == 0) TRUE
  else if (fermat.test(n)) is.pseudoprime(n, times - 1)
  else FALSE
}
