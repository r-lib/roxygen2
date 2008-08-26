#' Tests pseudoprimality by Fermat's Little Theorem.
#'
#' \tabular{ll}{
#' Package: \tab pseudoprime\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2008-08-24\cr
#' License: \tab What license is it under?\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Using the Fermat primality test, pseudoprime checks for primes
#' probabilistically; the test is fooled every time by Carmichael
#' numbers.
#'
#' \code{\link{is.pseudoprime}} checks a number \code{n} for
#' pseudoprimality, applying Fermat's test \code{times} times.
#' 
#' @name pseudoprime-package
#' @aliases pseudoprime
#' @docType package
#' @title Tests pseudoprimality by Fermat's Little Theorem
#' @author Peter Danenberg \email{pcd@@roxygen.org}
#' @references
#' \url{http://en.wikipedia.org/wiki/Fermat's_little_theorem}
#'
#' \url{http://en.wikipedia.org/wiki/Fermat_primality_test}
#'
#' \url{http://en.wikipedia.org/wiki/Carmichael_number}
#' @keywords package
#' @seealso \code{\link{is.pseudoprime}}
#' @examples
#' is.pseudoprime(13, 4)
roxygen()
