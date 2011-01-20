\name{is.pseudoprime}
\alias{is.pseudoprime}
\title{Check an integer for pseudo-primality to an arbitrary...}
\usage{is.pseudoprime(n, times)}

\description{
  Check an integer for pseudo-primality to an arbitrary precision.
}

\details{
  A number is pseudo-prime if it is probably prime, the basis of which
  is the probabilistic Fermat test; if it passes two such tests, the
  chances are better than 3 out of 4 that \eqn{n} is prime.
}
\value{Whether the number is pseudoprime}
\seealso{\code{\link{fermat.test}}}
\references{Abelson, Hal; Jerry Sussman, and Julie Sussman.
Structure and Interpretation of Computer Programs.
Cambridge: MIT Press, 1984.}
\author{Peter Danenberg \email{pcd@roxygen.org}}
\arguments{
  \item{n}{the integer to test for pseudoprimality.}
  \item{times}{the number of Fermat tests to perform}
}
\examples{is.pseudoprime(13, 4)  # TRUE most of the time}
