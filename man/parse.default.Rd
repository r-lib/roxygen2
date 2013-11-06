\name{parse.default}
\alias{parse.default}
\title{Default parser which simply emits the key and expression.}
\usage{
  parse.default(key, rest, srcref)
}
\arguments{
  \item{key}{the parsing key}

  \item{rest}{the expression to be parsed}

  \item{srcref}{srcref providing location of file name and
  line number}
}
\value{
  A list containing the key and expression (possibly null)
}
\description{
  Used for elements with optional values (like
  \code{@export}) where roclets can do more sophisticated
  things with \code{NULL}.
}
\seealso{
  Other preref parsing functions: \code{\link{parse.name}},
  \code{\link{parse.name.description}},
  \code{\link{parse.toggle}}, \code{\link{parse.unknown}},
  \code{\link{parse.value}}, \code{\link{parse.words}}
}
\keyword{internal}

