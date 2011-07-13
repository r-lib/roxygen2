\name{parse.unknown}
\alias{parse.unknown}
\title{Parse an unknown tag.}
\usage{
  parse.unknown(key, rest)
}
\arguments{
  \item{key}{the parsing key}

  \item{rest}{the expression to be parsed}
}
\value{
  A list containing the key and expression (possibly null)
}
\description{
  Resorts to the default parser but with a warning about
  the unknown tag.
}
\seealso{
  Other preref parsing functions:
  \code{\link{parse.default}}, \code{\link{parse.name}},
  \code{\link{parse.name.description}},
  \code{\link{parse.toggle}}, \code{\link{parse.value}}
}
\keyword{internal}

