\name{parse.toggle}
\alias{parse.toggle}
\title{Turn a binary element on; parameters are ignored.}
\usage{
  parse.toggle(key, rest, srcref)
}
\arguments{
  \item{key}{the parsing key}

  \item{rest}{the expression to be parsed}

  \item{srcref}{srcref providing location of file name and
  line number}
}
\value{
  A list with the key and \code{TRUE}
}
\description{
  Turn a binary element on; parameters are ignored.
}
\seealso{
  Other preref parsing functions:
  \code{\link{parse.default}}, \code{\link{parse.name}},
  \code{\link{parse.name.description}},
  \code{\link{parse.unknown}}, \code{\link{parse.value}}
}
\keyword{internal}

