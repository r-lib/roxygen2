\name{parse.words}
\alias{parse.words}
\title{Parse an element with a least one word}
\usage{
  parse.words(key, rest, srcref)
}
\arguments{
  \item{key}{the parsing key}

  \item{rest}{the expression to be parsed}

  \item{srcref}{srcref providing location of file name and
  line number}
}
\value{
  A list containing the key and value
}
\description{
  Parse an element with a least one word
}
\seealso{
  Other preref parsing functions:
  \code{\link{parse.default}}, \code{\link{parse.name}},
  \code{\link{parse.name.description}},
  \code{\link{parse.toggle}}, \code{\link{parse.unknown}},
  \code{\link{parse.value}}
}
\keyword{internal}

