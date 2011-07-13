\name{parse.name}
\alias{parse.name}
\title{Parse an element containing a single name and only a name.}
\usage{
  parse.name(key, name)
}
\arguments{
  \item{name}{the name to be parsed}

  \item{key}{the parsing key}
}
\value{
  A list containing key and name
}
\description{
  Extra material will be ignored and a warning issued.
}
\seealso{
  Other preref parsing functions:
  \code{\link{parse.default}},
  \code{\link{parse.name.description}},
  \code{\link{parse.toggle}}, \code{\link{parse.unknown}},
  \code{\link{parse.value}}
}
\keyword{internal}

