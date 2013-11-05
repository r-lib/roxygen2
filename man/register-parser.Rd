\name{register.preref.parser}
\alias{register.preref.parser}
\title{Register parsers.}
\usage{
  register.preref.parser(key, parser)
}
\arguments{
  \item{key}{the key upon which to register}

  \item{parser}{the parser callback to register; a function
  taking \code{key} and \code{expression}}
}
\value{
  \code{NULL}
}
\description{
  Register parsers.
}
\keyword{internal}

