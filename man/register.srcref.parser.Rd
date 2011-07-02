\name{register.srcref.parser}
\alias{register.srcref.parser}
\title{Specifically register a srcref parser...}
\usage{
  register.srcref.parser(key, parser)
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
  Specifically register a srcref parser
}
\keyword{internal}

