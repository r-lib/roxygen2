\name{object}
\alias{object}
\title{Constructors for S3 object to represent R object.}
\usage{
object(subclass, name, value, ..., alias = NULL)
}
\arguments{
  \item{subclass}{This is an abstract class so this must be
  provided.  Currently this is one of "function",
  "s4generic", "s4class", "s4method", "rcclass", or
  "data".}

  \item{name}{Name of the object being documented}

  \item{value}{The object itself.}

  \item{...}{optional additional fields used by subclasses}

  \item{alias}{Used for \code{\link{setClass}} and
  \code{\link{setRefClass}} to capture the name of the
  created object.}
}
\description{
These objects are usually created by the parsers, but it is also
useful to generate them by hand for testing.
}
\keyword{internal}

