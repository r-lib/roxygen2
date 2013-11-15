\name{parsers}
\alias{parse.default}
\alias{parse.name}
\alias{parse.name.description}
\alias{parse.toggle}
\alias{parse.unknown}
\alias{parse.value}
\alias{parse.words}
\alias{parsers}
\title{Parsers.}
\usage{
parse.default(key, rest, srcref)

parse.unknown(key, rest, srcref)

parse.value(key, rest, srcref)

parse.words(key, rest, srcref)

parse.name.description(key, rest, srcref)

parse.name(key, name, srcref)

parse.toggle(key, rest, srcref)
}
\arguments{
  \item{key}{the parsing key}

  \item{rest}{the expression to be parsed}

  \item{srcref}{srcref providing location of file name and
  line number}
}
\description{
These function implement parsing different tag types.
}
\details{
\code{parse.default}: emit value unchanged.

\code{parse.default}: warns about unknown tag.

\code{parse.value}: fail if empty

\code{parse.words}: parse values into words separated by space

\code{parse.description}: parse mandatory name and description

\code{parse.name}: one and only one word

\code{parse.toggle}: turn binary element on
}
\keyword{internal}

