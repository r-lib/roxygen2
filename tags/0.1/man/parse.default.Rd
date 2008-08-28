\name{parse.default}
\alias{parse.default}
\title{Default parser which simply emits the key and expression;...}
\usage{parse.default(key, rest)}
\description{Default parser which simply emits the key and expression;
used for elements with optional values (like \code{@export})
where roclets can do more sophisticated things with \code{NULL}.}
\value{A list containing the key and expression (possibly
null)}
\arguments{\item{key}{the parsing key}
\item{rest}{the expression to be parsed}}
