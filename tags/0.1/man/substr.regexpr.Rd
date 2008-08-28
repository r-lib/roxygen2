\name{substr.regexpr}
\alias{substr.regexpr}
\title{Actually do the substring representation that...}
\usage{substr.regexpr(pattern, text)}
\description{Actually do the substring representation that
regexpr should do; does not acknowledge groups,
since \code{regexpr} doesn't.}
\value{The matched substring}
\arguments{\item{pattern}{the pattern to match}
\item{text}{the text to match against}}
