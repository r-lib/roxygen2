\name{strmap}
\alias{strmap}
\title{Map through the words in a string, joining the mapped...}
\usage{strmap(proc, sep, string)}
\description{Map through the words in a string, joining the mapped
words with a separator.}
\details{General enough to be designated `map': isn't it closer to a
specialized reduce?}
\value{Mapped words separated by \code{sep}}
\arguments{\item{proc}{procedure to apply to each word}
\item{sep}{the separator joining the mapped words}
\item{string}{the string to be mapped}}
