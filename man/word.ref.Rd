\name{word.ref}
\alias{word.ref}
\title{Find the nth word in a string.}
\usage{word.ref(string, n)}
\description{Find the nth word in a string.}
\value{A list containing:
\item{start}{the first letter of the word.}
\item{end}{the last letter of the word.}
Undefined if no such word; though \code{end} may be less than
\code{start} in such a case.}
\arguments{\item{string}{the string to search in}
\item{n}{the nth word to find}}
