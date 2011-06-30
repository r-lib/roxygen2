\name{make.roclet}
\alias{make.roclet}
\title{Abstract roclet that serves as a rudimentary API.}
\usage{make.roclet(package.dir, roxygen.dir, parse.default = NULL,
    pre.parse = NULL, post.parse = NULL, pre.files = NULL,
    post.files = NULL)}

\description{
  Abstract roclet that serves as a rudimentary API.
}

\details{
  Contains the following member functions: \itemize{ \item
  \code{register.parser}: takes \code{key} and
  \code{parser} \item \code{register.parsers}: takes
  \code{parser} and \code{keys} \item
  \code{register.default.parser}: takes a \code{key} \item
  \code{parse} parses material contained in files }
}
\arguments{
  \item{parse.default}{the default parser taking \code{key} and \code{value}}
  \item{pre.parse}{a callback function taking a list of parsed elements;
called before processing a file}
  \item{post.parse}{a callback function taking a list of parsed elements;
called after processing a file}
  \item{pre.files}{a callback function with no arguments; called before any
file has been parsed}
  \item{post.files}{a callback function with no arguments; called after every
file has been parsed}
}
