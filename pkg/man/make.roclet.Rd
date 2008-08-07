\name{make.roclet}
\alias{make.roclet}
\title{Abstract roclet that serves as a rudimentary API.}
\usage{make.roclet(parse.default, pre.parse, post.parse, pre.files,
    post.files)}
\description{Abstract roclet that serves as a rudimentary API.}
\details{Contains the following member functions:
\itemize{\item{register.parser}{takes \code{key} and \code{parser}}
\item{register.parsers}{takes \code{parser} and \code{keys}}
\item{register.default.parser}{takes a \code{key}}
\item{register.default.parsers}{take \code{parsers}}
\item{parse}{parses material contained in files}}}
\arguments{\item{parse.default}{the default parser taking \code{key}
and \code{value}}
\item{pre.parse}{a callback function taking a list of parsed
elements; called before processing a file}
\item{post.parse}{a callback function taking a list of parsed
elements; called after processing a file}
\item{pre.files}{a callback function with no arguments;
called before any file has been parsed}
\item{post.files}{a callback function with no arguments;
called after every file has been parsed}}
