\name{make.description.parser}
\alias{make.description.parser}
\title{Make a parser to parse DESCRIPTION files.}
\usage{make.description.parser(parse.default=cat.description,
    pre.parse=noop.description, post.parse=noop.description)}
\description{Make a parser to parse \file{DESCRIPTION} files.}
\details{Contains the member functions \code{register.parser},
taking a field and parser; and \code{parse}, taking the
parsed fields from \code{\link{parse.description.file}}
or similar.}
\value{\code{NULL}}
\arguments{\item{parse.default}{the default parser receiving
a field and value}
\item{pre.parse}{a function receiving the parsed fields
before individual parsing}
\item{post.parse}{a function receiving the parsed fields
after individual parsing}}
