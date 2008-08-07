\name{make.collate.roclet}
\alias{make.collate.roclet}
\title{Make collate roclet which parses the given files; topologically...}
\usage{make.collate.roclet(merge.file, target.file, verbose=TRUE)}
\description{Make collate roclet which parses the given files; topologically
sorting \code{@include}s, and either merging the \code{Collate:}
directive with a pre-existing \file{DESCRIPTION} or writing to
standard out.}
\details{Each \code{@include} tag should specify the filename of one intrapackage
dependency; multiple \code{@include} tags may be given.

Contains the member function \code{parse} which parses an arbitrary number
of files, and \code{parse.dir} which recursively parses a directory tree.}
\value{Rd roclet}
\seealso{\code{\link{make.roclet}}}
\arguments{\item{merge.file}{\file{DESCRIPTION} file with which to merge directive;
or \code{NULL} for none}
\item{target.file}{whither to \code{cat} directive (whether merged or
not); blank line is standard out}
\item{verbose}{whether to describe what we're doing with the
target.file}}
\examples{#' `example-a.R', `example-b.R' and `example-c.R' reside
#' in the `example' directory, with dependencies
#' a -> {b, c}. This is `example-a.R'.
#' @include example-b.R
#' @include example-c.R
roxygen()

roclet <- make.collate.roclet()
\dontrun{roclet$parse.dir('example')}}
