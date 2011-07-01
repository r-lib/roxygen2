\name{collate_roclet}
\alias{collate_roclet}
\title{Collate roclet topological sorts R files and records in Collate field of...}
\usage{collate_roclet(package.dir, roxygen.dir, merge.file = NULL,
    target.file = NULL, verbose = TRUE)}

\description{
  Collate roclet topological sorts R files and records in
  Collate field of \file{DESCRIPTION}.
}

\details{
  Each \code{@include} tag should specify the filename of
  one intrapackage dependency; multiple \code{@include}
  tags may be given.

  Contains the member function \code{parse} which parses an
  arbitrary number of files, and \code{parse.dir} which
  recursively parses a directory tree.
}
\value{Rd roclet}
\seealso{\code{\link{make.roclet}}}
\arguments{
  \item{package.dir}{the package's top directory}
  \item{roxygen.dir}{where to create roxygen output; defaults to
\file{package.roxygen}.}
  \item{merge.file}{\file{DESCRIPTION} file with which to merge directive;
or \code{NULL} for none}
  \item{target.file}{whither to \code{cat} directive (whether merged or
not); blank line is standard out}
  \item{verbose}{whether to describe what we're doing with the
target.file}
}
\examples{#' `example-a.R', `example-b.R' and `example-c.R' reside
#' in the `example' directory, with dependencies
#' a -> {b, c}. This is `example-a.R'.
#' @include example-b.R
#' @include example-c.R
NULL

roclet <- collate_roclet()
\dontrun{roclet$parse.dir('example')}}
