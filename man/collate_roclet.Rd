\name{collate_roclet}
\alias{collate_roclet}
\title{Collate roclet topological sorts R files and records in Collate field of...}
\arguments{
  \item{package.dir}{the package's top directory}
}
\description{
  Collate roclet topological sorts R files and records in
  Collate field of \file{DESCRIPTION}.
}
\details{Each \code{@include} tag should specify the filename of
  one intrapackage dependency; multiple \code{@include}
  tags may be given.

  Contains the member function \code{parse} which parses an
  arbitrary number of files, and \code{parse.dir} which
  recursively parses a directory tree.}
\seealso{\code{\link{make.roclet}}}
\value{Rd roclet}
\examples{#' `example-a.R', `example-b.R' and `example-c.R' reside
#' in the `example' directory, with dependencies
#' a -> {b, c}. This is `example-a.R'.
#' @include example-b.R
#' @include example-c.R
NULL

roclet <- collate_roclet()
\dontrun{roclet$parse.dir('example')}}

