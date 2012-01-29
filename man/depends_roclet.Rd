\name{depends_roclet}
\alias{depends_roclet}
\title{Roclet: make Depend field in DESCRIPTION.}
\usage{
  depends_roclet()
}
\value{
  Rd roclet
}
\description{
  Topologically sort R files and record in Depend field.
}
\details{
  Each \code{@include} tag should specify the filename of
  one intrapackage dependency; multiple \code{@include}
  tags may be given.
}
\examples{
#' `example-a.R', `example-b.R' and `example-c.R' reside
#' in the `example' directory, with dependencies
#' a -> {b, c}. This is `example-a.R'.
#' @include example-b.R
#' @include example-c.R
NULL

roclet <- depends_roclet()
\dontrun{
  roc_proc(roclet, dir('example'))
  roc_out(roclet, dir('example'), "example")
}
}
\seealso{
  Other roclets: \code{\link{collate_roclet}},
  \code{\link{index_roclet}},
  \code{\link{namespace_roclet}}, \code{\link{rd_roclet}}
}

