\docType{package}
\name{roxygen}
\alias{roxygen}
\alias{roxygen-package}
\title{Literate Programming in R}
\description{
  In-line documentation for R.
}
\details{
  Roxygen is a Doxygen-like documentation system for R;
  allowing in-source specification of Rd files, collation
  and namespace directives.

  If you have existing Rd files, check out the
  \code{Rd2roxygen} package for a convenient way of
  converting Rd files to roxygen comments.

  \tabular{ll}{ Package: \tab Roxygen2\cr Type: \tab
  Package\cr Version: \tab 0.1-1\cr Date: \tab
  2008-08-25\cr License: \tab GPL (>= 2)\cr LazyLoad: \tab
  yes\cr }
}
\examples{
\dontrun{roxygenize('pkg')}
}
\author{
  Peter Danenberg \email{pcd@roxygen.org}, Manuel Eugster
  \email{Manuel.Eugster@stat.uni-muenchen.de}

  Maintainer: Peter Danenberg \email{pcd@roxygen.org}
}
\seealso{
  See \code{\link{namespace_roclet}},
  \code{\link{collate_roclet}}, for an overview of roxygen
  tags.
}
\keyword{package}

