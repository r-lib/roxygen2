\name{roxygen-package}
\alias{roxygen-package}
\title{Roxygen: Literate Programming in R}
\description{Roxygen is a Doxygen-like documentation system for R; allowing
in-source specification of Rd files, collation and namespace
directives.}
\details{\tabular{ll}{
Package: \tab Roxygen\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2008-08-25\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

Roxygen is run on a package (hereafter <package>) by
\command{R CMD roxygen <package>} or \command{Rcmd roxygen.sh <package>}
on Windows. By default, it creates a directory \file{<package>.roxygen}
with the complete package cum populated Rd files, \file{NAMESPACE}, etc.;
but can also operate descructively on the package itself with the
\option{-d} option.

See the vignette (\file{roxygen.pdf}) or manual (\file{roxygen-manual.pdf})
for details.}
\author{Peter Danenberg \email{pcd@roxygen.org},
Manuel Eugster \email{Manuel.Eugster@stat.uni-muenchen.de}

Maintainer: Peter Danenberg \email{pcd@roxygen.org}}
\docType{package}
\keyword{package}
\seealso{See \code{\link{make.Rd.roclet}},
\code{\link{make.namespace.roclet}},
\code{\link{make.collate.roclet}},
\code{\link{make.callgraph.roclet}}
for an overview of roxygen tags.

See \code{\link{roxygenize}} for an alternative to `R CMD roxygen'.}
\examples{## To process a package in `pkg', run `R CMD roxygen pkg'; or:
\dontrun{roxygenize('pkg')}}
