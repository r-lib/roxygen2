\name{roxygenize}
\alias{roxygenise}
\alias{roxygenize}
\title{Process a package with the Rd, namespace and collate roclets.}
\usage{
  roxygenize(package.dir, roxygen.dir = package.dir,
    copy.package = package.dir != roxygen.dir,
    overwrite = TRUE, unlink.target = FALSE,
    roclets = c("collate", "namespace", "rd"))

  roxygenise(package.dir, roxygen.dir = package.dir,
    copy.package = package.dir != roxygen.dir,
    overwrite = TRUE, unlink.target = FALSE,
    roclets = c("collate", "namespace", "rd"))
}
\arguments{
  \item{package.dir}{the package's top directory}

  \item{roxygen.dir}{where to create roxygen output;
  defaults to \file{package.dir}.}

  \item{copy.package}{copies the package over before
  adding/manipulating files.}

  \item{overwrite}{overwrite target files?}

  \item{unlink.target}{unlink target directory before
  processing files?}

  \item{roclets}{character vector of roclet names to apply
  to package}
}
\value{
  \code{NULL}
}
\description{
  This is the workhorse function that uses roclets, the
  built-in document tranformation functions, to build all
  documentation for a package.  See the documentation for
  the individual roclets, \code{\link{rd_roclet}},
  \code{\link{namespace_roclet}} and
  \code{\link{collate_roclet}}, for documentation on how to
  use each one.
}

