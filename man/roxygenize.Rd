\name{roxygenize}
\alias{roxygenise}
\alias{roxygenize}
\title{Process a package with the Rd, namespace and collate roclets.}
\usage{
roxygenize(package.dir = ".", roxygen.dir = package.dir,
  copy.package = package.dir != roxygen.dir, overwrite = TRUE,
  unlink.target = FALSE, roclets = c("collate", "namespace", "rd"),
  load_code = source_package)

roxygenise(package.dir = ".", roxygen.dir = package.dir,
  copy.package = package.dir != roxygen.dir, overwrite = TRUE,
  unlink.target = FALSE, roclets = c("collate", "namespace", "rd"),
  load_code = source_package)
}
\arguments{
  \item{package.dir}{the package's top directory}

  \item{roxygen.dir,copy.package,overwrite,unlink.target}{deprecated}

  \item{roclets}{character vector of roclet names to apply
  to package}

  \item{load_code}{A function used to load all the R code
  in the package directory. It is called with the path to
  the package, and it should return an environment
  containing all the sourced code.}
}
\value{
  \code{NULL}
}
\description{
  This is the workhorse function that uses roclets, the
  built-in document tranformation functions, to build all
  documentation for a package.  See the documentation for
  the individual roclets, \code{\link{rd_roclet}},
  \code{\link{namespace_roclet}}, and for
  \code{\link{update_collate}}, for more details.
}

