% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roxygenize.R
\name{roxygenize}
\alias{roxygenize}
\alias{roxygenise}
\title{Process a package with the Rd, namespace and collate roclets}
\usage{
roxygenize(package.dir = ".", roclets = NULL, load_code = NULL, clean = FALSE)

roxygenise(package.dir = ".", roclets = NULL, load_code = NULL, clean = FALSE)
}
\arguments{
\item{package.dir}{Location of package top level directory. Default is
working directory.}

\item{roclets}{Character vector of roclet names to use with package.
The default, \code{NULL}, uses the roxygen \code{roclets} option,
which defaults to \code{c("collate", "namespace", "rd")}.}

\item{load_code}{A function used to load all the R code in the package
directory. The default, \code{NULL}, uses the strategy defined by
the \code{load} roxygen option, which defaults to \code{\link[=load_pkgload]{load_pkgload()}}.
See \link{load} for more details.}

\item{clean}{If \code{TRUE}, roxygen will delete all files previously
created by roxygen before running each roclet.}
}
\value{
\code{NULL}
}
\description{
This is the workhorse function that uses roclets, the built-in document
transformation functions, to build all documentation for a package. See
the documentation for the individual roclets, \code{\link[=rd_roclet]{rd_roclet()}},
\code{\link[=namespace_roclet]{namespace_roclet()}}, and for \code{\link[=update_collate]{update_collate()}},
for more details.
}
\details{
Note that roxygen2 is a dynamic documentation system: it works by
inspecting loaded objects in the package. This means that you must
be able to load the package in order to document it: see \link{load} for
details.
}
