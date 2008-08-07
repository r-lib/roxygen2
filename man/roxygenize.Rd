\name{roxygenize}
\alias{roxygenize}
\title{Process a package with the Rd, namespace and collate roclets.}
\usage{roxygenize(package.dir, copy.package=TRUE)}
\description{Process a package with the Rd, namespace and collate roclets.}
\value{\code{NULL}}
\section{TODO}{Options to enable/disable specific roclet
(\command{--no-callgraphs}, etc.)}
\arguments{\item{package.dir}{the package's top directory}
\item{copy.package}{if R.utils is present, copies the package
over before adding/manipulating files.}}
