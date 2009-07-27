\name{roxygenize}
\alias{roxygenize}
\title{Process a package with the Rd, namespace and collate roclets.}
\usage{roxygenize(package.dir, roxygen.dir, copy.package=TRUE, overwrite=TRUE,
    unlink.target=FALSE, use.Rd2=FALSE)}
\description{Process a package with the Rd, namespace and collate roclets.}
\value{\code{NULL}}
\section{TODO}{Options to enable/disable specific roclet
(\command{--no-callgraphs}, etc.)}
\arguments{\item{package.dir}{the package's top directory}
\item{roxygen.dir}{whither to copy roxygen files; defaults to
\file{package.roxygen}.}
\item{copy.package}{copies the package over before
adding/manipulating files.}
\item{overwrite}{overwrite target files}
\item{unlink.target}{unlink target directory before
processing files}
\item{use.Rd2}{use the Rd2 roclet}}
