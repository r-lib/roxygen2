\name{make.Rd2.roclet}
\title{New implementation of the Rd roclet; same functionality as the original...}
\usage{make.Rd2.roclet(subdir, verbose=TRUE, exportonly=FALSE,
    documentedonly=TRUE)}
\description{New implementation of the Rd roclet; same functionality as the original
implementation plus basic S4 handling.}
\details{See \code{\link{make.Rd.roclet}} for description and available tags; new
tags are:

\enumerate{
\item{\code{@nord}}{Suppress Rd creation.}
\item{\code{@rdname}}{Definition of the Rd name; blocks with the same
\code{@rdname} are merged into one Rd file.}
\item{\code{@slot}}{Each S4 class slot should have a \code{@slot <name>
<description>} specified.}
}}
\value{Rd roclet}
\alias{nord}
\alias{rdname}
\alias{slot}
\alias{make.Rd2.roclet}
\arguments{\item{subdir}{directory into which to place the Rd files; if
\code{NULL}, standard out.}
\item{verbose}{whether to declare what we're doing in the
\var{subdir}}
\item{exportonly}{create Rd files only for exported "things"}
\item{documentedonly}{create Rd files only for "things" which
are documented with Roxygen}}
