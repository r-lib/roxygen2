\name{make.namespace.roclet}
\title{Make a namespace roclet which parses the given files and writes a list of...}
\usage{make.namespace.roclet(outfile, verbose=TRUE)}
\description{Make a namespace roclet which parses the given files and writes a list of
namespace directives to a given file or standard out; see
\cite{Writing R Extensions}
(\url{http://cran.r-project.org/doc/manuals/R-exts.pdf}) for details.}
\details{The namespace roclet supports the following tags:

\tabular{ll}{
Roxygen tag \tab \file{NAMESPACE} equivalent\cr
\code{@export} \tab \code{export}\cr
\code{@exportClass} \tab \code{exportClasses}\cr
\code{@exportMethod} \tab \code{exportMethod}\cr
\code{@exportPattern} \tab \code{exportPattern}\cr
\code{@S3method} \tab \code{S3method}\cr
\code{@import} \tab \code{import}\cr
\code{@importFrom} \tab \code{importFrom}\cr
\code{@importClassesFrom} \tab \code{importClassesFrom}\cr
\code{@importMethodsFrom} \tab \code{importMethodsFrom}\cr
}

\enumerate{
\item{\code{@export}}{May be specified with or without value;
if unadorned, roxygen will try to guess
the exported value by assignee, \code{setMethod},
\code{setClass}, etc. Otherwise,
\code{@export f g ...}
translates to
\code{export(f, g, ...)}.}
\item{\code{@exportClass}}{Overrides \code{setClass}.}
\item{\code{@exportMethod}}{Overrides \code{setMethod} or \code{setGeneric}.}
\item{\code{@exportPattern}}{See \dQuote{1.6.2 Registering S3 methods} from
\cite{Writing R Extensions}.}
\item{\code{@S3method}}{Overrides the export of an S3 method.}
\item{\code{@import}}{See \dQuote{1.6.1 Specifying imports and exports}
from \cite{Writing R Extensions}.}
\item{\code{@importFrom}}{See \dQuote{1.6.1 Specifying imports and exports}
from \cite{Writing R Extensions}.}
\item{\code{@importClassesFrom}}{See \dQuote{1.6.6 Name spaces with formal
classes and methods} from \cite{Writing R
Extensions}.}
\item{\code{@importMethodsFrom}}{See \dQuote{1.6.6 Name spaces with formal
classes and methods} from \cite{Writing R
Extensions}.}
}}
\value{Namespace roclet}
\alias{make.namespace.roclet}
\alias{exportClass}
\alias{exportMethod}
\alias{exportPattern}
\alias{S3method}
\alias{import}
\alias{importFrom}
\alias{importClassesFrom}
\alias{importMethodsFrom}
\alias{export}
\arguments{\item{outfile}{whither to send output; blank string means standard out}
\item{verbose}{whether to anounce what we're doing with
the \var{outfile}}}
\examples{#' An example file, example.R, which imports
#' packages foo and bar
#' @import foo bar
roxygen()

#' An exportable function
#' @export
fun <- function() {}

roclet <- make.namespace.roclet()
\dontrun{roclet$parse('example.R')}}
