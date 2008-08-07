\name{make.Rd.roclet}
\alias{make.Rd.roclet}
\title{Make an Rd roclet which parses the given files and, if specified, populates...}
\usage{make.Rd.roclet(subdir, verbose=TRUE)}
\description{Make an Rd roclet which parses the given files and, if specified, populates
the given subdirectory with Rd files; or writes to standard out.  See
\cite{Writing R Extensions}
(\url{http://cran.r-project.org/doc/manuals/R-exts.pdf}) for details.}
\details{The first paragraph of a roxygen block constitutes its description, the
subsequent paragraphs its details; moreover, the Rd roclet supports these
tags:

\tabular{ll}{
Roxygen tag \tab Rd analogue\cr
\code{@author} \tab \code{\\author}\cr
\code{@aliases} \tab \code{\\alias, ...}\cr
\code{@concept} \tab \code{\\concept}\cr
\code{@example} \tab \emph{n/a}\cr
\code{@examples} \tab \code{\\examples}\cr
\code{@keywords} \tab \code{\\keyword, ...}\cr
\code{@method} \tab \code{\\method}\cr
\code{@name} \tab \code{\\name}\cr
\code{@note} \tab \code{\\note}\cr
\code{@param} \tab \code{\\arguments{\\item, ...}}\cr
\code{@references} \tab \code{\\references}\cr
\code{@return} \tab \code{\\value}\cr
\code{@seealso} \tab \code{\\seealso}\cr
\code{@title} \tab \code{\\title}\cr
\code{@TODO} \tab \emph{n/a}\cr
\code{@usage} \tab \code{\\usage}\cr
}

\enumerate{
\item{\code{@author}}{See \dQuote{2.1.1 Documenting functions} from
                       \cite{Writing R Extensions}.}
\item{\code{@aliases}}{A default alias is plucked from the \code{@name} or
                        assignee; otherwise, \code{@alias a b ...} translates
                        to \code{\alias{a}}, \code{\alias{b}}, &c.

                        If you specify one alias, however, specify them all.}
\item{\code{@concept}}{See \dQuote{2.8 Indices} from
                        \cite{Writing R Extensions}.}
\item{\code{@example}}{Each \code{@example} tag specifies an example file
                        relative to the package head; if the file resides in
                        \file{tests}, for instance, it will be checked with
                        \command{R CMD check}.

                        The contents of the file will
                        be concatenated under \code{\examples{...}}.}
\item{\code{@examples}}{Verbatim examples; see \dQuote{2.1.1
                         Documenting functions} from \cite{Writing R
                         Extensions}.}
\item{\code{@keywords}}{\code{@keywords a b ...} translates to
                         \code{\keyword{a}}, \code{\keyword{b}}, &c.}
\item{\code{@method}}{Use \code{@method <generic> <class>} to document
                       S3 functions.}
\item{\code{@name}}{In the absense of an explicit \code{@name} tag, the
                     name of an assignment is plucked from the assignee.}
\item{\code{@note}}{See \dQuote{2.1.1 Documenting functions} from
                     \cite{Writing R Extensions}.}
\item{\code{@param}}{Each function variable should have a
                      \code{@param <variable> <description>} specified.}
\item{\code{@references}}{See \dQuote{2.1.1 Documenting functions} from
                           \cite{Writing R Extensions}.}
\item{\code{@return}}{The return value of the function, or \code{NULL}.}
\item{\code{@seealso}}{See \dQuote{2.1.1 Documenting functions} from
                        \cite{Writing R Extensions}.}
\item{\code{@title}}{A default title is plucked from the first sentence
                      of the description; that is, the first phrase ending
                      with a period, question mark or newline.

                      In the absence of a description, the title becomes
                      the \code{@name} or assignee; lastly, it can be
                      overridden with \code{@title}.}
\item{\code{@TODO}}{Note to developers to get off their asses.}
\item{\code{@usage}}{A default usage is construed from a function's formals,
                      but can be overridden with \code{@usage} (e.g. in the case
                      of multiple functions in one Rd unit).}
}}
\value{Rd roclet}
\arguments{\item{subdir}{directory into which to place the Rd files; if
\code{NULL}, standard out.}
\item{verbose}{whether to declare what we're doing in the
\var{subdir}}}
\examples{#' This sentence describes the function.
#'
#' Here are the details (notice the preceding blank
#' line); the name, title, usage and alias will be
#' automatically generated.
#'
#' @param a a parameter
#' @return NULL
f <- function(a=1) NULL

#' S3 functions require a @method tag for
#' the time being.
#'
#' @method specialize foo
#' @param f a generic foo
#' @param ... ignored
#' @return The specialized foo
specialize.foo <- function(f, ...)
  actually.specialize(f)

roclet <- make.Rd.roclet('man')
\dontrun{roclet$parse('example.R')}}
