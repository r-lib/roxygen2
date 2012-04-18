\name{bibliography_roclet}
\alias{bibliography_roclet}
\title{Roclet to Generate File REFERENCES.bib}
\usage{
  bibliography_roclet()
}
\description{
  This roclet generates a file REFERENCES.bib in the
  package's \dQuote{inst} sub-directory, based on BibTeX
  database files declared with tag \code{@bibliography}
  citations specified with tag \code{@cite}.
}
\section{Specific tags and command}{
  This roclet processes the following tags:

  \describe{

  \item{\code{@bibliography filename}}{Declare a BibTeX
  database file where citation keys are looked for.
  Multiple database files are declared using multiple
  \code{@bibliography} tags, one for each file. The files
  are searched by order of declaration, and the first entry
  that matches the looked-up key is used to generate a full
  reference string.

  \code{@bibliography} tags are typically put in the
  roxygen chunk that describes the package (i.e.. that
  contains the tag \code{@docType package}).

  There is no need to add a tag \code{@bibliography
  inst/REFERENCES.bib}, as this BibTeX file is searched by
  default if already present. }

  \item{\code{@cite space/comma separated BibTeX citation
  keys}}{Keys of BibTeX entries that will be substituted by
  full references and inserted in the \emph{References}
  section of the corresponding Rd file.

  \code{@cite} tags should be put in the roxygen chunks
  corresponding to the Rd where they must appear. }

  \item{\code{\\rcite{single BibTeX citation key}}}{This
  command can be used within the body of roxygen tags, e.g.
  in descriptions, details, sections, etc...

  The command is substituted in the Rd file by a quick
  reference, wrapped in a \code{\\cite}, while a full
  reference is added in the \emph{References} section.
  Unresolved keys are left unchanged, also wrapped in a
  \code{\\cite} command.

  For example:

  - \\rcite{Toto2008} would sds generate something like
  \dQuote{\cite{Toto et al. (2008)}}

  - If unresolved, \\rcite{Toto2010} would generate
  \dQuote{\cite{Toto2010}}

  }

  }
}
\examples{
#' An example file, example.R, which document a package
#'
#' A very nice package.
#' For more info see \\rcite{Somebody2012}
#'
#' @name NicePkg
#' @docType package
#' @bibliography /home/username/articles/library.bib
NULL

#' Some function
#'
#' This function implements the method from \\rcite{Toto2008}.
#'
#' @cite Tata1980
fun <- function() {}

roclet <- bibliography_roclet()
\dontrun{roc_proc(roclet, "example.R")}
\dontrun{roc_out(roclet, "example.R", ".")}
}
\author{
  Renaud Gaujoux
}
\seealso{
  Other roclets: \code{\link{collate_roclet}},
  \code{\link{namespace_roclet}}, \code{\link{rd_roclet}}
}

