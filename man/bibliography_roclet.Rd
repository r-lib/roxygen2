\name{bibliography_roclet}
\alias{bibliography_roclet}
\title{Roclet to Generate File REFERENCES.bib}
\usage{
  bibliography_roclet()
}
\description{
  This roclet generates a file REFERENCES.bib in the
  package's \dQuote{inst} sub-directory, based on BibTeX
  database files declared with tags \code{@bibliography}
  and citations specified with tags \code{@cite}.
}
\section{Specific tags and command}{
  This roclet processes the following tags:

  \describe{

  \item{\code{@bibliography filename}: }{Declare a BibTeX
  database file where citation keys from \code{@cite} tags
  and \code{\\cite} commands are looked for. Multiple
  database files are declared using multiple
  \code{@bibliography} tags, one for each file. The files
  are searched by order of declaration, and the first entry
  that matches the looked-up key is used to generate a full
  reference string in the Rd files' \emph{References}
  sections.

  \code{@bibliography} tags are typically put in the
  roxygen chunk that describes the package being documented
  (i.e. the chunk that contains the tag \code{@docType
  package}). However they can be put in any other chunk, in
  any given collated R file.

  There is no need to manually create the package's default
  BibTeX file (\dQuote{inst/REFERENCES.bib}) or add a
  \code{@bibliography} tag to declare it, as it is always
  searched if present -- and is created automatically if
  any citation was found in roxygen chunks and an external
  bibliography file is declared. }

  \item{\code{@cite space/comma separated BibTeX citation
  keys}: }{Keys of BibTeX entries that will be substituted
  by full references and inserted in the \emph{References}
  section of the corresponding Rd file.

  \code{@cite} tags should be put in the roxygen chunks
  corresponding to the Rd where they must appear. }

  \item{\code{\\cite{comma separated BibTeX citation
  keys}}: }{This command can be used within the body of any
  relevant roxygen tags, e.g. in @descriptions, @details,
  @sections, etc...

  The commands that contain BibTeX citation keys are
  substituted in the Rd file by short citation strings,
  still wrapped in a \code{\\cite} command that is rendered
  accordingly by R Rd engine (e.g. \cite{Toto et al.
  (2008)}), and a full reference is added in the
  \emph{References} section. Unresolved citation keys are
  left unchanged, also wrapped in a \code{\\cite} command.

  The \code{\\cite} commands are substituted in the
  generated Rd files with the following rules:

  \itemize{

  \item If the citation key \dQuote{Toto2008} exists in one
  of the declared BibTeX files, then the command
  \code{\\cite{Toto2008}} is substituted by
  \code{\\cite{Toto (2008)}} or \code{\\cite{Toto et al.
  (2008)}} if the reference is from a single or multiple
  authors respectively;

  \item Multiple citations are specified with commas (no
  space allowed, see last rule) , e.g.
  \code{\\cite{Toto2008,Tata1989}}, and are substituted as:
  \dQuote{\cite{Toto et al. (2008), Tata (1989)}};

  \item Unresolved citation keys (e.g. Unknown2010) remain
  unchanged: \code{\\cite{Unknown2010}} remains
  \code{\\cite{Unknown2010}} and
  \code{\\cite{Tata1989,Unknown2010}} becomes
  \code{\\cite{Tata (1989), Unknown2010}};

  \item Escaped commands \code{\\\\cite} are not
  substituted;

  \item Commands that contain spaces are not considered as
  being BibTeX citations, and are therefore ignored by
  roxygen. Hence roxygen will not try to find a BibTeX
  entry from the command \code{\\cite{Writing R
  Extensions}}, which will remain unchanged. In the --
  unlikely -- case where one wants to use \code{\\cite}
  with a single word, that also happens to be a BibTeX
  citation key, then a leading or trailing space should be
  added -- or the BibTeX entry key be changed.

  }% end itemize

  }

  }
}
\examples{
### An example file, example.R, which document a package
#'
#' A very nice package.
#' For more info see \\cite{Somebody2012}
#'
#' @name NicePkg
#' @docType package
#' @bibliography /home/username/articles/library.bib
NULL

#' Some function
#'
#' This function implements the method from \\cite{Toto2008}.
#'
#' @cite Tata1980
fun <- function() {}
###

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

