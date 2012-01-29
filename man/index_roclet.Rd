\name{index_roclet}
\alias{export}
\alias{exportClass}
\alias{exportMethod}
\alias{import}
\alias{importClassesFrom}
\alias{importFrom}
\alias{importMethodsFrom}
\alias{index_roclet}
\alias{S3method}
\title{Roclet: make INDEX.}
\usage{
  index_roclet()
}
\description{
  This roclet automates the production of an \file{INDEX}
  file, see \cite{Writing R Extensions}
  (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf})
  for details.
}
\section{Tags}{
  There are four tags for exporting objects from the
  package:

  \describe{

  \item{\code{@title}}{Roxygen guesses the directive:
  \code{export} for functions, \code{exportMethod} for S4
  methods, \code{S3method} for S3 methods,
  \code{exportClass} for S4 classes.

  This is the only directive you should need for documented
  function, the other directives are useful if you want to
  export (e.g.) methods but not document them.}

  \item{\code{@export f g ...}}{overrides auto-detection
  and produces multiple export directives:
  \code{export(f)}, \code{export(g)} ...}

  }

  Only unique directives are saved to the \file{INDEX}
  file, so you can repeat them as needed to maintain a
  close link between the functions where they are needed
  and the index file..
}
\examples{
#' An example file, example.R, which imports
#' packages foo and bar
#' @import foo bar
NULL

#' An exportable function
#' @export
fun <- function() {}

roclet <- index_roclet()
\dontrun{roc_proc(roclet, "example.R")}
\dontrun{roc_out(roclet, "example.R", ".")}
}
\seealso{
  Other roclets: \code{\link{collate_roclet}},
  \code{\link{depends_roclet}},
  \code{\link{namespace_roclet}}, \code{\link{rd_roclet}}
}

