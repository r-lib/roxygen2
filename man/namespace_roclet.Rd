\name{namespace_roclet}
\alias{export}
\alias{exportClass}
\alias{exportMethod}
\alias{import}
\alias{importClassesFrom}
\alias{importFrom}
\alias{importMethodsFrom}
\alias{namespace_roclet}
\alias{S3method}
\title{Roclet: make NAMESPACE.}
\usage{
  namespace_roclet()
}
\description{
  This roclet automates the production of a
  \file{NAMESPACE} file, see \cite{Writing R Extensions}
  (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf})
  for details.
}
\section{Tags}{
  There are four tags for exporting objects from the
  package:

  \describe{

  \item{\code{@export}}{Roxygen guesses the directive:
  \code{export} for functions, \code{exportMethod} for S4
  methods, \code{S3method} for S3 methods,
  \code{exportClass} for S4 classes.

  This is the only directive you should need for documented
  function, the other directives are useful if you want to
  export (e.g.) methods but not document them.}

  \item{\code{@export f g ...}}{overrides auto-detection
  and produces multiple export directives:
  \code{export(f)}, \code{export(g)} ...}

  \item{\code{@exportClass x}}{produces
  \code{exportClasses(x)} directive.}

  \item{\code{@exportMethod x}}{produces
  \code{exportMethods(x)} directive.}

  \item{\code{@S3method generic class}}{produces
  \code{S3method(generic,class)} directive}

  }

  There are five tags for importing objects into the
  package:

  \describe{

  \item{\code{@import package}}{produces
  \code{import(package) directive to import all functions
  from the given package}}

  \item{\code{@importFrom package functiona functionb
  ...}}{produces multiple \code{importFrom(package,
  function)} directives to import selected functions from a
  package.}

  \item{\code{@importClassesFrom package classa classb
  ...}}{produces multiple \code{importClassesFrom(package,
  class)} directives to import selected classes from a
  package.}

  \item{\code{@importMethodsFrom package methoda methodb
  ...}}{produces multiple \code{importMethodsFrom(package,
  method)} directives to import selected methods from a
  package.}

  \item{\code{@useDynLib package}}{produces a
  \code{useDynLib(package)} directive to import all
  compiled routines from the shared objects in the
  specified package}

  \item{\code{@useDynLib paackage routinea
  routineb}}{produces multiple
  \code{useDynLib(package,routine)} directions to import
  specified compiled routines from a package.} }

  Only unique directives are saved to the \file{NAMESPACE}
  file, so you can repeat them as needed to maintain a
  close link between the functions where they are needed
  and the namespace file..
}
\examples{
#' An example file, example.R, which imports
#' packages foo and bar
#' @import foo bar
NULL

#' An exportable function
#' @export
fun <- function() {}

roclet <- namespace_roclet()
\dontrun{roc_proc(roclet, "example.R")}
\dontrun{roc_out(roclet, "example.R", ".")}
}
\seealso{
  Other roclets: \code{\link{collate_roclet}},
  \code{\link{rd_roclet}}
}

