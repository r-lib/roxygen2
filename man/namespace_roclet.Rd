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
\value{
  Namespace roclet
}
\description{
  Roclet: make NAMESPACE.
}
\details{
  This roclet automates the production of a
  \file{NAMESPACE} file, see \cite{Writing R Extensions}
  (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf})
  for details.
}
\section{Tags

There are four tags for exporting objects from the package}{
  \describe{

  \item{\code{@export}}{May be specified with or without
  value; if without, roxygen guess at the type of export:
  \code{exportMethod} for S4 methods, \code{S3method} for
  S3 methods, \code{exportClass} for S4 classes,
  \code{export} for functions and so on.

  If you specify a value, it overrides this process and
  will produce multiple export direction, e.g.
  \code{@export f} will add \code{export(f)} and
  \code{export(g)} directives.

  This is the only directive you should need for documented
  function, the other directives are useful if you want to
  export (e.g.) methods but not document them}

  \item{\code{@exportClass x}}{Produces
  \code{exportClasses(x)} directive.}

  \item{\code{@exportMethod x}}{Produces
  \code{exportMethods(x)} directive.} .
  \item{\code{@S3method generic class}}{Produces
  \code{S3method(generic,class)} directive}

  }

  There are four tags for importing object into the
  package:

  \describe{

  \item{\code{@import package}}{Produces
  \code{import(package) directive to import all functions
  from the given package}}

  \item{\code{@importFrom package functiona functionb
  ...}}{Produces multiple \code{importFrom(package,
  function)} directives to import selected functions from a
  package.}

  \item{\code{@importClassesFrom package classa classb
  ...}}{Produces multiple \code{importClassesFrom(package,
  class)} directives to import selected classes from a
  package.}

  \item{\code{@importMethodsFrom package methoda methodb
  ...}}{Produces multiple \code{importMethodsFrom(package,
  method)} directives to import selected methods from a
  package.}

  }

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

