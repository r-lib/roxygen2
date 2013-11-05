\name{is_s3_generic}
\alias{is_s3_generic}
\title{Determine if a function is an S3 generic or S3 method.}
\usage{
  is_s3_generic(name, env = parent.frame())
}
\arguments{
  \item{name}{name of function.}

  \item{env}{environment to search in.}
}
\description{
  \code{is_s3_generic} compares name to
  \code{.knownS3Generics} and \code{.S3PrimitiveGenerics},
  then uses \code{\link[codetools]{findGlobals}} to see if
  the functionion calls \code{\link{UseMethod}}.

  \code{is_s3_method} builds names of all possible generics
  for that function and then checks if any of them actually
  is a generic.
}

