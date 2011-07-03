\name{rd_roclet}
\alias{rd_roclet}
\title{Roclet: make Rd files.}
\description{
  Roclet: make Rd files.
}
\details{
  This roclet is the workhorse of \pkg{roxygen}, producing
  the Rd files that document that functions in your
  package.
}
\section{Tags}{
  Valid tags for \code{rd_roclet} are:

  \describe{

  \item{\code{@name topicname}}{Override the default topic
  name, which is taken by default from the object that is
  assigned to in the code immediately following the roxygen
  block. This tag is useful when documenting datasets, and
  other non-function elements.}

  \item{\code{@aliases space separated aliases}}{Add
  additional aliases. The topic name is always included in
  the list of aliases.}

  \item{\code{@title Topic title}}{Specify the topic title,
  which by by default is taken from the first sentence of
  the roxygen block.}

  \item{\code{@rdname filename}}{Overrides the output file
  name (without extension). This is useful if your function
  has a name that is not a valid filename (e.g.
  \code{[[<-}), or you want to merge documentation for
  multiple function into a single file.}

  \item{\code{@usage usage_string}}{Override the default
  usage string. You should not need to use this tag - if
  you are trying to document multiple functions in the same
  topic, use \code{@rdname}.}

  \item{\code{@section Name: contents}}{Use to add to an
  arbitrary section to the documentation. The name of the
  section will be the content before the first colon, and
  the contents will be everything after the colon.}

  }
}
\examples{
roclet <- rd_roclet()
\dontrun{roc_proc(roclet, "example.R")}
\dontrun{roc_out(roclet, "example.R", ".")}
}

