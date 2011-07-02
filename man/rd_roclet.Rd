\name{rd_roclet}
\alias{rd_roclet}
\title{Roclet: make Rd files.}
\description{
  Roclet: make Rd files.
}
\details{
  \describe{ \item{\code{@name topicname}}{Override the
  default topic name, which is taken by default from the
  object that is assigned to in the code immediately
  following the roxygen block. This tag is useful when
  documenting datasets, and other non-function elements.}

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

  }
}

