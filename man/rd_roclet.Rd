\name{rd_roclet}
\alias{rd_roclet}
\title{Roclet: make Rd files.}
\usage{
rd_roclet()
}
\description{
This roclet is the workhorse of \pkg{roxygen}, producing the Rd files that
document that functions in your package.
}
\details{
This roclet also automatically runs \code{\link[tools]{checkRd}} on all
generated Rd files so that you know as early as possible if there's a
problem.
}
\section{Required tags}{
  As well as a title and description, extracted from the
  first sentence and first paragraph respectively, all
  functions must have the following tags:

  \describe{

  \item{\code{@param name description}}{Document a
  parameter.  Documentation is required for every
  parameter.}

  \item{\code{@inheritParams
  source_function}}{Alternatively, you can inherit
  parameter description from another function. This tag
  will bring in all documentation for parameters that are
  undocumented in the current function, but documented in
  the source function. The source can be a function in the
  current package, \code{function}, or another package
  \code{package::function}.}

  \item{\code{@method generic class}}{Required if your
  function is an S3 method.  This helps R to distinguish
  between (e.g.) \code{t.test} and the \code{t} method for
  the \code{test} class.}

  }
}

\section{Optional tags that add extra information}{
  Valid tags for \code{rd_roclet} are:

  \describe{

  \item{\code{@examples R code...}}{Highly recommended:
  example code that demonstrates how to use your function.
  Use \\dontrun{} to tag code that should not automatically
  be run.}

  \item{\code{@example
  path/relative/to/packge/root}}{Instead of including
  examples directly in the documentation, you can include
  them as separate files, and use the \code{@example} tag
  to insert them into the documentation.}

  \item{\code{@return}}{Used to document the object
  returned by the function. For lists, use the
  \code{\\item{name a}{description a}} describe each
  component of the list}

  \item{\code{@author authors...}}{A free text string
  describing the authors of the function.  This is
  typically only necessary if the author is not the same as
  the package author.}

  \item{\code{@note contents}}{Create a note section
  containing additional information.}

  \item{\code{@section Name: contents}}{Use to add to an
  arbitrary section to the documentation. The name of the
  section will be the content before the first colon, and
  the contents will be everything after the colon.}

  \item{\code{@keywords keyword1 keyword2 ...}}{Keywords
  are optional, but if present, must be taken from the list
  in \file{file.path(R.home(), "doc/KEYWORDS")}.  Use the
  \code{internal} keyword for functions that should not
  appear in the main function listing.}

  }
}

\section{Optional tags for cross-referencing}{
  \describe{

  \item{\code{@aliases space separated aliases}}{Add
  additional aliases, through which the user can find the
  documentation with \code{?}.  The topic name is always
  included in the list of aliases.}

  \item{\code{@concepts space separated concepts}}{Similar
  to \code{@aliases} but for \code{\link{help.search}}}

  \item{\code{@references free text reference}}{Pointers to
  the literature related to this object.}

  \item{\code{@seealso Text with
  \\code{\\link{function}}}}{Pointers to related R objects,
  and why you might be interested in them.}

  \item{\code{@family family name}}{Automatically adds
  see-also cross-references between all functions in a
  family. A function can belong to multiple families.} }
}

\section{Template tags}{
  Templates make it possible to substantially reduce
  documentation duplication. A template is an \file{R} file
  in the \code{man-roxygen/} directory. It is processed
  with \code{\link[brew]{brew}} and then inserted into the
  roxygen block. You can run any R code with brew; you can
  insert template variables with \code{<\%= varname \%>}.

  \describe{

  \item{\code{@template templateName}}{Insert named
  template in current location.}

  \item{\code{@templateVar varname value}}{Set up variables
  for template use.}

  }

  Limitations:

  \itemize{ \item Templates are not parsed recursively, so
  you can not include templates from within other
  templates.

  \item Templates must be composed of complete tags -
  becuase all roxygen tags are current block tags, they can
  not be used for inline insertions. }
}

\section{Optional tags that override defaults}{
  These tags all override the default values that roxygen
  guess from inspecting the source code.

  \describe{

  \item{\code{@rdname filename}}{Overrides the output file
  name (without extension). This is useful if your function
  has a name that is not a valid filename (e.g.
  \code{[[<-}), or you want to merge documentation for
  multiple function into a single file.}

  \item{\code{@title Topic title}}{Specify the topic title,
  which by by default is taken from the first sentence of
  the roxygen block.}

  \item{\code{@usage usage_string}}{Override the default
  usage string.  You should not need to use this tag - if
  you are trying to document multiple functions in the same
  topic, use \code{@rdname}.}

  }
}

\section{Tags for non-functions}{
  These tags are useful when documenting things that aren't
  functions, datasets and packages.

  \describe{

  \item{\code{@name topicname}}{Override the default topic
  name, which is taken by default from the object that is
  assigned to in the code immediately following the roxygen
  block. This tag is useful when documenting datasets, and
  other non-function elements.}

  \item{\code{@docType type}}{Type of object being
  documented. Useful values are \code{data} and
  \code{package}. Package doc type will automatically add a
  \code{package-} alias if needed.}

  \item{\code{@format description}}{A textual description
  of the format of the object.}

  \item{\code{@source text}}{The original source of the
  data.}

  \item{\code{@slot name description}}{Describe the slots
  of an S4 class in a standard way. Slots will be listed in
  their own section.}

  }
}
\examples{
roclet <- rd_roclet()
\dontrun{roc_proc(roclet, "example.R")}
\dontrun{roc_out(roclet, "example.R", ".")}
}
\seealso{
Other roclets: \code{\link{S3method}},
  \code{\link{export}}, \code{\link{exportClass}},
  \code{\link{exportMethod}}, \code{\link{import}},
  \code{\link{importClassesFrom}},
  \code{\link{importFrom}},
  \code{\link{importMethodsFrom}},
  \code{\link{namespace_roclet}}
}

