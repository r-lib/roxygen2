#' @include roxygen.R
#' @include string.R
#' @include roclet.R
roxygen()

#' Make collate roclet which parses the given files; topologically
#' sorting \code{@@include}s and writing a \code{Collate:} directive to
#' standard out.
#'
#' Each \code{@@include} tag should specify the filename of one intrapackage
#' dependency; multiple \code{@@include} tags may be given.
#'
#' Contains the member function \code{parse} which parses an arbitrary number
#' of files.
#'
#' @param merge.file \file{DESCRIPTION} file with which to merge directive;
#' or \code{NULL} for none
#' @param target.file whither to \code{cat} directive (whether merged or
#' not); blank line is standard out
#' @param verbose whether to describe what we're doing with the
#' target.file
#' @return Rd roclet
#' @seealso \code{\link{make.roclet}}
#' @examples
#' #' An example source file, example.R
#' #' @@include roxygen.R
#' #' @@include collate.R
#' roxygen()
#'
#' roclet <- make.collate.roclet()
#' \dontrun{roclet$parse('example.R')}
#' @export
make.collate.roclet <- function(merge.file=NULL,
                                target.file='',
                                verbose=TRUE) {
  vertices <- NULL

  make.vertex <- function(file) {
    vertex <- new.env(parent=emptyenv())
    vertex$file <- trim(file)
    vertex$discovered <- FALSE
    vertex$ancestors <- NULL
    vertex
  }

  maybe.append.vertex <- function(file)
    if (is.null(vertices[[file]]))
      assign.parent('vertices',
                    append(vertices,
                           as.list(structure(c(make.vertex(file)),
                                             names=file))),
                    environment())

  member <- function(ancestor, ancestors) {
    for (vertex in ancestors)
      if (identical(ancestor, vertex))
        TRUE
    FALSE
  }

  maybe.append.ancestor <- function(predecessor, ancestor)
    if (!member(ancestor, predecessor$ancestors))
      predecessor$ancestors <-
        append(ancestor, predecessor$ancestors)

  current.predecessor <- NULL

  parse.include <- function(key, file) {
    file <- trim(file)
    maybe.append.vertex(file)
    ancestor <- vertices[[file]]
    maybe.append.ancestor(current.predecessor,
                          ancestor)
  }
  
  pre.parse <- function(partitum) {
    file <- partitum$srcref$filename
    maybe.append.vertex(file)
    vertex <- vertices[[file]]
    assign.parent('current.predecessor',
                  vertex,
                  environment())
  }

  topological.sort <- function(vertices) {
    sorted <- NULL
    visit <- function(predecessor) {
      predecessor$discovered <- TRUE
      for (ancestor in predecessor$ancestors)
        if (!ancestor$discovered)
          visit(ancestor)
      assign.parent('sorted',
                    append(sorted, predecessor),
                    environment())
    }
    for (vertex in vertices)
      if (!vertex$discovered)
        visit(vertex)
  }

  COLLATE.FIELD <- 'Collate:'

  merge <- function(directive) {
    lines <- readLines(merge.file)
    filtered.lines <- Filter(function(line)
                             length(grep(sprintf('^%s', COLLATE.FIELD),
                                         trim(line))) == 0,
                             lines)
    if (verbose && !is.null.string(target.file))
      cat(sprintf('Merging `Collate:\' from %s to %s',
                  merge.file,
                  target.file), '\n')
    cat(filtered.lines, directive, file=target.file, sep='\n')
  }

  post.files <- function() {
    directive <-
      sprintf('Collate:%s',
              Reduce.paste(function(vertex)
                           sprintf("'%s'", vertex$file),
                           topological.sort(vertices),
                           ' '))
    if (!is.null(merge.file))
      merge(directive)
    else
      cat(directive, '\n', file=target.file, sep='')
  }

  roclet <- make.roclet(parse.include,
                        pre.parse=pre.parse,
                        post.files=post.files)

  roclet$register.default.parser('include')

  roclet
}
