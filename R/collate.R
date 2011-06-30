#' @include roxygen.R
#' @include string.R
#' @include functional.R
#' @include roclet.R
#' @include description.R
#' @include parse.R
NULL

#' Collate value parser
#' @name include
#' @seealso make.collate.roclet
register.preref.parsers(parse.value,
                        'include')

#' Make collate roclet which parses the given files; topologically
#' sorting \code{@@include}s, and either merging the \code{Collate:}
#' directive with a pre-existing \file{DESCRIPTION} or writing to
#' standard out.
#'
#' Each \code{@@include} tag should specify the filename of one intrapackage
#' dependency; multiple \code{@@include} tags may be given.
#'
#' Contains the member function \code{parse} which parses an arbitrary number
#' of files, and \code{parse.dir} which recursively parses a directory tree.
#'
#' @param package.dir the package's top directory
#' @param roxygen.dir where to create roxygen output; defaults to
#' \file{package.roxygen}.
#' @param merge.file \file{DESCRIPTION} file with which to merge directive;
#' or \code{NULL} for none
#' @param target.file whither to \code{cat} directive (whether merged or
#' not); blank line is standard out
#' @param verbose whether to describe what we're doing with the
#' target.file
#' @return Rd roclet
#' @seealso \code{\link{make.roclet}}
#' @examples
#' #' `example-a.R', `example-b.R' and `example-c.R' reside
#' #' in the `example' directory, with dependencies
#' #' a -> {b, c}. This is `example-a.R'.
#' #' @@include example-b.R
#' #' @@include example-c.R
#' NULL
#'
#' roclet <- make.collate.roclet()
#' \dontrun{roclet$parse.dir('example')}
#' @export
make.collate.roclet <- function(package.dir, 
                                roxygen.dir, 
                                merge.file=NULL,
                                target.file=NULL,
                                verbose=TRUE) {
                                  
  if (is.null(merge.file)) {
    merge.file <- file.path(package.dir, DESCRIPTION.FILE)
  }
  if (is.null(target.file)) {
    target.file <- file.path(roxygen.dir, DESCRIPTION.FILE)
  }
  
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
      vertices <<- append(vertices, as.list(structure(c(make.vertex(file)),
          names=file)))

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
    file <- basename(partitum$srcref$filename)
    maybe.append.vertex(file)
    vertex <- vertices[[file]]
    current.predecessor <<- vertex
  }

  topological.sort <- function(vertices) {
    sorted <- NULL
    visit <- function(predecessor) {
      predecessor$discovered <- TRUE
      for (ancestor in predecessor$ancestors)
        if (!ancestor$discovered)
          visit(ancestor)
      sorted <<- append(sorted, predecessor)
    }
    for (vertex in vertices)
      if (!vertex$discovered)
        visit(vertex)

    sorted
  }

  COLLATE.FIELD <- 'Collate:'

  merge <- function(files) {
    if (verbose && !is.null.string(target.file))
      cat(sprintf('Merging collate directive with %s to %s',
                  merge.file,
                  target.file), '\n')
    pre.parse <- function(parsed.fields) file.remove(target.file)
    post.parse <- function(parsed.fields)
      cat.description('Collate', files, file=target.file)
    parse.default <- Curry(cat.description, file=target.file)
    parser <- make.description.parser(parse.default,
                                      pre.parse=pre.parse,
                                      post.parse=post.parse)
    parser$register.parser('Collate', noop.description)
    ## Force parse.description.file to be evaluated before
    ## parser$parser (applicative order).
    parsed.file <- parse.description.file(merge.file)
    parser$parse(parsed.file)
  }

  post.files <- function() {
    sorted <- topological.sort(vertices)
    names <- basename(sapply(sorted, function(x) x$file))
    name_string <- paste(sprintf("'%s'", names), collapse = " ")

    if (!is.null(merge.file))
      merge(name_string)
    else
      cat.description('Collate', name_string, file=target.file)
  }

  roclet <- make.roclet(package.dir, roxygen.dir, parse.include,
                        pre.parse=pre.parse,
                        post.files=post.files)

  roclet$register.default.parser('include')

  roclet
}
