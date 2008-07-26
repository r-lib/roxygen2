#' @include roxygen.R
#' @include parse.R
#' @include roclet.R
#' @include string.R
roxygen()

#' Make collate roclet which parses the given files; topologically
#' sorting \code{@@include}s and writing a \code{Collate} directive to
#' standard out.
#'
#' Contains the member function \code{parse} which parses the result
#' of \code{parse.files}.
#'
#' @return Rd roclet
make.collate.roclet <- function() {
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

  maybe.append.ancestor <- function(predecessor, ancestor)
    if (!c(ancestor) %in% predecessor$ancestors)
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

  post.files <-
    function() cat('Collate:',
                   Reduce.paste(function(vertex)
                                sprintf("'%s'", vertex$file),
                                topological.sort(vertices),
                                ' '),
                   '\n',
                   sep='')

  roclet <- make.roclet(parse.include,
                        pre.parse=pre.parse,
                        post.files=post.files)

  roclet$register.default.parser('include')

  roclet
}
