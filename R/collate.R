#' @include roclet.R
#' @include string.R
make.collate.roclet <- function() {
  vertices <- NULL

  make.vertex <- function(file) {
    vertex <- new.env(parent=emptyenv())
    vertex$file <- trim(file)
    vertex$discovered <- -1
    vertex$finished <- -1
    vertex$predecessor <- NULL
    vertex$ancestors <- NULL
    vertex$is.discovered <-
      function() vertex$discovered >= 0
    vertex$is.finished <-
      function() vertex$discovered >= 0
    structure(vertex, class='vertex')
  }

  maybe.append.vertex <- function(file) {
    file <- trim(file)
    if (is.null(vertices[[file]]))
      assign.parent('vertices',
                    append(vertices,
                           as.list(structure(c(make.vertex(file)),
                                             names=file))),
                    environment())
  }

  parse.include <- function(key, file)
    maybe.append.vertex(file)
  
  pre.parse <- function(partitum)
    maybe.append.vertex(partitum$srcref$filename)

  topological.sort <- function(vertices) {
    time <- 0
    sorted <- NULL
    visit <- function(predecessor) {
      assign.parent('time', time + 1, environment())
      predecessor$discovered <- time
      for (ancestor in predecessor$ancestors)
        if (!ancestor$is.discovered()) {
          ancestor$predecessor <- predecessor
          visit(ancestor)
        }
      predecessor$finished <- time
      assign.parent('sorted',
                    append(predecessor, sorted),
                    environment())
    }
    for (vertex in vertices)
      if (!vertex$is.discovered())
        visit(vertex)
  }

  post.files <-
    function() cat(Reduce.paste(function(vertex) vertex$file,
                                topological.sort(vertices),
                                ' '),
                   '\n')

  roclet <- make.roclet(parse.include,
                        pre.parse=pre.parse,
                        post.files=post.files)

  roclet$register.default.parser('include')

  roclet
}
