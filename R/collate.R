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
      assign.parent('time', time + 1, environment())
      predecessor$finished <- time
      assign.parent('sorted',
                    append(sorted, predecessor),
                    environment())
    }
    for (vertex in vertices)
      if (!vertex$is.discovered())
        visit(vertex)
  }

  post.files <-
    function() cat('collate',
                   Reduce.paste(function(vertex) vertex$file,
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
