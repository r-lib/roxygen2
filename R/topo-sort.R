topo_sort <- function() {
  vertices <- NULL

  make.vertex <- function(file) {
    vertex <- new.env(parent = emptyenv())
    vertex$file <- file
    vertex$discovered <- FALSE
    vertex$ancestors <- NULL
    vertex
  }

  maybe.append.vertex <- function(file) {
    if (is.null(vertices[[file]])) {
      vertices[[file]] <<- make.vertex(file)
    }
    vertices[[file]]
  }

  member <- function(ancestor, ancestors) {
    for (vertex in ancestors)
      if (identical(ancestor, vertex))
        TRUE
    FALSE
  }
  
  maybe.append.ancestor <- function(predecessor, ancestor_name) {
    ancestor <- maybe.append.vertex(ancestor_name)
    
    if (!member(ancestor, predecessor$ancestors)) {
      predecessor$ancestors <- append(ancestor, predecessor$ancestors)
    }
  }


  topological.sort <- function() {
    sorted <- NULL
    visit <- function(predecessor) {
      predecessor$discovered <- TRUE
      for (ancestor in predecessor$ancestors) {
        if (!ancestor$discovered) {
          visit(ancestor)
        }
      }
      sorted <<- append(sorted, predecessor)
    }
    
    for (vertex in vertices) {
      if (!vertex$discovered) {
        visit(vertex)        
      }
    }
    vapply(sorted, function(x) x$file, character(1))
  }
  
  list(add = maybe.append.vertex, 
     add_ancestor = maybe.append.ancestor, 
     sort = topological.sort
  )
}
