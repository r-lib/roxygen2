topo_sort <- R6::R6Class("topo_sort",
  private = list(
    make.vertex = function(file) {
      vertex <- new.env(parent = emptyenv())
      vertex$file <- file
      vertex$discovered <- FALSE
      vertex$ancestors <- NULL
      vertex
    },

    member = function(ancestor, ancestors) {
      for (vertex in ancestors)
        if (identical(ancestor, vertex))
          return(TRUE)
      FALSE
    }
  ),
  public = list(
    vertices = list(),

    add = function(file) {
      if (is.null(self$vertices[[file]])) {
        self$vertices[[file]] <- private$make.vertex(file)
      }
      self$vertices[[file]]
    },

    add_ancestor = function(predecessor, ancestor_name) {
      ancestor <- self$add(ancestor_name)

      if (!private$member(ancestor, predecessor$ancestors)) {
        predecessor$ancestors <- append(ancestor, predecessor$ancestors)
      }
      invisible()
    },

    sort = function() {
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

      for (vertex in self$vertices) {
        if (!vertex$discovered) {
          visit(vertex)
        }
      }
      vapply(sorted, function(x) x$file, character(1))
    }
  )
)
