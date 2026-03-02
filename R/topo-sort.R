TopoSort <- R6::R6Class(
  "TopoSort",
  public = list(
    vertices = list(),

    add = function(name) {
      if (is.null(self$vertices[[name]])) {
        self$vertices[[name]] <- Vertex$new(name)
      }
      invisible(self$vertices[[name]])
    },

    add_ancestor = function(predecessor_name, ancestor_name) {
      predecessor <- self$add(predecessor_name)
      ancestor <- self$add(ancestor_name)

      predecessor$add_ancestor(ancestor)
    },

    sort = function() {
      sorted <- list()

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
      map_chr(sorted, "name")
    }
  )
)

Vertex <- R6::R6Class(
  "Vertex",
  public = list(
    name = NA_character_,
    discovered = FALSE,
    ancestors = list(),

    initialize = function(name) {
      self$name <- name
    },

    has_ancestor = function(ancestor) {
      for (vertex in self$ancestors) {
        if (identical(ancestor, vertex)) {
          return(TRUE)
        }
      }
      FALSE
    },

    add_ancestor = function(ancestor) {
      if (!self$has_ancestor(ancestor)) {
        self$ancestors <- append(ancestor, self$ancestors)
      }
    }
  )
)
