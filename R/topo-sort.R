topo_sort <- R6::R6Class("topo_sort", public = list(
  vertices = list(),

  add = function(file) {
    if (is.null(self$vertices[[file]])) {
      self$vertices[[file]] <- vertex$new(file)
    }
    self$vertices[[file]]
  },

  add_ancestor = function(predecessor_name, ancestor_name) {
    predecessor <- self$add(predecessor_name)
    ancestor <- self$add(ancestor_name)

    predecessor$add_ancestor(ancestor)
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
))

vertex <- R6::R6Class("Vertex", public = list(
  file = NA_character_,
  discovered = FALSE,
  ancestors = list(),

  initialize = function(file) {
    self$file <- file
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

))
