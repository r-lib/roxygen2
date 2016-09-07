# Manage a list of topics, indexed by file name.
# Adding a topic with an existing file name merges it with the existing topic
RoxyTopics <- R6::R6Class("RoxyTopics", public = list(
  topics = list(),

  add = function(topic) {
    if (is.null(topic))
      return()
    stopifnot(inherits(topic, "RoxyTopic"))

    filename <- topic$filename
    if (filename %in% names(self$topics)) {
      self$topics[[filename]]$add(topic)
    } else {
      self$topics[[filename]] <- topic
    }

    invisible()
  },

  # Drop any topics that don't have a title
  drop_invalid = function() {
    for (topic in names(self$topics)) {
      if (!self$topics[[topic]]$is_valid()) {
        warning(topic, " is missing name/title. Skipping", call. = FALSE)
        self$topics[[topic]] <- NULL
      }
    }

    invisible()
  },

  get = function(filename) {
    self$topics[[filename]]
  },

  #' Given a topic, find its file name.
  find_filename = function(name) {
    for (i in seq_along(self$topics)) {
      if (name %in% self$topics[[i]]$get_field("name")$values) {
        return(names(self$topics)[[i]])
      }
    }
    NA_character_
  },

  # Topologically sort the topics.
  #
  # @param deps A function. Is passed a topic, and should return a character
  #   vector giving the file names that it depends on.
  topo_sort = function(deps) {
    topo <- TopoSort$new()

    for (i in seq_along(self$topics)) {
      name <- names(self$topics)[[i]]
      topo$add(name)

      for (dep in deps(self$topics[[i]])) {
        if (!is.na(dep))
          topo$add_ancestor(name, dep)
      }
    }

    self$topics <- self$topics[topo$sort()]
    invisible()
  },

  # Extract values for simple fields
  simple_values = function(field) {
    fields <- lapply(self$topics, function(rd) rd$get_field(field))
    lapply(compact(fields), "[[", "values")
  }

))
