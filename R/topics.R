# Manage a list of topics, indexed by file name.
# Adding a topic with an existing file name merges it with the existing topic
RoxyTopics <- R6::R6Class(
  "RoxyTopics",
  public = list(
    topics = list(),

    add = function(topic, block) {
      if (is.null(topic)) {
        return()
      }
      stopifnot(inherits(topic, "RoxyTopic"))

      filename <- topic$filename
      if (filename %in% names(self$topics)) {
        self$topics[[filename]]$add(topic, block)
      } else {
        self$topics[[filename]] <- topic
      }

      invisible()
    },

    # Drop any topics that don't have a title
    drop_invalid = function() {
      for (topic in names(self$topics)) {
        if (!self$topics[[topic]]$is_valid()) {
          warn_roxy_topic(topic, "Skipping; no name and/or title")
          self$topics[[topic]] <- NULL
        }
      }

      invisible()
    },

    get = function(filename) {
      self$topics[[filename]]
    },

    # Given a topic name, find its file name.
    find_filename = function(name) {
      for (i in seq_along(self$topics)) {
        if (name %in% self$topics[[i]]$get_value("name")) {
          return(names(self$topics)[[i]])
        }
      }
      NA_character_
    },

    # Topologically sort the topics.
    #
    # @param deps A function. Is passed RoxyTopic, and should return a character
    #   vector of topic names
    topo_order = function(dependencies) {
      topo <- TopoSort$new()

      for (i in seq_along(self$topics)) {
        name <- names(self$topics)[[i]]
        topo$add(name)

        dep_topics <- dependencies(self$topics[[i]])
        for (dep_topic in dep_topics) {
          dep_rd <- self$find_filename(dep_topic)
          if (!is.na(dep_rd)) {
            topo$add_ancestor(name, dep_rd)
          }
        }
      }

      topo$sort()
    },

    # Call fun in topological order defined by dep.
    topo_apply = function(dep, fun, ...) {
      topics_topo <- self$topo_order(dep)
      for (topic_name in topics_topo) {
        topic <- self$get(topic_name)
        fun(topic, self, ...)
      }

      invisible()
    },

    apply = function(fun, ...) {
      for (topic in self$topics) {
        fun(topic, self, ...)
      }
      invisible()
    },

    # Extract values for simple fields
    simple_values = function(field) {
      fields <- lapply(self$topics, \(rd) rd$get_section(field))
      lapply(compact(fields), "[[", "value")
    }
  )
)
