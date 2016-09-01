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
  }

))
