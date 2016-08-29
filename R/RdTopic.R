# An RdTopic is an ordered collection of unique rd_tags
RdTopic <- R6::R6Class("RdTopic", public = list(
  tags = list(),

  format = function(...) {
    order <- c("backref", "docType", "encoding", "name", "alias", "title",
      "format", "source", "usage", "param", "value", "description",
      "details", "minidesc", "reexport", "field", "slot", "rcmethods", "note",
      "section", "examples", "author", "references", "seealso",
      "concept", "keyword", "rawRd")

    tags <- move_names_to_front(self$tags, order)

    formatted <- lapply(tags, format, ...)
    paste0(
      made_by("%"),
      paste0(unlist(formatted), collapse = "")
    )
  },

  has_tag = function(tag_name) {
    tag_name %in% names(self$tags)
  },

  get_tag = function(tag_name) {
    self$tags[[tag_name]]
  },

  # Ensures that each type of name (as given by its name), only appears
  # once in self$tags
  add_tag = function(tag, overwrite = FALSE) {
    stopifnot(is.rd_tag(tag))
    if (is.null(tag))
      return()

    tag_name <- tag$tag
    if (self$has_tag(tag_name) && !overwrite) {
      # Currently merge returns a list of tags
      tag <- merge(self$get_tag(tag_name), tag)[[1]]
    }

    self$tags[[tag_name]] <- tag

    invisible()
  },

  add_tags = function(tags, overwrite = FALSE) {
    for (tag in tags) {
      self$add_tag(tag, overwrite = overwrite)
    }
  },

  add_file = function(y, overwrite = FALSE) {
    if (is.null(y))
      return()

    self$add_tags(y$tags, overwrite = overwrite)
    invisible()
  }
))

move_names_to_front <- function(x, to_front) {
  nms <- names(x)
  x[union(intersect(to_front, nms), nms)]
}

get_tag <- function(topic, tag_name) topic$get_tag(tag_name)
