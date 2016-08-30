# An RoxyTopic is an ordered collection of unique rd_tags
RoxyTopic <- R6::R6Class("RoxyTopic", public = list(
  tags = list(),
  filename = "",

  format = function(...) {
    # This has to happen here to get a better order when combining topics
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
    if (is.null(tag))
      return()

    stopifnot(is.rd_tag(tag))
    tag_name <- tag$tag
    if (self$has_tag(tag_name) && !overwrite) {
      tag <- merge(self$get_tag(tag_name), tag)
    }

    self$tags[[tag_name]] <- tag

    invisible()
  },

  add = function(x, overwrite = FALSE) {
    if (inherits(x, "RoxyTopic")) {
      self$add(x$tags, overwrite = overwrite)
    } else if (inherits(x, "rd_tag")) {
      self$add_tag(x, overwrite = overwrite)
    } else if (is.list(x)) {
      for (tag in x) {
        self$add_tag(tag, overwrite = overwrite)
      }
    } else if (is.null(x)) {
      # skip
    } else {
      stop("Don't know how to add object of type ", class(x)[1])
    }
    invisible()
  }


))

move_names_to_front <- function(x, to_front) {
  nms <- names(x)
  x[union(intersect(to_front, nms), nms)]
}

get_tag <- function(topic, tag_name) topic$get_tag(tag_name)
