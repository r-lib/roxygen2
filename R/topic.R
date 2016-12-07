# An RoxyTopic is an ordered collection of unique roxy_fields
RoxyTopic <- R6::R6Class("RoxyTopic", public = list(
  fields = list(),
  filename = "",

  format = function(...) {
    # This has to happen here to get a better order when combining topics
    order <- c("backref", "docType", "encoding", "name", "alias", "title",
      "format", "source", "usage", "param", "value", "description",
      "details", "minidesc", "field", "slot", "rcmethods", "note",
      "section", "examples", "references", "seealso", "author",
      "concept", "keyword", "rawRd")
    fields <- move_names_to_front(self$fields, order)

    formatted <- lapply(fields, format, ...)
    paste0(
      made_by("%"),
      paste0(unlist(formatted), collapse = "\n")
    )
  },

  is_valid = function() {
    # Needs both title and name fields to generate valid Rd
    all(self$has_field(c("title", "name")))
  },

  has_field = function(field_name) {
    field_name %in% names(self$fields)
  },

  get_field = function(field_name) {
    self$fields[[field_name]]
  },

  inherits_from = function(type) {
    if (!self$has_field("inherit")) {
      return(character())
    }

    inherit <- self$get_field("inherit")

    inherits_field <- vapply(inherit$fields, function(x) type %in% x, logical(1))
    sources <- inherit$source[inherits_field]

    if ("NULL" %in% sources)
      return(character())

    sources
  },

  inherits_section_from = function() {
    if (!self$has_field("inherit_section")) {
      return(character())
    }

    self$get_field("inherit_section")$source
  },

  # Ensures that each type of name (as given by its name), only appears
  # once in self$fields
  add_field = function(field, overwrite = FALSE) {
    if (is.null(field))
      return()

    stopifnot(is_roxy_field(field))
    field_name <- field$field
    if (self$has_field(field_name) && !overwrite) {
      field <- merge(self$get_field(field_name), field)
    }

    self$fields[[field_name]] <- field

    invisible()
  },

  add_simple_field = function(name, values, overwrite = FALSE) {
    self$add_field(roxy_field_simple(name, values), overwrite = overwrite)
    invisible()
  },

  add = function(x, overwrite = FALSE) {
    if (inherits(x, "RoxyTopic")) {
      self$add(x$fields, overwrite = overwrite)
    } else if (inherits(x, "roxy_field")) {
      self$add_field(x, overwrite = overwrite)
    } else if (is.list(x)) {
      for (field in x) {
        self$add_field(field, overwrite = overwrite)
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

get_tag <- function(topic, field_name) topic$get_field(field_name)
