# An RoxyTopic is an ordered collection of unique rd_sections
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
    all(self$has_section(c("title", "name")))
  },

  has_section = function(field_name) {
    field_name %in% names(self$fields)
  },

  get_section = function(field_name) {
    self$fields[[field_name]]
  },

  get_value = function(field) {
    self$get_section(field)$value
  },

  get_rd = function(field) {
    format(self$get_section(field))
  },
  get_name = function() {
    self$get_value("name")
  },

  inherits_from = function(type) {
    if (!self$has_section("inherit")) {
      return(character())
    }

    inherit <- self$get_value("inherit")

    inherits_field <- map_lgl(inherit$fields, function(x) type %in% x)
    sources <- inherit$source[inherits_field]

    if ("NULL" %in% sources)
      return(character())

    sources
  },

  inherits_section_from = function() {
    if (!self$has_section("inherit_section")) {
      return(character())
    }

    self$get_value("inherit_section")$source
  },

  # Ensures that each type of name (as given by its name), only appears
  # once in self$fields
  add_section = function(field, overwrite = FALSE) {
    if (is.null(field))
      return()

    field_name <- field$type
    if (self$has_section(field_name) && !overwrite) {
      field <- merge(self$get_section(field_name), field)
    }

    self$fields[[field_name]] <- field

    invisible()
  },

  add = function(x, overwrite = FALSE) {
    if (inherits(x, "RoxyTopic")) {
      self$add(x$fields, overwrite = overwrite)
    } else if (inherits(x, "rd_section")) {
      self$add_section(x, overwrite = overwrite)
    } else if (is.list(x)) {
      for (field in x) {
        self$add_section(field, overwrite = overwrite)
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
