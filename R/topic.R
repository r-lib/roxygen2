# An RoxyTopic is an ordered collection of unique rd_sections
RoxyTopic <- R6::R6Class("RoxyTopic", public = list(
  sections = list(),
  filename = "",

  format = function(...) {
    # This has to happen here to get a better order when combining topics
    order <- c("backref", "docType", "encoding", "name", "alias", "title",
      "format", "source", "usage", "param", "value", "description",
      "details", "minidesc", "field", "slot", "rcmethods", "note",
      "section", "examples", "references", "seealso", "author",
      "concept", "keyword", "rawRd")
    sections <- move_names_to_front(self$sections, order)

    formatted <- lapply(sections, format, ...)
    paste0(
      made_by("%"),
      paste0(unlist(formatted), collapse = "\n")
    )
  },

  is_valid = function() {
    # Needs both title and name sections to generate valid Rd
    all(self$has_section(c("title", "name")))
  },

  has_section = function(type) {
    type %in% names(self$sections)
  },

  get_section = function(type) {
    self$sections[[type]]
  },

  get_value = function(type) {
    self$get_section(type)$value
  },

  get_rd = function(type) {
    format(self$get_section(type))
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

  add = function(x, overwrite = FALSE) {
    if (inherits(x, "RoxyTopic")) {
      self$add(x$sections, overwrite = overwrite)
    } else if (inherits(x, "rd_section")) {
      self$add_section(x, overwrite = overwrite)
    } else if (is.list(x)) {
      for (section in x) {
        self$add_section(section, overwrite = overwrite)
      }
    } else if (is.null(x)) {
      # skip
    } else {
      stop("Don't know how to add object of type ", class(x)[1])
    }
    invisible()
  },

  # Ensures that each type of name (as given by its name), only appears
  # once in self$sections - for internal use only.
  add_section = function(section, overwrite = FALSE) {
    type <- section$type
    if (self$has_section(type) && !overwrite) {
      section <- merge(self$get_section(type), section)
    }

    self$sections[[type]] <- section
    invisible()
  }
))

move_names_to_front <- function(x, to_front) {
  nms <- names(x)
  x[union(intersect(to_front, nms), nms)]
}
