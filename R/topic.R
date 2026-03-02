#' A `RoxyTopic` is an ordered collection of unique rd_sections
#'
#' @description
#' A `RoxyTopic` object corresponds to a generated `.Rd` file.
#'
#' @param type Section type, a character scalar.
#' @param overwrite Whether to overwrite an existing section. If `FALSE`
#' then the two sections will be merged.
#'
#' @keywords internal

RoxyTopic <- R6::R6Class(
  "RoxyTopic",
  public = list(
    #' @field sections Named list of sections. Each item must be an
    #' [rd_section()] object.
    sections = list(),

    #' @field filename Path to the `.Rd` file to generate.
    filename = "",

    #' @description Format the `.Rd` file. It considers the sections in
    #' particular order, even though Rd tools will reorder them again.
    #'
    #' @param ... Passed to the `format()` methods of the [rd_section()]
    #' objects, the sections.
    #' @return Character string.

    format = function(...) {
      # This has to happen here to get a better order when combining topics
      order <- c(
        "backref",
        "docType",
        "encoding",
        "name",
        "alias",
        "title",
        "format",
        "source",
        "usage",
        "param",
        "value",
        "description",
        "details",
        "minidesc",
        "field",
        "slot",
        "rcmethods",
        "note",
        "section",
        "examples",
        "references",
        "seealso",
        "author",
        "concept",
        "keyword",
        "rawRd"
      )
      sections <- move_names_to_front(self$sections, order)

      formatted <- lapply(sections, format, ...)
      paste0(
        made_by("%"),
        paste0(unlist(formatted), collapse = "\n")
      )
    },

    #' @description Check if an `.Rd` file is valid
    #' @return Logical flag, `TRUE` for valid `.Rd` files

    is_valid = function() {
      # Needs both title and name sections to generate valid Rd
      all(self$has_section(c("title", "name")))
    },

    #' @description Check if an `.Rd` file has a certain section.
    #' @return Logical flag.

    has_section = function(type) {
      type %in% names(self$sections)
    },

    #' @description Query a section.
    #' @return The [rd_section] object representing the section, or `NULL`
    #' if the topic has no such section.

    get_section = function(type) {
      self$sections[[type]]
    },

    #' @description Query the value of a section. This is the value of
    #' the [rd_section] object.
    #' @return Value.

    get_value = function(type) {
      self$get_section(type)$value
    },

    #' @description Get the Rd code of a section.
    #' @return Character vector, one element per line.

    get_rd = function(type) {
      format(self$get_section(type))
    },

    #' @description Get the value of the `name` section. This is the name
    #' of the Rd topic.
    #' @return Character scalar.

    get_name = function() {
      self$get_value("name")[[1]]
    },

    #' @description Query the topics this topic inherits `type` from.
    #' @return A character vector of topic names.

    inherits_from = function(type) {
      if (!self$has_section("inherit")) {
        return(character())
      }

      inherit <- self$get_value("inherit")

      inherits_field <- map_lgl(inherit$fields, \(x) type %in% x)
      sources <- inherit$source[inherits_field]

      if ("NULL" %in% sources) {
        return(character())
      }

      sources
    },

    #' @description Query the topics this topic inherits sections from.
    #' @return A character vector of topic names.

    inherits_section_from = function() {
      if (!self$has_section("inherit_section")) {
        return(character())
      }

      self$get_value("inherit_section")$source
    },

    #' @description Add one or more sections to the topic.
    #' @param x Section(s) to add. It may be
    #' another `RoxyTopic` object, all of its sections will be added;
    #' or an [rd_section] object;
    #' or a list of [rd_section] objects to add.
    #' @param block Name of block to use in error messages.

    add = function(x, block = "???", overwrite = FALSE) {
      if (inherits(x, "RoxyTopic")) {
        self$add(x$sections, block, overwrite = overwrite)
      } else if (inherits(x, "rd_section")) {
        self$add_section(x, block, overwrite = overwrite)
      } else if (is.list(x)) {
        for (section in x) {
          self$add_section(section, block, overwrite = overwrite)
        }
      } else if (is.null(x)) {
        # skip
      } else {
        cli::cli_abort(
          "Don't know how to add object of type {.cls {class(x)[1]}}",
          .internal = TRUE
        )
      }
      invisible()
    },

    #' @description Add a section.
    #' @details
    #' Ensures that each type of name (as given by its name), only appears
    #' once in `self$sections`. This method if for internal use only.
    #' @param section [rd_section] object to add.
    #' @param block Name of block to use in error messages.

    add_section = function(section, block = "???", overwrite = FALSE) {
      if (is.null(section)) {
        return()
      }
      type <- section$type
      if (self$has_section(type) && !overwrite) {
        section <- merge(self$get_section(type), section, block = block)
      }

      self$sections[[type]] <- section
      invisible()
    }
  )
)

move_names_to_front <- function(x, to_front) {
  nms <- names(x)
  x[union(intersect(to_front, nms), nms)]
}
