# Translate a tag and expressions into an Rd expression;
# multiple expressions take their own braces.
#
# Tags have two methods: \code{merge} and \code{format}.  Currently for all
# tags, merge just combines all values, and format selects from these to
# display the tags in the appropriate way.
#
new_tag <- function(tag, values) {
  if (is.null(values)) return()
  # NULL is special sentinel value that suppresses output of that tag
  if (identical(values, "NULL")) return()

  subc <- paste0(tag, "_tag")
  list(structure(list(tag = tag, values = values), class = c(subc, "rd_tag")))
}

is.rd_tag <- function(x) inherits(x, "rd_tag")

#' @export
print.rd_tag <- function(x, ...) {
  cat(format(x), "\n")
}

# Translate a tag and values into an Rd expression; multiple values get their
# own braces.
rd_tag <- function(tag, ..., space = FALSE) {
  if (space) {
    values <- paste0("\n", paste0(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }

  paste0("\\", tag, paste0("{", values, "}", collapse = ""), "\n")
}

#' @export
format.rd_tag <- function(x, ...) stop("Unimplemented format")

#' @export
merge.rd_tag <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  new_tag(x$tag, c(x$values, y$values))
}

#' @export
merge.minidesc_tag <- function(x, y, ...) {
  if (x$values$type != y$values$type) {
    stop("Can't merge @minidesc of different types", call. = FALSE)
  }

  x$values$desc <- c(x$values$desc, y$values$desc)
  x$values$label <- c(x$values$label, y$values$label)
  list(x)
}

#' @export
merge.section_tag <- function(x, y, ...) {
  x_names <- vapply(x$values, `[[`, "name", FUN.VALUE = character(1L))
  y_names <- vapply(y$values, `[[`, "name", FUN.VALUE = character(1L))
  xy_names <- unique(c(x_names, y_names))
  xy_both_names <- intersect(x_names, y_names)
  x_contents <- setNames(lapply(x$values, `[[`, "content"), x_names)
  y_contents <- setNames(lapply(y$values, `[[`, "content"), y_names)
  values <- lapply(
    xy_names,
    function (name) {
      if (name %in% xy_both_names) {
        content <- paste(x_contents[[name]], y_contents[[name]], sep = "\n\n")
      } else {
        content <- x_contents[[name]] %||% y_contents[[name]]
      }

      list(name = name, content = content)
    }
  )
  new_tag("section", values)
}

#' @export
merge.reexport_tag <- function(x, y, ...) {
  values <- list(
    pkg = c(x$values$pkg, y$values$pkg),
    fun = c(x$values$fun, y$values$fun)
  )
  new_tag("reexport", values)
}


# Comment tags -----------------------------------------------------------------------

#' @export
format.backref_tag <- function(x, ...) {
  filename <- unique(x$values)
  filename <- file.path(basename(dirname(filename)), basename(filename), fsep = "/")
  sprintf("%% Please edit documentation in %s\n", paste(filename, collapse = ", "))
}

# Tags that repeat multiple times --------------------------------------------

format_rd <- function(x, ...) {
  vapply(sort_c(unique(x$values)), rd_tag, tag = x$tag,
    FUN.VALUE = character(1), USE.NAMES = FALSE)
}
#' @export
format.keyword_tag <- format_rd
#' @export
format.alias_tag <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_rd(x, ...)
}

# Tags that keep the first occurence -----------------------------------------
format_first <- function(x, ...) {
  rd_tag(x$tag, x$values[1])
}
#' @export
format.name_tag <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_first(x, ...)
}
#' @export
format.title_tag <- format_first
#' @export
format.docType_tag <- format_first
#' @export
format.format_tag <- format_first
#' @export
format.encoding_tag <- format_first

# Tags collapse their values into a single string ----------------------------

format_collapse <- function(x, ..., indent = 0, exdent = 0, wrap = TRUE) {
  values <- paste0(x$values, collapse = "\n\n")
  if (wrap) {
    values <- str_wrap(values, width = 60, indent = indent, exdent = exdent)
  }
  rd_tag(x$tag, values, space = TRUE)
}
#' @export
format.author_tag <- format_collapse
#' @export
format.concept_tag <- format_collapse
#' @export
format.description_tag <- format_collapse
#' @export
format.details_tag <- format_collapse
#' @export
format.note_tag <- format_collapse
#' @export
format.references_tag <- format_collapse
#' @export
format.seealso_tag <- format_collapse
#' @export
format.source_tag <- format_collapse
#' @export
format.value_tag <- format_collapse

# Tags that don't have output ------------------------------------------------

format_null <- function(x, ...) NULL

#' @export
format.family_tag <- format_null
#' @export
format.inheritParams_tag <- format_null
#' @export
format.formals_tag <- format_null

# Tags with special errors or other semantics --------------------------------

#' @export
format.usage_tag <- function(x, ...) {
  rd_tag(x$tag, build_rd(x$values, collapse = "\n\n"), space = TRUE)
}

#' @export
format.param_tag <- function(x, ..., wrap = TRUE) {
  names <- names(x$values)

  # add space to multiple arguments so they can wrap
  names <- gsub(",", ", ", names)

  items <- paste0("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  if (wrap) {
    items <- str_wrap(items, width = 60, exdent = 2, indent = 2)
  }

  rd_tag("arguments", items, space = TRUE)
}

#' @export
format.section_tag <- function(x, ..., wrap = TRUE) {
  names <- vapply(x$values, "[[", "name", FUN.VALUE = character(1))

  contents <- vapply(x$values, "[[", "content", FUN.VALUE = character(1))
  if (wrap) {
    contents <- str_wrap(str_trim(contents), width = 60, exdent = 2, indent = 2)
  }

  setions <- paste0("\\section{", names, "}{\n", contents, "\n}\n",
    collapse = "\n")
}

#' @export
format.slot_tag <- function(x, ...) {
  describe_section("Slots", names(x$values), x$values)
}

#' @export
format.field_tag <- function(x, ...) {
  describe_section("Fields", names(x$values), x$values)
}

describe_section <- function(name, dt, dd) {
  if (length(dt) == 0) return("")

  items <- paste0("\\item{\\code{", dt, "}}{", dd, "}", collapse = "\n\n")
  paste0("\\section{", name, "}{\n\n",
    "\\describe{\n",
    items,
    "\n}}\n"
  )
}



#' @export
format.examples_tag <- function(x, ...) {
  values <- paste0(x$values, collapse = "\n")
  rd_tag(x$tag, values, space = TRUE)
}

#' @export
format.rcmethods_tag <- function(x, ...) {
  describe_section("Methods", names(x$values), x$values)
}

#' @export
format.minidesc_tag <- function(x, ...) {
  title <- switch(x$values$type,
    generic = "Methods (by class)",
    class = "Methods (by generic)",
    "function" = "Functions"
  )

  paste0(
    "\\section{", title, "}{\n",
    "\\itemize{\n",
    paste0("\\item \\code{", x$values$label, "}: ", x$values$desc,
      collapse = "\n\n"),
    "\n}}\n"
  )
}

#' @export
format.rawRd_tag <- function(x, ...) {
  paste(x$values, collapse = "\n")
}


#' @export
format.reexport_tag <- function(x, ...) {
  pkgs <- split(x$values$fun, x$values$pkg)
  pkg_links <- Map(pkg = names(pkgs), funs = pkgs, function(pkg, funs) {
    links <- paste0("\\code{\\link[", pkg, "]{", escape(funs), "}}",
      collapse = ", ")
    paste0("\\item{", pkg, "}{", links, "}")
  })

  paste0(
    "\\description{\n",
    "These objects are imported from other packages. Follow the links\n",
    "below to see their documentation.\n",
    "\n",
    "\\describe{\n",
    paste0("  ", unlist(pkg_links), collapse = "\n\n"),
    "\n}}\n"
  )

}
