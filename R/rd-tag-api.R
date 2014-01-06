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

  subc <- str_c(tag, "_tag")
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
    values <- str_c("\n", str_c(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }

  str_c("\\", tag, str_c("{", values, "}", collapse = ""), "\n")
}

#' @export
format.rd_tag <- function(x, ...) stop("Unimplemented format")

#' @export
merge.rd_tag <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  new_tag(x$tag, c(x$values, y$values))
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
  values <- str_c(x$values, collapse = "\n\n")
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
format.arguments_tag <- function(x, ...) {
  names <- names(x$values)
  dups <- duplicated(names)

  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  rd_tag("arguments", str_wrap(items, width = 60, exdent = 2, indent = 2),
    space = TRUE)
}

#' @export
format.section_tag <- function(x, ...) {
  names <- vapply(x$values, "[[", "name", FUN.VALUE = character(1))

  contents <- vapply(x$values, "[[", "content", FUN.VALUE = character(1))
  contents <- str_wrap(str_trim(contents), width = 60, exdent = 2, indent = 2)

  setions <- str_c("\\section{", names, "}{\n", contents, "\n}\n",
    collapse = "\n")
}

#' @export
format.slot_tag <- function(x, ...) {
  names <- names(x$values)
  items <- str_c("\\item{\\code{", names, "}}{", x$values, "}", collapse = "\n\n")
  str_c("\\section{Slots}{\n\n",
    "\\describe{\n", 
    items,
    "\n}}\n")
}

#' @export
format.examples_tag <- function(x, ...) {
  values <- str_c(x$values, collapse = "\n")
  rd_tag(x$tag, values, space = TRUE)
}

#' @export
format.rcmethods_tag <- function(x, ...) {
  paste0(
    "\\section{Methods}{\n", 
    "\\itemize{\n", 
    paste0("\\item ", x$values, collapse = "\n\n"),
    "\n}}\n"
  )
}