# Translate a tag and expressions into an Rd expression;
# multiple expressions take their own braces.
#
# Tags have two methods: \code{merge} and \code{format}.  Currently for all
# tags, merge just combines all values, and format selects from these to 
# display the tags in the appropriate way. 
#
new_tag <- function(tag, values) {
  if (is.null(values)) return()
  
  subc <- str_c(tag, "_tag")
  list(structure(list(tag = tag, values = values), class = c(subc, "rd_tag")))
}

is.rd_tag <- function(x) inherits(x, "rd_tag")

#' @S3method print rd_tag
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
  # Turn non-breaking spaces back into regular spaces
  values <- str_replace_all(values, fixed("\u{A0}"), " ")
  str_c("\\", tag, str_c("{", values, "}", collapse = ""), "\n")                         
}

#' @S3method format rd_tag
format.rd_tag <- function(x, ...) stop("Unimplemented format")

#' @S3method merge rd_tag
merge.rd_tag <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))  
  new_tag(x$tag, c(x$values, y$values))
}

# Tags that repeat multiple times --------------------------------------------

#' @S3method format keyword_tag
#' @S3method format alias_tag
format_rd <- function(x, ...) {
  vapply(sort(unique(x$values)), rd_tag, tag = x$tag, 
    FUN.VALUE = character(1), USE.NAMES = FALSE)
}
format.keyword_tag <- format_rd
format.alias_tag <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_rd(x, ...)
}

# Tags that keep the first occurence -----------------------------------------
format_first <- function(x, ...) {
  rd_tag(x$tag, x$values[1])
} 
#' @S3method format name_tag
#' @S3method format title_tag
#' @S3method format docType_tag
#' @S3method format format_tag
#' @S3method format encoding_tag
format.name_tag <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_first(x, ...)
}
format.title_tag <- format_first
format.docType_tag <- format_first
format.format_tag <- format_first
format.encoding_tag <- format_first

# Tags collapse their values into a single string ----------------------------

format_collapse <- function(x, ..., indent = 2, exdent = 2) {
  values <- str_c(x$values, collapse = "\n\n")
  rd_tag(x$tag, str_wrap(values, width = 60, indent = indent, 
    exdent = exdent), space = TRUE)
} 
#' @S3method format author_tag
#' @S3method format concept_tag
#' @S3method format description_tag
#' @S3method format details_tag
#' @S3method format note_tag
#' @S3method format references_tag
#' @S3method format seealso_tag
#' @S3method format source_tag
#' @S3method format usage_tag
#' @S3method format value_tag
format.author_tag <- format_collapse
format.concept_tag <- format_collapse
format.description_tag <- format_collapse
format.details_tag <- format_collapse
format.note_tag <- format_collapse
format.references_tag <- format_collapse
format.seealso_tag <- format_collapse
format.source_tag <- format_collapse
format.usage_tag <- function(x, ...) format_collapse(x, ..., exdent = 4)
format.value_tag <- format_collapse


# Tags that don't have output ------------------------------------------------

format_null <- function(x, ...) NULL

#' @S3method format family_tag
format.family_tag <- format_null
format.inheritParams_tag <- format_null
format.formals_tag <- format_null

# Tags with special errors or other semantics --------------------------------

#' @S3method format arguments_tag
format.arguments_tag <- function(x, ...) {
  names <- names(x$values)
  dups <- duplicated(names)
  
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  rd_tag("arguments", str_wrap(items, width = 60, exdent = 2, indent = 2),
    space = TRUE)
}

#' @S3method format slot_tag
format.slot_tag <- function(x, ...) {
  names <- names(x$values)
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  str_c("\\section{Slots}\n\n",
    "\\itemize{\n", 
    str_wrap(items, width = 60, exdent = 2, indent = 2),
    "\n}\n")
}


#' @S3method format section_tag
format.section_tag <- function(x, ...) {
  names <- vapply(x$values, "[[", "name", FUN.VALUE = character(1))

  contents <- vapply(x$values, "[[", "content", FUN.VALUE = character(1))
  contents <- str_wrap(str_trim(contents), width = 60, exdent = 2, indent = 2)
  
  setions <- str_c("\\section{", names, "}{\n", contents, "\n}\n", 
    collapse = "\n")
}

#' @S3method format examples_tag
format.examples_tag <- function(x, ...) {
  values <- str_c(x$values, collapse = "\n")
  rd_tag(x$tag, values, space = TRUE)  
}
