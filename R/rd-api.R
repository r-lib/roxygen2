# Translate a tag and expressions into an Rd expression;
# multiple expressions take their own braces.
new_tag <- function(tag, values, subclass) {
  subc <- str_c(tag, "_tag")
  list(structure(list(tag = tag, values = values), class = c(subc, "rd")))
}

#' @S3method print rd_tag
print.rd <- function(x, ...) {
  cat(format(x), "\n")
}

# Translate a tag and expressions into an Rd expression;
# multiple expressions take their own braces.
rd_tag <- function(tag, ..., space = FALSE) {
  if (space) {
    values <- str_c("\n", str_c(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }
  str_c("\\", tag, str_c("{", values, "}", collapse = ""), "\n")                         
}

# c(new_tag("name", "hadley"), new_tag("name", "John"))
# merge(new_tag("name", "hadley"), new_tag("name", "John"))

format.rd <- function(x, ...) stop("Unimplemented format")

#' @S3method merge rd_tag
merge.rd <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))  
  new_tag(x$tag, c(x$values, y$values))
}

# Tags that repeat multiple times --------------------------------------------

#' @S3method format rd_tag
format_rd <- function(x, ...) {
  rd_tag(x$tag, x$values)
}
format.keyword_tag <- format_rd
format.alias_tag <- format_rd

# Tags that keep the first occurence -----------------------------------------
format_first <- function(x, ...) {
  rd_tag(x$tag, x$values[1])
} 
format.name_tag <- format_first
format.title_tag <- format_first
format.docType_tag <- format_first

# Tags collapse their values into a single string ----------------------------

format_collapse <- function(x, ...) {
  values <- str_c(x$values, collapse = "\n\n")
  rd_tag(x$tag, str_wrap(values, width = 60, exdent = 4), space = TRUE)
} 

format.author_tag <- format_collapse
format.seealso_tag <- format_collapse
format.references_tag <- format_collapse
format.examples_tag <- format_collapse
format.value_tag <- format_collapse
format.note_tag <- format_collapse
format.concept_tag <- format_collapse

# Tags that collapse and wrap their input ------------------------------------

format_wrap <- function(x, ...) {
  desc <- str_c(x$values, collapse = "\n\n")
  rd_tag('description', str_wrap(desc, width = 60, exdent = 4), space = TRUE)
}

format.description_tag <- format_collapse
format.details_tag <- format_collapse
format.usage_tag <- format_collapse

# Tags with special errors or other semantics --------------------------------

format.arguments_tag <- function(x, ...) {
  names <- names(x$values)
  dups <- duplicated(names)
  if (any(dups)) {
    warning("Duplicated parameters: ", str_c(names[dups], collapse = ","), 
      call. = FALSE)
  }
  
  items <- str_c("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  rd_tag("arguments", str_wrap(items, width = 60, exdent = 4), space = TRUE)
}

format.section_tag <- function(x, ...) {
  stop("Not implemented")
}
