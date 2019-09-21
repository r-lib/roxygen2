roxy_field_simple <- function(field, values) {
  if (is.null(values) || identical(values, "NULL")) {
    # NULL is special sentinel value that suppresses output of that field
    return()
  }

  roxy_field(field, values = values)
}

# Low level constructor that doesn't impose any structure on the values
roxy_field <- function(field, ...) {

  structure(
    list(
      field = field,
      ...
    ),
    class = c(paste0("roxy_field_", field), "roxy_field")
  )
}

#' @export
print.roxy_field <- function(x, ...) {
  cat(format(x, wrap = FALSE), "\n")
}

#' @export
format.roxy_field <- function(x, ...) {
  paste0("[ ", x$field, " FIELD ]\n")
}

#' @export
merge.roxy_field <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  roxy_field_simple(x$field, c(x$values, y$values))
}

# Fields that repeat multiple times --------------------------------------------

format_rd <- function(x, ..., sort = TRUE) {
  x$values <- unique(x$values)
  if (sort) {
    x$values <- sort_c(x$values)
  }

  map_chr(x$values, rd_macro, field = x$field)
}
#' @export
format.roxy_field_keyword <- format_rd
#' @export
format.roxy_field_alias <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_rd(x, ..., sort = FALSE)
}
#' @export
format.roxy_field_concept <- format_rd

# Fields that keep the first occurrence -----------------------------------------
format_first <- function(x, ...) {
  rd_macro(x$field, x$values[1])
}
#' @export
format.roxy_field_name <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_first(x, ...)
}
#' @export
format.roxy_field_title <- format_first
#' @export
format.roxy_field_docType <- format_first
#' @export
format.roxy_field_format <- format_first
#' @export
format.roxy_field_encoding <- format_first

# Fields collapse their values into a single string ----------------------------

format_collapse <- function(x, ..., indent = 0, exdent = 0, wrap = TRUE) {
  values <- paste0(x$values, collapse = "\n\n")
  if (wrap) {
    values <- str_wrap(values, width = 60, indent = indent, exdent = exdent)
  }
  rd_macro(x$field, values, space = TRUE)
}
#' @export
format.roxy_field_author <- format_collapse
#' @export
format.roxy_field_description <- format_collapse
#' @export
format.roxy_field_details <- format_collapse
#' @export
format.roxy_field_note <- format_collapse
#' @export
format.roxy_field_references <- format_collapse
#' @export
format.roxy_field_seealso <- format_collapse
#' @export
format.roxy_field_source <- format_collapse
#' @export
format.roxy_field_value <- format_collapse

# Fields that don't have output ------------------------------------------------

#' @export
format.roxy_field_family <- function(x, ...) NULL
#' @export
format.roxy_field_formals <- function(x, ...) NULL

# Fields with special errors or other semantics --------------------------------

#' @export
format.roxy_field_usage <- function(x, ...) {
  rd_macro(x$field, build_rd(x$values, collapse = "\n\n"), space = TRUE)
}


#' @export
format.roxy_field_slot <- function(x, ...) {
  describe_section("Slots", names(x$values), x$values)
}

#' @export
format.roxy_field_field <- function(x, ...) {
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
format.roxy_field_rcmethods <- function(x, ...) {
  describe_section("Methods", names(x$values), x$values)
}

#' @export
format.roxy_field_rawRd <- function(x, ...) {
  paste(x$values, collapse = "\n")
}

# Markdown ----------------------------------------------------------------

roxy_field_markdown <- function(name, values) {
  # Any additional components are sections
  if (length(values) > 1) {
    name <- c(name, rep("rawRd", length(values) - 1))

    if (values[[1]] == "") {
      name <- name[-1]
      values <- values[-1]
    }
  }

  map2(name, values, roxy_field_simple)
}
