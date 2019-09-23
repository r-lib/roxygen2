roxy_field <- function(field, value) {
  if (is.null(value) || identical(value, "NULL")) {
    # NULL is special sentinel value that suppresses output of that field
    return()
  }

  structure(
    list(
      field = field,
      value = value
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
  roxy_field(x$field, c(x$value, y$value))
}

# Fields that repeat multiple times --------------------------------------------

format_rd <- function(x, ..., sort = TRUE) {
  x$value <- unique(x$value)
  if (sort) {
    x$value <- sort_c(x$value)
  }

  map_chr(x$value, rd_macro, field = x$field)
}
#' @export
format.roxy_field_keyword <- format_rd
#' @export
format.roxy_field_alias <- function(x, ...) {
  x$value <- str_replace_all(x$value, fixed("%"), "\\%")
  format_rd(x, ..., sort = FALSE)
}
#' @export
format.roxy_field_concept <- format_rd

# Fields that keep the first occurrence -----------------------------------------
format_first <- function(x, ...) {
  rd_macro(x$field, x$value[1])
}
#' @export
format.roxy_field_name <- function(x, ...) {
  x$value <- str_replace_all(x$value, fixed("%"), "\\%")
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

# Fields collapse their value into a single string ----------------------------

format_collapse <- function(x, ..., indent = 0, exdent = 0, wrap = TRUE) {
  value <- paste0(x$value, collapse = "\n\n")
  if (wrap) {
    value <- str_wrap(value, width = 60, indent = indent, exdent = exdent)
  }
  rd_macro(x$field, value, space = TRUE)
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
  rd_macro(x$field, build_rd(x$value, collapse = "\n\n"), space = TRUE)
}


#' @export
format.roxy_field_slot <- function(x, ...) {
  describe_section("Slots", names(x$value), x$value)
}

#' @export
format.roxy_field_field <- function(x, ...) {
  describe_section("Fields", names(x$value), x$value)
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
  describe_section("Methods", names(x$value), x$value)
}

#' @export
format.roxy_field_rawRd <- function(x, ...) {
  paste(x$value, collapse = "\n")
}

# Markdown ----------------------------------------------------------------

roxy_field_markdown <- function(name, value) {
  # Any additional components are sections
  if (length(value) > 1) {
    name <- c(name, rep("rawRd", length(value) - 1))

    if (value[[1]] == "") {
      name <- name[-1]
      value <- value[-1]
    }
  }

  map2(name, value, roxy_field)
}
