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

# Fields that keep the first occurrence -----------------------------------------
format_first <- function(x, ...) {
  rd_macro(x$field, x$value[1])
}
#' @export
format.roxy_field_name <- function(x, ...) {
  x$value <- str_replace_all(x$value, fixed("%"), "\\%")
  format_first(x, ...)
}

format_collapse <- function(x, ..., indent = 0, exdent = 0, wrap = TRUE) {
  value <- paste0(x$value, collapse = "\n\n")
  if (wrap) {
    value <- str_wrap(value, width = 60, indent = indent, exdent = exdent)
  }
  rd_macro(x$field, value, space = TRUE)
}

roxy_field_description <- function(name, dt, dd) {
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
  roxy_field_description("Methods", names(x$value), x$value)
}

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
