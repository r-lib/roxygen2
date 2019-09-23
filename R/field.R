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
  cat(format(x), "\n")
}

#' @export
format.roxy_field <- function(x, ...) {
  abort(paste0("`format.", class(x)[[1]], "` not found"))
}

#' @export
merge.roxy_field <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  roxy_field(x$field, c(x$value, y$value))
}

format_rd <- function(x, ..., sort = TRUE) {
  # One rd macro for each value
  x$value <- unique(x$value)
  if (sort) {
    x$value <- sort_c(x$value)
  }

  map_chr(x$value, rd_macro, field = x$field)
}

format_first <- function(x, ...) {
  # Only use the first value
  rd_macro(x$field, x$value[1])
}

format_collapse <- function(x, ..., indent = 0, exdent = 0) {
  # Collapse all into a single string
  value <- paste0(x$value, collapse = "\n\n")
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

