rd_section <- function(type, value) {
  if (is.null(value) || identical(value, "NULL")) {
    # NULL is special sentinel value that suppresses output of that field
    return()
  }

  structure(
    list(
      type = type,
      value = value
    ),
    class = c(paste0("rd_section_", type), "rd_section")
  )
}

#' @export
print.rd_section <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
format.rd_section <- function(x, ...) {
  abort(paste0("`format.", class(x)[[1]], "` not found"))
}

#' @export
merge.rd_section <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  rd_section(x$type, c(x$value, y$value))
}

format_rd <- function(x, ..., sort = TRUE) {
  # One rd macro for each value
  x$value <- unique(x$value)
  if (sort) {
    x$value <- sort_c(x$value)
  }

  map_chr(x$value, rd_macro, field = x$type)
}

format_first <- function(x, ...) {
  # Only use the first value
  rd_macro(x$type, x$value[1])
}

format_collapse <- function(x, ..., indent = 0, exdent = 0) {
  # Collapse all into a single string
  value <- paste0(x$value, collapse = "\n\n")
  rd_macro(x$type, value, space = TRUE)
}

rd_section_description <- function(name, dt, dd) {
  if (length(dt) == 0) return("")

  items <- paste0("\\item{\\code{", dt, "}}{", dd, "}", collapse = "\n\n")
  paste0("\\section{", name, "}{\n\n",
    "\\describe{\n",
    items,
    "\n}}\n"
  )
}

