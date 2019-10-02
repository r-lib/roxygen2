#' `roxy_tag` S3 constructor
#'
#' `roxy_tag()` is the constructor for tag objects.
#' `roxy_tag_warning()` generates a warning that gives the location of the tag.
#'
#' @section Methods:
#' Define a method for `roxy_tag_parse` to support new tags. See [tag_parsers]
#' for more details.
#'
#' @keywords internal
#' @export
#' @param tag Tag name. Arguments starting with `.` are reserved for internal
#'   usage.
#' @param raw Raw tag value, a string.
#' @param val Parsed tag value, typically a character vector, but sometimes
#'   a list. Usually filled in by `tag_parsers`
#' @param file,line Location of the tag
roxy_tag <- function(tag, raw, val = NULL, file = NA_character_, line = NA_integer_) {
  structure(
    list(
      file = file,
      line = line,
      raw = raw,
      tag = tag,
      val = val
    ),
    class = c(paste0("roxy_tag_", tag), "roxy_tag")
  )
}

#' @rdname roxy_tag
#' @param x A tag
#' @export
roxy_tag_parse <- function(x) {
  UseMethod("roxy_tag_parse")
}

#' @export
roxy_tag_parse.default <- function(x) {
  roxy_tag_warning(x, "unknown tag")
}

is.roxy_tag <- function(x) inherits(x, "roxy_tag")

#' @export
format.roxy_tag <- function(x, ..., file = NULL) {
  if (identical(x$file, file)) {
    file <- "line"
  } else if (is.na(x$file)) {
    file <- "????"
  } else {
    file <- basename(x$file)
  }
  line <- if (is.na(x$line)) "???" else format(x$line, width = 3)

  loc <- paste0("[", file, ":", line, "]")

  if (!is.null(x$raw)) {
    lines <- strsplit(x$raw, "\n")[[1]]
    ellipsis <- FALSE

    if (length(lines) > 1) {
      raw <- lines[[1]]
      ellipsis <- TRUE
    } else {
      raw <- x$raw
    }

    if (nchar(raw) > 50) {
      raw <- substr(raw, 1, 47)
      ellipsis <- TRUE
    }

    raw <- paste0(raw, if (ellipsis) "...")
  } else {
    raw <- "<generated>"
  }

  parsed <- if (is.null(x$val)) "{unparsed}" else "{parsed}"

  paste0(loc, " @", x$tag, " '", raw, "' ", parsed)
}

#' @export
print.roxy_tag <- function(x, ...) {
  cat_line(format(x, ...))
}

#' @export
#' @rdname roxy_tag
roxy_tag_warning <- function(x, ...) {
  roxy_warning(file = x$file, line = x$line, "@", x$tag, " ", ...)
}
