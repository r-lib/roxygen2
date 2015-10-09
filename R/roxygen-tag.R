
# roxygen_tag datastructure -----------------------------------------------

roxygen_tag <- function(tag, val, file = "", line = 0) {
  structure(
    list(
      file = file,
      line = line,
      tag = tag,
      val = val
    ),
    class = "roxygen_tag"
  )
}

is.roxygen_tag <- function(x) inherits(x, "roxygen_tag")

#' @export
print.roxygen_tag <- function(x, ...) {
  cat("[", x$file, ":", x$line, "] @", x$tag, " ", encodeString(x$val), "\n",
    sep = "")
}

make_tag_message <- function(x, message) {
  paste0(
    "@",
    x$tag,
    if (x$file != "") paste0(" [", x$file, "#", x$line, "]"),
    ": ",
    message
  )
}

tag_warning <- function(x, ...) {
  warning(make_tag_message(x, paste0(...)), call. = FALSE)
  NULL
}
