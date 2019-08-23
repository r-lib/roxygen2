#' Constructors for S3 object to represent R objects.
#'
#' These objects are usually created by the parsers, but it is also
#' useful to generate them by hand for testing.
#'
#' @param value The object itself.
#' @param alias Alias for object being documented, in case you create a
#'   generator function with different name.
#' @export
#' @keywords internal
object <- function(value, alias, type) {
  structure(
    list(
      alias = alias,
      value = value,
      methods = if (type == "rcclass") rc_methods(value),
      topic = object_topic(value, alias, type)
    ),
    class = c(type, "object")
  )
}

#' @export
print.object <- function(x, ...) {
  cat("<", class(x)[1], "> ", x$name,
    if (!is.null(x$alias)) paste0(" (", x$alias, ")"), "\n",
    sep = ""
  )
}

object_topic <- function(value, alias, type) {
  switch(type,
    s4method = paste0(value@generic, ",", paste0(value@defined, collapse = ","), "-method"),
    s4class = paste0(value@className, "-class"),
    s4generic = value@generic,
    rcclass = paste0(value@className, "-class"),
    rcmethod = value@name,
    s3generic = alias,
    s3method = alias,
    import = alias,
    `function` = alias,
    package = alias,
    data = alias,
    stop("Unsupported type '", type, "'", call. = FALSE)
  )
}
