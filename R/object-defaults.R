# Combine a block with defaults generated from the object
add_defaults <- function(block) {
  defaults <- object_defaults(block$object)

  for (tag in names(defaults)) {
    if (tag %in% names(block)) next

    block[[tag]] <- defaults[[tag]]
  }

  block
}

object_defaults <- function(x) UseMethod("object_defaults")

#' @export
object_defaults.default <- function(x) list()

#' @export
object_defaults.data <- function(x) {
  str_out <- rd(object_format(x$value))

  list(
    docType = "data",
    format = str_out,
    keywords = "datasets"
  )
}

#' @export
object_defaults.import <- function(x) {
  list(
    docType = "import",
    name = "reexports",
    keywords = "internal",
    title = "Objects exported from other packages",
    .reexport = roxy_field_reexport(x$value$pkg, x$value$fun)
  )
}

#' @export
object_defaults.package <- function(x) {
  desc <- x$value$desc

  list(
    docType = "package",
    name = package_suffix(desc$Package),
    # "NULL" prevents addition of default aliases, see also #202
    aliases = paste("NULL", desc$Package, package_suffix(desc$Package)),
    title = paste0(desc$Package, ": ", desc$Title),
    description = as.character(desc$Description),
    seealso = package_seealso(desc),
    author = package_authors(desc)
  )
}

#' @export
object_defaults.s4class <- function(x) {
  list(
    docType = "class"
  )
}

#' @export
object_defaults.rcclass <- function(x) {
  list(
    docType = "class"
  )
}

#' @export
object_defaults.s4method <- function(x) {
  list(
    docType = "methods"
  )
}
