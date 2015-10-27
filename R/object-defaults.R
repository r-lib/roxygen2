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
  str_out <- rd(default_data_format(x$value))

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
    reexport = list(pkg = x$value$pkg, fun = x$value$fun)
  )
}

#' @export
object_defaults.package <- function(x) {
  desc <- x$value$desc
  list(
    docType = "package",
    title = as.character(desc$Title),
    description = as.character(desc$Description),
    # "NULL" prevents addition of default aliases, see also #202
    aliases = paste("NULL", desc$Package, package_suffix(desc$Package)),
    name = package_suffix(desc$Package)
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
  generic <- x$value@generic
  pkg <- attr(generic, "package")

  list(
    docType = "methods"
  )
}

inherit_from <- function(generic, pkg) {
  if (pkg == "roxygen_devtest") return(generic)

  # Check that Rd file available
  rd <- get_rd(generic, pkg)
  if (is.null(rd)) return(NULL)

  if (!is.syntactic(generic)) {
    generic <- paste0("`", generic, "`")
  }

  paste0(pkg, "::", generic)
}
