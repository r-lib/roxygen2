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
  str_out <- paste0(capture.output(str(x$value)), collapse = "\n")
  str_pre <- build_rd("\\preformatted{", escape_preformatted(str_out), "\n}")

  list(
    docType = "data",
    format = str_pre,
    keywords = "datasets"
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
