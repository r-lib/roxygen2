object_defaults <- function(x, block) UseMethod("object_defaults")

#' @export
object_defaults.default <- function(x, block) list()

#' @exportS3Method object_defaults "function"
object_defaults.function <- function(x, block) {
  list(
    roxy_generated_tag(block, "usage", object_usage(x)),
    # Used in process_inherit_params()
    roxy_generated_tag(block, ".formals", names(formals(x$value)))
  )
}

#' @export
object_defaults.s3generic <- object_defaults.function

#' @export
object_defaults.s3method <- object_defaults.function

#' @export
object_defaults.s4generic <- object_defaults.function

#' @export
object_defaults.s4method <- object_defaults.function

#' @export
object_defaults.data <- function(x, block) {
  str_out <- rd(object_format(x$value))

  list(
    roxy_generated_tag(block, "docType", "data"),
    roxy_generated_tag(block, "format", str_out),
    roxy_generated_tag(block, "keywords", "datasets"),
    roxy_generated_tag(block, "usage", object_usage(x))
  )
}

#' @export
object_defaults.package <- function(x, block) {
  desc <- x$value$desc

  description <- as.character(desc$Description)
  logo_path <- file.path(x$value$path, "man", "figures", "logo.png")
  if (file.exists(logo_path)) {
    fig <- "\\if{html}{\\figure{logo.png}{options: align='right' alt='logo' width='120'}}"
    description <- paste0(fig, "\n\n", description)
  }

  list(
    roxy_generated_tag(block, "docType", "package"),
    roxy_generated_tag(block, "name", package_suffix(desc$Package)),
    # "NULL" prevents addition of default aliases, see also #202
    roxy_generated_tag(block, "aliases", paste("NULL", desc$Package, package_suffix(desc$Package))),
    roxy_generated_tag(block, "title", paste0(desc$Package, ": ", desc$Title)),
    roxy_generated_tag(block, "description", description),
    roxy_generated_tag(block, "seealso", package_seealso(desc)),
    roxy_generated_tag(block, "author", package_authors(desc))
  )
}

#' @export
object_defaults.import <- function(x, block) {
  list(
    roxy_generated_tag(block, "docType", "import"),
    roxy_generated_tag(block, "name", "reexports"),
    roxy_generated_tag(block, "keywords", "internal"),
    roxy_generated_tag(block, "title", "Objects exported from other packages"),
    roxy_generated_tag(block, ".reexport", list(pkg = x$value$pkg, fun = x$value$fun))
  )
}

#' @export
object_defaults.s4class <- function(x, block) {
  list(
    roxy_generated_tag(block, "docType", "class")
  )
}

#' @export
object_defaults.rcclass <- function(x, block) {
  list(
    roxy_generated_tag(block, "docType", "class"),
    if (!is.null(x$methods))
      roxy_generated_tag(block, ".methods", x$methods)
  )
}

# Helpers -----------------------------------------------------------------

package_suffix <- function(name) {
  paste0(name, "-package")
}
