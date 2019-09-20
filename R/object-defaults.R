object_defaults <- function(x) UseMethod("object_defaults")

#' @export
object_defaults.default <- function(x) list()

#' @export
object_defaults.data <- function(x) {
  str_out <- rd(object_format(x$value))

  list(
    roxy_tag("docType", NULL, "data"),
    roxy_tag("format", NULL, str_out),
    roxy_tag("keywords", NULL, "datasets")
  )
}

#' @export
object_defaults.package <- function(x) {
  desc <- x$value$desc

  description <- as.character(desc$Description)
  logo_path <- file.path(x$value$path, "man", "figures", "logo.png")
  if (file.exists(logo_path)) {
    fig <- "\\if{html}{\\figure{logo.png}{options: align='right' alt='logo' width='120'}}"
    description <- paste0(fig, "\n\n", description)
  }

  list(
    roxy_tag("docType", NULL, "package"),
    roxy_tag("name", NULL, package_suffix(desc$Package)),
    # "NULL" prevents addition of default aliases, see also #202
    roxy_tag("aliases", NULL, paste("NULL", desc$Package, package_suffix(desc$Package))),
    roxy_tag("title", NULL, paste0(desc$Package, ": ", desc$Title)),
    roxy_tag("description", NULL, description),
    roxy_tag("seealso", NULL, package_seealso(desc)),
    roxy_tag("author", NULL, package_authors(desc))
  )
}

#' @export
object_defaults.import <- function(x) {
  list(
    roxy_tag("docType", NULL, "import"),
    roxy_tag("name", NULL, "reexports"),
    roxy_tag("keywords", NULL, "internal"),
    roxy_tag("title", NULL, "Objects exported from other packages"),
    roxy_tag(".reexport", NULL, roxy_field_reexport(x$value$pkg, x$value$fun))
  )
}

#' @export
object_defaults.s4class <- function(x) {
  list(
    roxy_tag("docType", NULL, "class")
  )
}

#' @export
object_defaults.rcclass <- function(x) {
  list(
    roxy_tag("docType", NULL, "class")
  )
}

#' @export
object_defaults.s4method <- function(x) {
  list(
    roxy_tag("docType", NULL, "class")
  )
}
