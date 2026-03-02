object_defaults <- function(x, block) UseMethod("object_defaults")

#' @export
object_defaults.default <- function(x, block) list()

#' @export
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

  logo_path <- file.path(x$value$path, "man", "figures", "logo.png")
  if (file.exists(logo_path)) {
    fig <- "\\if{html}{\\figure{logo.png}{options: style='float: right' alt='logo' width='120'}}\n\n"
  } else {
    fig <- ""
  }

  name <- desc$get_field("Package")
  title <- desc$get_field("Title")

  description <- desc$get_field("Description")
  description <- package_url_parse(description)
  description <- paste0(fig, description)

  seealso <- package_seealso(
    desc$get_field("URL", NULL),
    desc$get_field("BugReports", NULL)
  )
  authors <- package_authors(desc$get_field("Authors@R", NULL))

  list(
    roxy_generated_tag(block, ".package", name),
    roxy_generated_tag(block, "docType", "package"),
    roxy_generated_tag(block, "name", package_suffix(name)),
    # default aliases are added in topics_add_package_alias()
    roxy_generated_tag(block, "title", paste0(name, ": ", title)),
    roxy_generated_tag(block, "description", description),
    roxy_generated_tag(block, "seealso", seealso),
    roxy_generated_tag(block, "author", authors)
  )
}

#' @export
object_defaults.import <- function(x, block) {
  importFrom <- roxy_generated_tag(
    block,
    "importFrom",
    c(x$value$pkg, x$value$fun)
  )

  if (block_has_tags(block, c("rdname", "name"))) {
    obj <- object_from_name(x$value$fun, asNamespace(x$value$pkg), block)
    return(c(list(importFrom), object_defaults(obj, block)))
  }

  list(
    importFrom,
    roxy_generated_tag(block, "docType", "import"),
    roxy_generated_tag(block, "name", "reexports"),
    roxy_generated_tag(block, "keywords", "internal"),
    roxy_generated_tag(block, "title", "Objects exported from other packages"),
    roxy_generated_tag(
      block,
      ".reexport",
      list(pkg = x$value$pkg, fun = x$value$fun)
    )
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
    if (!is.null(x$methods)) {
      roxy_generated_tag(block, ".methods", x$methods)
    }
  )
}

# Helpers -----------------------------------------------------------------

package_suffix <- function(name) {
  paste0(name, "-package")
}
