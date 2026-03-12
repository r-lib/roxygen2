#' Update Collate field in DESCRIPTION
#'
#' @description
#' By default, R loads files in alphabetical order. This is fine if your
#' package doesn't have any cross-file dependencies, but if you're using a tool
#' like S4, you'll need to make sure that classes are loaded before subclasses
#' and generics are defined before methods. You can do this by hand by setting
#' the `Collate` field in the `DESCRIPTION` or automate it by using `@include`
#' tags to specify the cross-file dependencies:
#'
#' ```R
#' #' @include before.R
#' NULL
#' ```
#'
#' If there are no `@include` tags, roxygen2 will leave the `Collate` field as
#' is. This makes it easier to use roxygen2 with an existing collate directive,
#' but if you remove all your `@include` tags, you'll need to also
#' manually delete the collate field.
#'
#' Generally, you should not need to run this function yourself; it will be
#' run automatically by any package that needs to load your R files in
#' collation order.
#'
#' `update_collate()` is not not technically a [roclet], like [rd_roclet()]
#' and [namespace_roclet()], because you have to be able to load the pacakge
#' before you can process it with roclets. However, because it was historical
#' implemented as a roclet, it's still controlled by the `roclets` argument of
#' [roxygenize()].
#'
#' @param base_path Path to package directory.
#' @examples
#' #' If `example-a.R`, `example-b.R` and `example-c.R` live in `R/`
#' #' and we're in `example-a.R`, then the following @include tag
#' #' ensures that example-b and example-c are sourced before example-a.
#' #' @include example-b.R example-c.R
#' NULL
#' @export
#' @aliases @include
update_collate <- function(base_path) {
  check_string(base_path)
  if (!file.exists(base_path)) {
    cli::cli_abort("{.path {base_path}} doesn't exist.")
  }

  new <- generate_collate(file.path(base_path, "R"))
  if (is.null(new)) {
    return(invisible())
  }

  desc_path <- file.path(base_path, "DESCRIPTION")
  old <- desc::desc_get_collate(file = desc_path)

  if (!identical(old, new)) {
    cli::cli_inform("Updating collate directive in {.path {desc_path}}")
    desc::desc_set_collate(new, file = desc_path)
  }

  invisible()
}

generate_collate <- function(base_path) {
  paths <- sort_c(dir(base_path, pattern = "[.][Rr]$", full.names = TRUE))

  includes <- lapply(paths, find_and_filter_includes, paths = basename(paths))
  names(includes) <- paths

  n <- sum(map_int(includes, length))
  if (n == 0) {
    return()
  }

  topo <- TopoSort$new()
  for (path in paths) {
    file <- base_path(path, base_path)

    topo$add(file)
    for (include in includes[[path]]) {
      topo$add_ancestor(file, include)
    }
  }

  topo$sort()
}

find_and_filter_includes <- function(path, paths) {
  includes <- find_includes(path)

  invalid <- setdiff(includes, paths)
  if (length(invalid) > 0) {
    for (include in invalid) {
      path <- cli::style_hyperlink(basename(path), paste0("file://", path))
      cli::cli_inform(c(
        x = "{path}: unknown path in `@include {invalid}`."
      ))
    }
  }

  intersect(includes, paths)
}

base_path <- function(path, base) {
  path <- normalizePath(path, winslash = "/")
  base <- normalizePath(base, winslash = "/")

  str_replace(path, fixed(paste0(base, "/")), "")
}
