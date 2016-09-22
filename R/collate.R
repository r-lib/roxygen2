#' Update Collate field in DESCRIPTION.
#'
#' Topologically sort R files and record in Collate field. The topological
#' sort is based on the `@include` tag, which should specify the filenames
#' (space separated) that should be loaded before the current file. These are
#' typically necessary if you're using S4 or RC classes (because super classes
#' must be defined before subclasses).
#'
#' If there are no `@include` tags, roxygen2 will leave collate as is.
#' This makes it easier to use roxygen2 with an existing collate directive,
#' but if you remove all your `@include` tags, you'll need to also
#' manually delete the collate field.
#'
#' This is not a roclet because roclets need the values of objects in a package,
#' and those values can not be generated unless you've sourced the files,
#' and you can't source the files unless you know the correct order.
#'
#' @param base_path Path to package directory.
#' @examples
#' #' `example-a.R', `example-b.R' and `example-c.R' reside
#' #' in the `example' directory, with dependencies
#' #' a -> {b, c}. This is `example-a.R'.
#' #' @@include example-b.R
#' #' @@include example-c.R
#' NULL
#'
#' \dontrun{
#'   update_collate("my_package")
#' }
#' @export
update_collate <- function(base_path) {
  new <- generate_collate(file.path(base_path, "R"))
  if (is.null(new)) return()

  desc_path <- file.path(base_path, "DESCRIPTION")
  old <- desc::desc_get_collate(file = desc_path)

  if (!identical(old, new)) {
    cat('Updating collate directive in ', desc_path, "\n")
    desc::desc_set_collate(new, file = desc_path)
  }

  invisible()
}

generate_collate <- function(base_path) {
  paths <- sort_c(dir(base_path, pattern = "[.][Rr]$", full.names = TRUE))
  includes <- lapply(paths, find_includes)
  names(includes) <- paths

  n <- sum(vapply(includes, length, integer(1)))
  if (n == 0) return()

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

base_path <- function(path, base) {
  path <- normalizePath(path, winslash = "/")
  base <- normalizePath(base, winslash = "/")

  str_replace(path, fixed(paste0(base, "/")), "")
}
