#' @include tag-registry.R
NULL

# Needed to silence unknown tag warnings
register_tags(
  include = parse.value
)

#' Update Collate field in DESCRIPTION.
#'
#' Topologically sort R files and record in Collate field. The topological
#' sort is based on the \code{@@include} tag, which should specify the filenames
#' (space separated) that should be loaded before the current file - these are
#' typically necessary if you're using S4 or RC classes (because super classes
#' must be defined before subclasses).
#'
#' If there are no \code{@@include} tags, roxygen2 will leave collate as is.
#' This makes it easier to use roxygen2 with an existing collate directive,
#' but if you remove all your \code{@@include} tags, you'll need to also
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
  collate <- generate_collate(file.path(base_path, "R"))
  if (is.null(collate)) return()

  desc_path <- file.path(base_path, "DESCRIPTION")
  old <- read.description(desc_path)

  new <- old
  new$Collate <- paste0("'", collate, "'", collapse = "\n")

  if (!identical(old, new)) {
    cat('Updating collate directive in ', desc_path, "\n")
    write.description(new, desc_path)
  }
}

generate_collate <- function(base_path) {
  paths <- sort_c(dir(base_path, pattern = "[.][Rr]$", full.names = TRUE))
  includes <- lapply(paths, find_includes)
  names(includes) <- paths

  n <- sum(vapply(includes, length, integer(1)))
  if (n == 0) return()

  topo <- topo_sort()
  for (path in paths) {
    file <- base_path(path, base_path)
    vertex <- topo$add(file)

    for (include in includes[[path]]) {
      topo$add_ancestor(vertex, include)
    }
  }

  unique(topo$sort())
}

base_path <- function(path, base) {
  path <- normalizePath(path, winslash = "/")
  base <- normalizePath(base, winslash = "/")

  str_replace(path, fixed(paste0(base, "/")), "")
}
