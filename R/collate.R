#' Update Collate field in DESCRIPTION
#'
#' @description
#' By default, R loads files in alphabetical order. Unfortunately not every
#' alphabet puts letters in the same order, so you can't rely on alphabetic
#' ordering if you need one file loaded before another. (This usually doesn't
#' matter but is important for S4, where you need to make sure that classes are
#' loaded before subclasses and generics are defined before methods.).
#' You can override the default alphabetical ordering with `@include before.R`,
#' which specify that `before.R` must be loaded before the current file.
#'
#' Generally, you will not need to run this function yourself; it should be
#' run automatically by any package that needs to load your R files in
#' collation order.
#'
#' @section Collate:
#' This is not a roclet because roclets need the values of objects in a package,
#' and those values can not be generated unless you've sourced the files,
#' and you can't source the files unless you know the correct order.
#'
#' If there are no `@include` tags, roxygen2 will leave collate as is.
#' This makes it easier to use roxygen2 with an existing collate directive,
#' but if you remove all your `@include` tags, you'll need to also
#' manually delete the collate field.
#'
#' @param base_path Path to package directory.
#' @examples
#' #' If `example-a.R', `example-b.R' and `example-c.R' live in R/
#' #' and we're in `example-a.R`, then the following @@include statement
#' #' ensures that example-b and example-c are sourced before example-a.
#' #' @@include example-b.R example-c.R
#' NULL
#' @export
#' @aliases @@include
update_collate <- function(base_path) {
  if (!file.exists(base_path)) {
    cli::cli_abort("{.path {base_path}} doesn't exist")
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
