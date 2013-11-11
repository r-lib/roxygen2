#' @include parse-registry.R
NULL

register.preref.parsers(parse.value, 'include')

#' Roclet: make Collate field in DESCRIPTION.
#'
#' Topologically sort R files and record in Collate field.
#'
#' Each \code{@@include} tag should specify the filename of one intrapackage
#' dependency; multiple \code{@@include} tags may be given.
#'
#' @family roclets
#' @return Rd roclet
#' @examples
#' #' `example-a.R', `example-b.R' and `example-c.R' reside
#' #' in the `example' directory, with dependencies
#' #' a -> {b, c}. This is `example-a.R'.
#' #' @@include example-b.R
#' #' @@include example-c.R
#' NULL
#'
#' roclet <- collate_roclet()
#' \dontrun{
#'   roc_proc(roclet, dir('example'))
#'   roc_out(roclet, dir('example'), "example")
#' }
#' @export

update_collate <- function(base_path) {
  collate <- generate_collate(file.path(base_path, "R"))
  if (!is.null(collate)) {
    collate <- paste0("'", collate, "'", collapse = " ")
  }

  desc_path <- file.path(base_path, "DESCRIPTION")
  old <- read.description(desc_path)

  new <- old
  new$Collate <- collate
  write.description(new, desc_path)

  if (!identical(old, read.description(desc_path))) {
    cat('Updating collate directive in ', desc_path, "\n")
  }
}

generate_collate <- function(base_path) {
  paths <- sort_c(dir(base_path, "[.Rr]$", full.names = TRUE))
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

find_includes <- function(path) {
  lines <- readLines(path, warn = FALSE)
  re <- regexec("^\\s*#+' @include (.*)$", lines)
  matches <- regmatches(lines, re)
  matches <- Filter(function(x) length(x) == 2, matches)
  
  if (length(matches) == 0) return()

  includes <- vapply(matches, "[[", 2, FUN.VALUE = character(1))
  sort_c(unlist(strsplit(includes, " ", fixed = TRUE)))
}

base_path <- function(path, base) {
  path <- normalizePath(path, winslash = "/")
  base <- normalizePath(base, winslash = "/")

  str_replace(path, fixed(str_c(base, "/")), "")
}
