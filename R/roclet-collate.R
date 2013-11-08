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
collate_roclet <- function() {
  new_roclet(list(), "collate")
}

#' @export
roc_process.collate <- function(roclet, partita, base_path) {
  topo <- topo_sort()

  for (partitum in partita) {
    file <- base_path(partitum$srcref$filename, base_path)
    vertex <- topo$add(file)

    includes <- partitum[names(partitum) == "include"]
    if (length(includes) > 0) {
      for (include in includes) {
        topo$add_ancestor(vertex, include)
      }
    }
  }

  unique(basename(topo$sort()))
}

#' @export
roc_output.collate <- function(roclet, results, base_path) {
  DESCRIPTION <- file.path(base_path, "DESCRIPTION")
  old <- read.description(DESCRIPTION)
  new <- old
  new$Collate <- str_c("'", results, "'", collapse = " ")
  write.description(new, DESCRIPTION)

  if (!identical(old, read.description(DESCRIPTION))) {
    cat('Updating collate directive in ', DESCRIPTION, "\n")
  }
}

base_path <- function(path, base) {
  path <- normalizePath(path, winslash = "/")
  base <- normalizePath(base, winslash = "/")

  str_replace(path, fixed(str_c(base, "/")), "")
}

