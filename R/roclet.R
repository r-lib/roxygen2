#' Abstract roclet that serves as a rudimentary API.
#'
#' @export
make.roclet <- function(package.dir, process, output) {
  roclet <- new.env(parent = emptyenv())

  roclet$parse <- function(paths) {
    parsed <- parse.files(paths)
    contents <- Filter(function(x) length(x) > 1, parsed)
    results <- process(contents)
    output(results)
  }

  roclet$parse.dir <- function(path = file.path(package.dir, "R")) {
    files <- as.list(dir(path, pattern = '\\.(R|r)$', full.names = TRUE))
    roclet$parse(files)
  }

  structure(roclet, class='roclet')
}
