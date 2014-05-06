#' Build new roclet object.
#'
#' @export
#' @keywords internal
new_roclet <- function(obj, subclass = NULL) {
  structure(obj, class = c(subclass, 'roclet'))
}

is.roclet <- function(x) inherits(x, "roclet")

#' Process roclet on string and capture results.
#'
#' Useful for testing.
#'
#' @param roclet Name of roclet to use for processing.
#' @param input Source string
#' @param options A list of options to control roxygen behaviour.
#'   Currently only \code{wrap} is recognised.
#' @export
#' @keywords internal
roc_proc_text <- function(roclet, input, options = list()) {
  stopifnot(is.roclet(roclet))

  parsed <- parse_text(input)
  roc_process(roclet, parsed, base_path = ".", options = options)
}

# Internal methods for processing and output

# Methods should return character vector describing all modified files.
roc_output <- function(roclet, results, base_path, options = list(),
                       check = TRUE) {
  UseMethod("roc_output", roclet)
}

roc_process <- function(roclet, parsed, base_path, options = list()) {
  UseMethod("roc_process", roclet)
}

# Given a roclet, removes all files created by that roclet
clean <- function(roclet, base_path) {
  UseMethod("clean")
}
