#' Build new roclet object.
#'
#' @export
#' @keywords internal
new_roclet <- function(obj, subclass = NULL) {
  structure(obj, class = c(subclass, 'roclet'))
}

is.roclet <- function(x) inherits(x, "roclet")

#' Process roclet on string and capture results.
#' Useful for testing.
#'
#' @param roclet to use for processing
#' @param input source string
#' @export
#' @keywords internal
roc_proc_text <- function(roclet, input, options = list()) {
  stopifnot(is.roclet(roclet))
  
  parsed <- parse_text(input)
  roc_process(roclet, parsed, base_path = ".", options = list())
} 

# Internal methods for processing and output

roc_output <- function(roclet, results, base_path, options = list()) {
  UseMethod("roc_output", roclet)
}

roc_process <- function(roclet, partita, base_path, options = list()) {
  UseMethod("roc_process", roclet)
}
