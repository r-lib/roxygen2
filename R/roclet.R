#' Build new roclet object.
#'
#' @export
#' @keywords internal
new_roclet <- function(obj, subclass = NULL) {
  structure(obj, class = c(subclass, 'roclet'))
}

is.roclet <- function(x) inherits(x, "roclet")

#' Process roclet and capture results.
#' 
#' @param roclet to use for processing
#' @param input character vector of paths to files to process
#' @seealso \code{\link{roxygenise}} for user-friendly interface
#' @keywords internal
#' @export
roc_proc <- function(roclet, paths) {
  stopifnot(is.roclet(roclet))
  
  parsed <- parse.files(paths)
  # Remove srcrefs with no attached roxygen comments
  contents <- Filter(function(x) length(x) > 1, parsed)
  roc_process(roclet, contents)
} 

#' Process roclet and output results.
#' 
#' @param roclet to use for processing
#' @param input character vector of paths to files to process
#' @param output_path base directory in which to save output
#' @keywords internal
#' @seealso \code{\link{roxygenise}} for user-friendly interface
#' @export
roc_out <- function(roclet, input, path) {
  stopifnot(is.roclet(roclet))

  results <- roc_proc(roclet, input)
  roc_output(roclet, results, path)
}

# Internal methods for processing and output

roc_output <- function(roclet, results, path) {
  UseMethod("roc_output", roclet)
}

roc_process <- function(roclet, partita) {
  UseMethod("roc_process", roclet)
}
