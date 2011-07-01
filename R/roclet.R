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
#' @param base_path base directory 
#' @seealso \code{\link{roxygenise}} for user-friendly interface
#' @keywords internal
#' @export
roc_proc <- function(roclet, paths, base_path) {
  stopifnot(is.roclet(roclet))
  
  parsed <- parse.files(paths)
  roc_process(roclet, parsed, base_path)
} 

#' Process roclet on string and capture results.
#' Useful for testing.
#'
#' @param roclet to use for processing
#' @param input source string
#' @export
#' @keywords internal
roc_proc_text <- function(roclet, input) {
  stopifnot(is.roclet(roclet))
  
  parsed <- parse.text(input)
  roc_process(roclet, parsed, base_path = ".")
} 


#' Process roclet and output results.
#' 
#' @param roclet to use for processing
#' @param input character vector of paths to files to process
#' @param base_path base directory in which to save output
#' @keywords internal
#' @seealso \code{\link{roxygenise}} for user-friendly interface
#' @export
roc_out <- function(roclet, input, base_path) {
  stopifnot(is.roclet(roclet))

  results <- roc_proc(roclet, input, base_path)
  roc_output(roclet, results, base_path)
}

# Internal methods for processing and output

roc_output <- function(roclet, results, base_path) {
  UseMethod("roc_output", roclet)
}

roc_process <- function(roclet, partita, base_path) {
  UseMethod("roc_process", roclet)
}
