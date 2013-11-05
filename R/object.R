#' Constructors for S3 object to represent R object.
#' 
#' These objects are usually created by the parsers, but it is also
#' useful to generate them by hand for testing.
#' 
#' @param subclass This is an abstract class so this must be provided. 
#'   Currently this is one of "function", "s4generic", "s4class", "s4method",
#'   "rcclass", or "data".
#' @param name Name of the object being documented
#' @param value The object itself. 
#' @param ... Any other components you'd like to store in the class
#' @export
#' @keywords internal
object <- function(subclass, name, value, ...) {
  structure(list(
    name = name, 
    value = value, 
    ...
  ), 
  class = c(subclass, "object"))
}
