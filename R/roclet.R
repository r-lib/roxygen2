#' Build a new roclet.
#'
#' To create a new roclet, you will need to create a constructor function
#' that wraps \code{roclet}, and then implement methods for
#' \code{roclet_tags}, \code{roclet_process}, \code{roclet_output}, and
#' \code{roclet_clean}.
#'
#' @keywords internal
#' @name roclet

#' @export
#' @rdname roclet
roclet <- function(subclass, ...) {
  structure(list(...), class = c(paste0("roclet_", subclass), "roclet"))
}

#' @export
#' @rdname roclet
roclet_output <- function(x, results, base_path, options = list(),
                          check = TRUE) {
  UseMethod("roclet_output", x)
}

#' @export
#' @rdname roclet
roclet_tags <- function(x) {
  UseMethod("roclet_tags")
}

#' @export
#' @rdname roclet
roclet_process <- function(x, parsed, base_path, options = list()) {
  UseMethod("roclet_process")
}

#' @export
#' @rdname roclet
roclet_clean <- function(x, base_path) {
  UseMethod("roclet_clean")
}

roclet_find <- function(x) {
  roclet <- paste0(x, "_roclet", sep = "")
  get(roclet, mode = "function")()
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
#' @param registry Named list of tag parsers
#' @export
#' @keywords internal
roc_proc_text <- function(roclet, input, options = list(), registry = default_tags()) {
  stopifnot(is.roclet(roclet))

  parsed <- parse_text(input, registry = registry)
  roclet_process(roclet, parsed, base_path = ".", options = options)
}

default_tags <- function() {
  c(
    roclet_tags.roclet_namespace(list()),
    roclet_tags.roclet_rd(list())
  )
}
