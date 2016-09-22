#' Build a new roclet.
#'
#' To create a new roclet, you will need to create a constructor function
#' that wraps `roclet`, and then implement methods for
#' `roclet_tags`, `roclet_process`, `roclet_output`, and `roclet_clean`.
#'
#' @keywords internal
#' @name roclet
NULL

#' @export
#' @rdname roclet
roclet <- function(subclass, ...) {
  structure(list(...), class = c(paste0("roclet_", subclass), "roclet"))
}

#' @export
#' @rdname roclet
roclet_output <- function(x, results, base_path, ...) {
  UseMethod("roclet_output", x)
}

#' @export
#' @rdname roclet
roclet_tags <- function(x) {
  UseMethod("roclet_tags")
}

#' @export
#' @rdname roclet
roclet_process <- function(x, parsed, base_path, global_options = list()) {
  UseMethod("roclet_process")
}

#' @export
#' @rdname roclet
roclet_clean <- function(x, base_path) {
  UseMethod("roclet_clean")
}

#' Create a roclet from a string.
#'
#' This provides a flexible way of specifying a roclet in a string.
#'
#' @param x Arbitrary R code evaluated in roxygen2 package.
#' @export
#' @examples
#' # rd, namespace, and vignette work for backward compatibility
#' roclet_find("rd")
#'
#' # But generally you should specify the name of a function that
#' # returns a roclet
#' roclet_find("rd_roclet")
#'
#' # If it lives in another package, you'll need to use ::
#' roclet_find("roxygen2::rd_roclet")
#'
#' # If it takes parameters (which no roclet does currently), you'll need
#' # to call the function
#' roclet_find("roxygen2::rd_roclet()")
roclet_find <- function(x) {
  env <- new.env(parent = getNamespace("roxygen2"))
  env$rd <- rd_roclet
  env$namespace <- namespace_roclet
  env$vignette <- vignette_roclet

  expr <- parse(text = x)
  res <- eval(expr, env)

  if (is.function(res)) {
    res <- res()
  }

  if (!is.roclet(res)) {
    stop("Must return a roclet", call. = FALSE)
  }

  res
}

is.roclet <- function(x) inherits(x, "roclet")

#' Process roclet on string and capture results.
#'
#' Useful for testing.
#'
#' @param roclet Name of roclet to use for processing.
#' @param input Source string
#' @param registry Named list of tag parsers
#' @param global_options List of global options
#' @export
#' @keywords internal
roc_proc_text <- function(roclet, input, registry = default_tags(),
                          global_options = list()) {
  stopifnot(is.roclet(roclet))

  parsed <- parse_text(input, registry = registry, global_options)
  roclet_process(roclet, parsed, base_path = ".", global_options)
}

default_tags <- function() {
  c(
    roclet_tags.roclet_namespace(list()),
    roclet_tags.roclet_rd(list())
  )
}
