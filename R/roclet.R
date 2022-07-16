#' Build a new roclet.
#'
#' To create a new roclet, you will need to create a constructor function
#' that wraps `roclet`, and then implement the methods described below.
#'
#' @section Methods:
#'
#' * `roclet_preprocess()` is called after blocks have been parsed but before
#'   code has been evaluated. This should only be needed if your roclet affects
#'   how code will evaluated. Should return a roclet.
#'
#' * `roclet_process()` called after blocks have been evaluated; i.e. the
#'   `@eval` tag has been processed, and the object associated with each block
#'   has been determined.
#'
#' * `roclet_output()` is given the output from `roclet_process()` and should
#'   produce files on disk.
#'
#' * `roclet_clean()` called when `roxygenise(clean = TRUE)`. Should remove
#'   any files created by the roclet.
#'
#' ### Deprecated methods
#'
#' `roclet_tags()` is no longer used; instead provide a [roxy_tag_parse()]
#' method for each tag.
#'
#' @param x A `roclet` object.
#' @param blocks A list of [roxy_block] objects.
#' @param results Value returned from your `roclet_process()` method.
#' @param base_path Path to root of source package.
#' @param env Package environment.
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
roclet_preprocess <- function(x, blocks, base_path) {
  UseMethod("roclet_preprocess")
}

#' @export
roclet_preprocess.default <- function(x, blocks, base_path) {
  x
}

#' @export
#' @rdname roclet
roclet_process <- function(x, blocks, env, base_path) {
  UseMethod("roclet_process")
}

#' @export
#' @rdname roclet
roclet_output <- function(x, results, base_path, ...) {
  UseMethod("roclet_output", x)
}

#' @export
#' @rdname roclet
roclet_clean <- function(x, base_path) {
  UseMethod("roclet_clean")
}

#' @export
#' @rdname roclet
roclet_tags <- function(x) {
  UseMethod("roclet_tags")
}

#' Create a roclet from a string.
#'
#' This provides a flexible way of specifying a roclet in a string.
#'
#' @param x Arbitrary R code evaluated in roxygen2 package.
#' @keywords internal
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
    cli::cli_abort("Must return a roclet")
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
#' @export
#' @keywords internal
roc_proc_text <- function(roclet, input) {
  stopifnot(is.roclet(roclet))

  file <- tempfile()
  write_lines(input, file)
  on.exit(unlink(file))

  env <- env_file(file)
  blocks <- parse_text(input, env = env)
  roclet_process(roclet, blocks, env = env, base_path = ".")
}
