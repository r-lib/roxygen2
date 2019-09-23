#' @import stringr
NULL

#' Roclet: make Rd files.
#'
#' @template rd
#' @family roclets
#' @eval rd_roclet_description()
#' @export
#' @examples
#' #' The length of a string (in characters)
#' #'
#' #' @param x String input character vector
#' #' @return An integer vector the same length as `x`.
#' #'   `NA` strings have `NA` length.
#' #' @seealso [nchar()]
#' #' @export
#' #' @examples
#' #' str_length(letters)
#' #' str_length(c("i", "like", "programming", NA))
#' str_length <- function(x) {
#' }
rd_roclet <- function() {
  roclet("rd")
}

rd_roclet_description <- function() {
  c(
    "@description",
    "Generally you will not call this function directly",
    "but will instead use roxygenise() specifying the rd roclet"
  )
}

#' @export
roclet_process.roclet_rd <- function(x,
                                     blocks,
                                     env,
                                     base_path,
                                     global_options = list()) {

  # Convert each block into a topic, indexed by filename
  topics <- RoxyTopics$new()

  for (block in blocks) {
    rd <- block_to_rd(block, base_path, env, global_options)
    topics$add(rd)
  }
  topics_process_family(topics, env)
  topics_process_inherit(topics, env)
  topics$drop_invalid()
  topics_fix_params_order(topics)
  topics_add_default_description(topics)

  topics$topics
}

#' @export
roclet_output.roclet_rd <- function(x, results, base_path, ..., is_first = FALSE) {
  man <- normalizePath(file.path(base_path, "man"))

  contents <- map_chr(results, format, wrap = FALSE)
  paths <- file.path(man, names(results))

  # Always check for roxygen2 header before overwriting NAMESPACE (#436),
  # even when running for the first time
  mapply(write_if_different, paths, contents, MoreArgs = list(check = TRUE))

  if (!is_first) {
    # Automatically delete any files in man directory that were generated
    # by roxygen in the past, but weren't generated in this sweep.

    old_paths <- setdiff(dir(man, full.names = TRUE), paths)
    old_paths <- old_paths[!file.info(old_paths)$isdir]
    old_roxygen <- Filter(made_by_roxygen, old_paths)
    if (length(old_roxygen) > 0) {
      message(paste0("Deleting ", basename(old_roxygen), collapse = "\n"))
      unlink(old_roxygen)
    }
  }

  paths
}

#' @export
roclet_clean.roclet_rd <- function(x, base_path) {
  rd <- dir(file.path(base_path, "man"), full.names = TRUE)
  rd <- rd[!file.info(rd)$isdir]
  unlink(purrr::keep(rd, made_by_roxygen))
}

# Does this block get an Rd file?
needs_doc <- function(block) {
  if (block_has_tags(block, "noRd")) {
    return(FALSE)
  }

  block_has_tags(block, c(
    "description", "param", "return", "title", "example",
    "examples", "name", "rdname", "usage", "details", "introduction",
    "inherit", "describeIn")
  )
}

# Tag processing functions ------------------------------------------------

block_to_rd <- function(block, base_path, env, global_options = list()) {
  # Must start by processing templates
  block <- process_templates(block, base_path, global_options)

  if (!needs_doc(block)) {
    return()
  }

  name <- block_get_tag(block, "name")$val %||% block$object$topic
  if (is.null(name)) {
    roxy_tag_warning(block$tags[[1]], "Missing name")
    return()
  }

  rd <- RoxyTopic$new()
  topic_add_name_aliases(rd, block, name)
  for (tag in block$tags) {
    rd$add(roxy_tag_rd(tag, env = env, base_path = base_path))
  }

  topic_add_usage(rd, block, old_usage = global_options$old_usage)

  if (rd$has_field("description") && rd$has_field("reexport")) {
    roxy_tag_warning(block$tags[[1]], "Can't use description when re-exporting")
    return()
  }

  describe_rdname <- topic_add_describe_in(rd, block, env)
  filename <- describe_rdname %||% block_get_tag(block, "rdname")$val %||% nice_name(name)
  rd$filename <- paste0(filename, ".Rd")

  rd
}

# Special cases -----------------------------------------------------------

topics_add_default_description <- function(topics) {
  for (topic in topics$topics) {
    if (length(topic$get_field("description")) > 0)
      next

    # rexport manually generates a own description, so don't need to
    if (!topic$has_field("reexport")) {
      topic$add_simple_field("description", topic$get_value("title"))
    }
  }

  invisible()
}

# Tag-wise processing -----------------------------------------------------

roxy_tag_rd <- function(x, base_path, env) {
  UseMethod("roxy_tag_rd")
}

roxy_tag_rd.default <- function(x, base_path, env) {
}

# Internal tags -----------------------------------------------------------

#' @export
roxy_tag_rd.roxy_tag_.reexport <- function(x, base_path, env) {
  roxy_field_reexport(x$val$pkg, x$val$fun)
}

#' @export
roxy_tag_rd.roxy_tag_.formals <- function(x, base_path, env) {
  roxy_field("formals", x$val)
}
#' @export
format.roxy_field_formals <- function(x, ...) NULL

#' @export
roxy_tag_rd.roxy_tag_.methods <- function(x, base_path, env) {
  desc <- lapply(x$val, function(x) docstring(x$value@.Data))
  usage <- map_chr(x$val, function(x) {
    function_usage(x$value@name, formals(x$value@.Data))
  })

  has_docs <- !map_lgl(desc, is.null)
  desc <- desc[has_docs]
  usage <- usage[has_docs]

  roxy_field("rcmethods", setNames(desc, usage))
}
#' @export
format.roxy_field_rcmethods <- function(x, ...) {
  roxy_field_description("Methods", names(x$value), x$value)
}

# Special tags ------------------------------------------------------------

#' @export
roxy_tag_parse.roxy_tag_method <- function(x) tag_words(x, 2, 2)

#' @export
roxy_tag_parse.roxy_tag_noRd <- function(x) tag_toggle(x)

#' @export
roxy_tag_parse.roxy_tag_rdname <- function(x) tag_value(x)
