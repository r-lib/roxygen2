#' @import stringr
NULL

#' Roclet: make Rd files
#'
#' @description
#' This roclet is the workhorse of roxygen2, producing the `.Rd` files that
#' R uses to document functions, datasets, packages, classes, and more.
#' See `vignette("rd")` for details.
#'
#' Generally you will not call this function directly
#' but will instead use [roxygenise()] specifying the rd roclet.
#'
#' @seealso [tags-rd], [tags-rd-other], [tags-reuse], [tags-index-crossref] for
#'   tags provided by this roclet.
#' @export
#' @examples
#' #' The length of a string (in characters)
#' #'
#' #' @param x A character vector.
#' #' @returns An integer vector the same length as `x`.
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

#' @export
roclet_process.roclet_rd <- function(x, blocks, env, base_path) {
  # Convert each block into a topic, indexed by filename
  topics <- RoxyTopics$new()

  for (block in blocks) {
    rd <- block_to_rd(block, base_path, env)
    topics$add(rd, block)
  }
  topics_process_family(topics, env)
  topics_process_inherit(topics, env)
  topics$drop_invalid()
  topics_fix_params_order(topics)
  topics_add_default_description(topics)
  topics_add_package_alias(topics)

  topics$topics
}

#' @export
roclet_output.roclet_rd <- function(
  x,
  results,
  base_path,
  ...,
  is_first = FALSE
) {
  man <- normalizePath(file.path(base_path, "man"))

  contents <- map_chr(results, format)
  paths <- file.path(man, names(results))

  names <- unname(map_chr(results, \(x) x$get_name()[[1]]))
  if (length(names) > 0) {
    commands <- paste0("pkgload::dev_help('", names, "')")
  } else {
    commands <- character()
  }

  # Always check for roxygen2 header before overwriting NAMESPACE (#436),
  # even when running for the first time
  mapply(write_if_different, paths, contents, command = commands)

  if (!is_first) {
    # Automatically delete any files in man directory that were generated
    # by roxygen in the past, but weren't generated in this sweep.

    old_paths <- setdiff(dir(man, full.names = TRUE), paths)
    old_paths <- old_paths[!file.info(old_paths)$isdir]
    old_roxygen <- Filter(made_by_roxygen, old_paths)
    if (length(old_roxygen) > 0) {
      cli::cli_inform("Deleting {.file {basename(old_roxygen)}}")
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

  block_has_tags(
    block,
    c(
      "description",
      "param",
      "return",
      "title",
      "example",
      "examples",
      "name",
      "rdname",
      "details",
      "inherit",
      "describeIn"
    )
  )
}

# Tag processing functions ------------------------------------------------

block_to_rd <- function(block, base_path, env) {
  UseMethod("block_to_rd")
}

#' @export

block_to_rd.default <- function(block, ...) {
  cli::cli_abort("Unknown block type", .internal = TRUE)
}

#' @export

block_to_rd.roxy_block <- function(block, base_path, env) {
  # Must start by processing templates
  block <- process_templates(block, base_path)

  if (!needs_doc(block)) {
    return()
  }

  name <- block_get_tag(block, "name")$val %||% block$object$topic
  if (is.null(name)) {
    warn_roxy_block(
      block,
      c(
        "Block must have a @name",
        i = "Either document an existing object or manually specify with @name"
      )
    )
    return()
  }

  rd <- RoxyTopic$new()
  topic_add_name_aliases(rd, block, name)
  for (tag in block$tags) {
    rd$add(roxy_tag_rd(tag, env = env, base_path = base_path))
  }

  if (rd$has_section("description") && rd$has_section("reexport")) {
    warn_roxy_block(
      block,
      "Block must not include a description when re-exporting a function"
    )
    return()
  }

  describe_rdname <- topic_add_describe_in(rd, block, env)
  filename <- describe_rdname %||%
    block_get_tag(block, "rdname")$val %||%
    nice_name(name)
  rd$filename <- paste0(filename, ".Rd")

  rd
}

#' @export

block_to_rd.roxy_block_r6class <- function(block, base_path, env) {
  r6on <- roxy_meta_get("r6", TRUE)
  if (!isTRUE(r6on)) {
    return(NextMethod())
  }

  # Must start by processing templates
  block <- process_templates(block, base_path)

  if (!needs_doc(block)) {
    return()
  }

  name <- block_get_tag(block, "name")$val %||% block$object$topic
  if (is.null(name)) {
    warn_roxy_block(block, "must have a @name")
    return()
  }

  rd <- RoxyTopic$new()
  topic_add_name_aliases(rd, block, name)

  rd$add(roxy_tag_rd(
    block_get_tag(block, "name"),
    env = env,
    base_path = base_path
  ))
  rd$add(roxy_tag_rd(
    block_get_tag(block, "title"),
    env = env,
    base_path = base_path
  ))

  if (rd$has_section("description") && rd$has_section("reexport")) {
    warn_roxy_block(
      block,
      "Block must not include a description when re-exporting a function"
    )
    return()
  }

  topic_add_r6_methods(rd, block, env)

  describe_rdname <- topic_add_describe_in(rd, block, env)
  filename <- describe_rdname %||%
    block_get_tag(block, "rdname")$val %||%
    nice_name(name)
  rd$filename <- paste0(filename, ".Rd")

  rd
}

# Special cases -----------------------------------------------------------

topics_add_default_description <- function(topics) {
  for (topic in topics$topics) {
    if (length(topic$get_section("description")) > 0) {
      next
    }

    # rexport manually generates a own description, so don't need to
    if (
      !topic$has_section("reexport") &&
        !identical(topic$get_value("docType"), "package")
    ) {
      topic$add(rd_section("description", topic$get_value("title")))
    }
  }

  invisible()
}

topics_add_package_alias <- function(topics) {
  aliases <- unlist(topics$simple_values("alias"), use.names = FALSE)

  for (topic in topics$topics) {
    if (!identical(topic$get_value("docType"), "package")) {
      next
    }

    package <- topic$get_value("package")
    defaults <- c(package, package_suffix(package))

    aliases <- union(setdiff(defaults, aliases), topic$get_value("alias"))
    topic$add(rd_section("alias", aliases), overwrite = TRUE)
    break
  }

  invisible(NULL)
}

# Tag-wise processing -----------------------------------------------------

#' Generate Rd output from a tag
#'
#' Provide a method for this generic if you want a tag to generate output
#' in `.Rd` files. See `vignette("extending")` for more details.
#'
#' @param x The tag
#' @param base_path Path to package root directory.
#' @param env Environment in which to evaluate code (if needed)
#' @return Methods must return a [rd_section].
#' @export
#' @keywords internal
roxy_tag_rd <- function(x, base_path, env) {
  UseMethod("roxy_tag_rd")
}

#' @export
roxy_tag_rd.default <- function(x, base_path, env) {}

# Special tags ------------------------------------------------------------
# These tags do not directly affect the output, and are no complicated enough
# to require their own files.

#' @export
roxy_tag_rd.roxy_tag_.formals <- function(x, base_path, env) {
  rd_section("formals", x$val)
}
#' @export
format.rd_section_formals <- function(x, ...) NULL

#' @export
roxy_tag_rd.roxy_tag_.package <- function(x, base_path, env) {
  rd_section("package", x$val)
}
#' @export
format.rd_section_package <- function(x, ...) NULL

#' @export
roxy_tag_parse.roxy_tag_method <- function(x) tag_words(x, 2, 2)

#' @export
roxy_tag_parse.roxy_tag_noRd <- function(x) tag_toggle(x)

#' @export
roxy_tag_parse.roxy_tag_rdname <- function(x) tag_value(x)
