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
      topic$add_simple_field("description", topic$get_field("title")$values)
    }
  }

  invisible()
}

topic_add_name_aliases <- function(topic, block, name) {
  tags <- block_get_tags(block, "aliases")

  if (length(tags) == 0) {
    aliases <- character()
  } else {
    vals <- map_chr(tags, "val")
    aliases <- unlist(str_split(vals, "\\s+"))
  }

  if (any(aliases == "NULL")) {
    # Don't add default aliases
    aliases <- setdiff(aliases, "NULL")
  } else {
    aliases <- c(name, block$object$alias, aliases)
  }
  aliases <- unique(aliases)

  topic$add_simple_field("name", name)
  topic$add_simple_field("alias", aliases)
}

# Prefer explicit \code{@@usage} to a \code{@@formals} list.
topic_add_usage <- function(topic, block, old_usage = FALSE) {
  tag <- block_get_tag(block, "usage")

  if (is.null(tag)) {
    usage <- object_usage(block$object, old_usage = old_usage)
  } else if (tag$val == "NULL") {
    usage <- NULL
  } else {
    # Treat user input as already escaped, otherwise they have no way
    # to enter \S4method etc.
    usage <- rd(tag$val)
  }
  topic$add_simple_field("usage", usage)
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
  roxy_field_simple("formals", x$val)
}

#' @export
roxy_tag_rd.roxy_tag_.methods <- function(x, base_path, env) {
  desc <- lapply(x$val, function(x) docstring(x$value@.Data))
  usage <- map_chr(x$val, function(x) {
    function_usage(x$value@name, formals(x$value@.Data))
  })

  has_docs <- !map_lgl(desc, is.null)
  desc <- desc[has_docs]
  usage <- usage[has_docs]

  roxy_field_simple("rcmethods", setNames(desc, usage))
}

# Regular tags ------------------------------------------------------------

#' @export
roxy_tag_parse.roxy_tag_aliases <- function(x) tag_value(x)

#' @export
roxy_tag_parse.roxy_tag_author <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_author <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_concept <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_concept <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_description <- function(x) tag_markdown_with_sections(x)
#' @export
roxy_tag_rd.roxy_tag_description <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_details <- function(x) tag_markdown_with_sections(x)
#' @export
roxy_tag_rd.roxy_tag_details <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_docType <- function(x) tag_name(x)
#' @export
roxy_tag_rd.roxy_tag_docType <- function(x, base_path, env) {
  roxy_field_simple("docType", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_encoding <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_encoding <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_evalRd <- function(x) tag_code(x)
#' @export
roxy_tag_rd.roxy_tag_evalRd <- function(x, base_path, env) {
  roxy_field_simple("rawRd", roxy_tag_eval(x, env))
}

#' @export
roxy_tag_parse.roxy_tag_family <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_family <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_field <- function(x) tag_name_description(x)
#' @export
roxy_tag_rd.roxy_tag_field <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  roxy_field_simple(x$tag, value)
}

#' @export
roxy_tag_parse.roxy_tag_format <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_format <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_keywords <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_keywords <- function(x, base_path, env) {
  roxy_field_simple("keyword", str_split(x$val, "\\s+")[[1]])
}

#' @export
roxy_tag_parse.roxy_tag_method <- function(x) tag_words(x, 2, 2)

#' @export
roxy_tag_parse.roxy_tag_name <- function(x) tag_value(x)

#' @export
roxy_tag_parse.roxy_tag_md <- function(x) tag_toggle(x)

#' @export
roxy_tag_parse.roxy_tag_noMd <- function(x) tag_toggle(x)

#' @export
roxy_tag_parse.roxy_tag_noRd <- function(x) tag_toggle(x)

#' @export
roxy_tag_parse.roxy_tag_note <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_note <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_rdname <- function(x) tag_value(x)

#' @export
roxy_tag_parse.roxy_tag_rawRd <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_rawRd <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_references <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_references <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_return <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_return <- function(x, base_path, env) {
  roxy_field_markdown("value", x$val)
}


#' @export
roxy_tag_parse.roxy_tag_seealso <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_seealso <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_slot <- function(x) tag_name_description(x)
#' @export
roxy_tag_rd.roxy_tag_slot <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  roxy_field_simple(x$tag, value)
}

#' @export
roxy_tag_parse.roxy_tag_source <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_source <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_template <- function(x) tag_value(x)

#' @export
roxy_tag_parse.roxy_tag_templateVar <- function(x) tag_name_description(x)

#' @export
roxy_tag_parse.roxy_tag_title <- function(x) tag_markdown(x)
#' @export
roxy_tag_rd.roxy_tag_title <- function(x, base_path, env) {
  roxy_field_markdown(x$tag, x$val)
}

#' @export
roxy_tag_parse.roxy_tag_usage <- function(x) tag_value(x)
