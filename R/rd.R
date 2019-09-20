#' @import stringr
NULL

#' Roclet: make Rd files.
#'
#' @template rd
#' @family roclets
#' @eval rd_roclet_description()
#' @eval tag_aliases(roclet_tags.roclet_rd)
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
roclet_tags.roclet_rd <- function(x) {
  list(
    aliases = tag_value,
    author = tag_markdown,
    backref = tag_value,
    concept = tag_markdown,
    describeIn = tag_name_description,
    description = tag_markdown_with_sections,
    details = tag_markdown_with_sections,
    docType = tag_name,
    encoding = tag_value,
    evalRd = tag_code,
    example = tag_value,
    examples = tag_examples,
    family = tag_value,
    field = tag_name_description,
    format = tag_markdown,
    includeRmd = tag_value,
    inherit = tag_inherit,
    inheritParams = tag_value,
    inheritDotParams =
      tag_two_part("source", "args", required = FALSE, markdown = FALSE),
    inheritSection = tag_name_description,
    keywords = tag_value,
    method = tag_words(2, 2),
    name = tag_value,
    md = tag_toggle,
    noMd = tag_toggle,
    noRd = tag_toggle,
    note = tag_markdown,
    param = tag_name_description,
    rdname = tag_value,
    rawRd = tag_value,
    references = tag_markdown,
    return = tag_markdown,
    section = tag_markdown,
    seealso = tag_markdown,
    slot = tag_name_description,
    source = tag_markdown,
    template = tag_value,
    templateVar = tag_name_description,
    title = tag_markdown,
    usage = tag_value
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

  # Note that order of operations here doesn't matter: fields are
  # ordered by RoxyFile$format()
  rd <- RoxyTopic$new()
  topic_add_name_aliases(rd, block, name)

  # Some fields added directly by roxygen internals
  tags <- Filter(roxy_tag_is_field, block$tags)
  for (tag in tags) {
    rd$add(tag$val)
  }

  topic_add_backref(rd, block)
  topic_add_doc_type(rd, block)
  topic_add_eval_rd(rd, block, env)
  topic_add_include_rmd(rd, block, base_path)
  topic_add_examples(rd, block, base_path)
  topic_add_fields(rd, block)
  topic_add_inherit(rd, block)
  topic_add_keyword(rd, block)
  topic_add_methods(rd, block)
  topic_add_params(rd, block)
  topic_add_simple_tags(rd, block)
  topic_add_sections(rd, block)
  topic_add_slots(rd, block)
  topic_add_usage(rd, block, old_usage = global_options$old_usage)
  topic_add_value(rd, block)

  if (rd$has_field("description") && rd$has_field("reexport")) {
    roxy_tag_warning(block$tags[[1]], "Can't use description when re-exporting")
    return()
  }

  describe_rdname <- topic_add_describe_in(rd, block, env)
  filename <- describe_rdname %||% block_get_tag(block, "rdname")$val %||% nice_name(name)
  rd$filename <- paste0(filename, ".Rd")

  rd
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

topic_add_backref <- function(topic, block) {
  tags <- block_get_tags(block, "backref")
  for (tag in tags) {
    topic$add_simple_field("backref", tag$val)
  }
}

# Simple tags can be converted directly to fields
topic_add_simple_tags <- function(topic, block) {
  simple_tags <- block_get_tags(block,
    c(
      "author", "concept", "description", "details", "encoding", "family",
      "format", "note", "rawRd", "references",
      "seealso", "source", "title"
    )
  )

  for (tag in simple_tags) {
    if (length(tag$val) && nchar(tag$val[[1]])) {
      topic$add_simple_field(tag$tag, tag$val[[1]])
    }
    for (extra in tag$val[-1]) {
      topic$add_simple_field("rawRd", extra)
    }
  }
}

topic_add_params <- function(topic, block) {
  # Used in process_inherit_params()
  value <- block$object$value
  if (is.function(value)) {
    formals <- formals(value)
    topic$add_simple_field("formals", names(formals))
  }

  process_def_tag(topic, block, "param")
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


topic_add_methods <- function(topic, block) {
  obj <- block$object
  if (!inherits(obj, "rcclass")) return()

  methods <- obj$methods
  if (is.null(obj$methods)) return()

  desc <- lapply(methods, function(x) docstring(x$value@.Data))
  usage <- map_chr(methods, function(x) {
    function_usage(x$value@name, formals(x$value@.Data))
  })

  has_docs <- !map_lgl(desc, is.null)
  desc <- desc[has_docs]
  usage <- usage[has_docs]

  topic$add_simple_field("rcmethods", setNames(desc, usage))
}

topic_add_inherit <- function(topic, block) {
  tags <- block_get_tags(block, "inherit")
  for (tag in tags) {
    field <- roxy_field_inherit(tag$val$source, list(tag$val$fields))
    topic$add_field(field)
  }

  tags <- block_get_tags(block, "inheritParams")
  for (tag in tags) {
    field <- roxy_field_inherit(tag$val, list("params"))
    topic$add_field(field)
  }

  tags <- block_get_tags(block, "inheritSection")
  for (tag in tags) {
    field <- roxy_field_inherit_section(tag$val$name, tag$val$description)
    topic$add_field(field)
  }

  tags <- block_get_tags(block, "inheritDotParams")
  for (tag in tags) {
    field <- roxy_field_inherit_dot_params(tag$val$source, tag$val$args)
    topic$add_field(field)
  }
}


topic_add_value <- function(topic, block) {
  tags <- block_get_tags(block, "return")

  for (tag in tags) {
    topic$add_simple_field("value", tag$val)
  }
}

topic_add_keyword <- function(topic, block) {
  tags <- block_get_tags(block, "keywords")

  vals <- map_chr(tags, "val")
  keywords <- unlist(str_split(vals, "\\s+"))

  topic$add_simple_field("keyword", keywords)
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

topic_add_slots <- function(topic, block) {
  process_def_tag(topic, block, "slot")
}

topic_add_fields <- function(topic, block) {
  process_def_tag(topic, block, "field")
}

topic_add_eval_rd <- function(topic, block, env) {
  tags <- block_get_tags(block, "evalRd")

  for (tag in tags) {
    out <- roxy_tag_eval(tag, env)
    topic$add_simple_field("rawRd", out)
  }
}

topic_add_include_rmd <- function(topic, block, base_path) {
  tags <- block_get_tags(block, "includeRmd")

  for (tag in tags) {
    if (!is_installed("rmarkdown")) {
      roxy_tag_warning(tag, "Needs the rmarkdown package")
    }
    out <- block_include_rmd(tag, block, base_path)
    if (!is.null(out[[1]])) {
      topic$add_simple_field("details", out[[1]])
    }
    lapply(out[-1], function(s) {
      topic$add_simple_field("rawRd", s)
    })
  }
}

topic_add_sections <- function(topic, block) {
  tags <- block_get_tags(block, "section")

  for (tag in tags) {
    pieces <- str_split(tag$val, ":", n = 2)[[1]]

    title <- str_split(pieces[1], "\n")[[1]]
    if (length(title) > 1) {
      roxy_tag_warning(tag,
        "Section title spans multiple lines: \n", "@section ", title[1]
      )
      return()
    }

    topic$add_field(roxy_field_section(pieces[1], pieces[2]))
  }
}

topic_add_doc_type <- function(topic, block) {
  tag <- block_get_tag(block, "docType")
  if (is.null(tag)) {
    return()
  }

  topic$add_simple_field("docType", tag$val)

  if (tag$val == "package") {
    name <- block_get_tag(block, "name")
    if (!str_detect(name$val, "-package")) {
      topic$add_simple_field("alias", package_suffix(name$val))
    }
  }
}

package_suffix <- function(name) {
  paste0(name, "-package")
}

# Name + description tags ------------------------------------------------------

process_def_tag <- function(topic, block, tag) {
  tags <- block_get_tags(block, tag)
  if (length(tags) == 0) {
    return()
  }

  desc <- str_trim(map_chr(tags, c("val", "description")))
  names(desc) <- map_chr(tags, c("val", "name"))

  topic$add_simple_field(tag, desc)
}
