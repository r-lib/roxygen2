#' @import stringr
NULL

#' Roclet: make Rd files.
#'
#' @family roclets
#' @template rd
#' @eval rd_roclet_description()
#' @seealso `vignette("rd", package = "roxygen2")`
#' @export
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
    description = tag_markdown,
    details = tag_markdown,
    docType = tag_name,
    encoding = tag_value,
    evalRd = tag_code,
    example = tag_value,
    examples = tag_examples,
    family = tag_value,
    field = tag_name_description,
    format = tag_markdown,
    inherit = tag_inherit,
    inheritParams = tag_value,
    inheritDotParams = tag_two_part("source", "args", required = FALSE),
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
    title = tag_markdown_restricted,
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
  topics_process_family(topics)
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

  name <- block$name %||% object_topic(attr(block, "object"))
  if (is.null(name)) {
    block_warning(block, "Missing name")
    return()
  }

  # Note that order of operations here doesn't matter: fields are
  # ordered by RoxyFile$format()
  rd <- RoxyTopic$new()
  topic_add_name_aliases(rd, block, name)

  # Some fields added directly by roxygen internals
  fields <- Filter(is_roxy_field, block)
  rd$add(fields)

  topic_add_backref(rd, block)
  topic_add_doc_type(rd, block)
  topic_add_eval_rd(rd, block, env)
  topic_add_examples(rd, block, base_path)
  topic_add_fields(rd, block)
  topic_add_inherit(rd, block)
  topic_add_keyword(rd, block)
  topic_add_methods(rd, block)
  topic_add_params(rd, block)
  topic_add_simple_tags(rd, block)
  topic_add_sections(rd, block)
  topic_add_slots(rd, block)
  topic_add_usage(rd, block)
  topic_add_value(rd, block)

  if (rd$has_field("description") && rd$has_field("reexport")) {
    block_warning(block, "Can't use description when re-exporting")
    return()
  }

  describe_rdname <- topic_add_describe_in(rd, block, env)

  filename <- describe_rdname %||% block$rdname %||% nice_name(name)
  rd$filename <- paste0(filename, ".Rd")

  rd
}

#' @export
roclet_output.roclet_rd <- function(x, results, base_path, ..., is_first = FALSE) {
  man <- normalizePath(file.path(base_path, "man"))

  contents <- vapply(results, format, wrap = FALSE, FUN.VALUE = character(1))
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
  made_by_me <- vapply(rd, made_by_roxygen, logical(1))

  unlink(rd[made_by_me])
}

block_tags <- function(x, tag) {
  x[names(x) %in% tag]
}

needs_doc <- function(block) {
  # Does this block get an Rd file?
  if (any(names(block) == "noRd")) {
    return(FALSE)
  }

  key_tags <- c("description", "param", "return", "title", "example",
    "examples", "name", "rdname", "usage", "details", "introduction",
    "inherit", "describeIn")

  any(names(block) %in% key_tags)
}

# Tag processing functions ------------------------------------------------

topic_add_backref <- function(topic, block) {
  backrefs <- block_tags(block, "backref") %||% attr(block, "filename")

  for (backref in backrefs) {
    topic$add_simple_field("backref", backref)
  }
}

# Simple tags can be converted directly to fields
topic_add_simple_tags <- function(topic, block) {
  simple_tags <- c(
    "author", "concept", "description", "details", "encoding", "family",
    "format", "note", "rawRd", "references",
    "seealso", "source", "title"
  )

  is_simple <- names(block) %in% simple_tags
  tag_values <- block[is_simple]
  tag_names <- names(block)[is_simple]

  for (i in seq_along(tag_values)) {
    topic$add_simple_field(tag_names[[i]], tag_values[[i]])
  }
}

topic_add_params <- function(topic, block) {
  # Used in process_inherit_params()
  value <- attr(block, "object")$value
  if (is.function(value)) {
    formals <- formals(value)
    topic$add_simple_field("formals", names(formals))
  }

  process_def_tag(topic, block, "param")
}

topic_add_name_aliases <- function(topic, block, name) {
  tags <- block_tags(block, "aliases")

  if (length(tags) == 0) {
    aliases <- character()
  } else {
    aliases <- str_split(str_trim(unlist(tags, use.names = FALSE)), "\\s+")[[1]]
  }

  if (any(aliases == "NULL")) {
    # Don't add default aliases
    aliases <- aliases[aliases != "NULL"]
  } else {
    aliases <- c(name, attr(block, "object")$alias, aliases)
  }
  aliases <- unique(aliases)

  topic$add_simple_field("name", name)
  topic$add_simple_field("alias", aliases)
}


topic_add_methods <- function(topic, block) {
  obj <- attr(block, "object")
  if (!inherits(obj, "rcclass")) return()

  methods <- obj$methods
  if (is.null(obj$methods)) return()

  desc <- lapply(methods, function(x) docstring(x$value@.Data))
  usage <- vapply(methods, function(x) {
    usage <- function_usage(x$value@name, formals(x$value@.Data))
    as.character(wrap_string(usage))
  }, character(1))

  has_docs <- !vapply(desc, is.null, logical(1))
  desc <- desc[has_docs]
  usage <- usage[has_docs]

  topic$add_simple_field("rcmethods", setNames(desc, usage))
}

topic_add_inherit <- function(topic, block) {
  tags <- block_tags(block, "inherit")
  for (tag in tags) {
    field <- roxy_field_inherit(tag$source, list(tag$fields))
    topic$add_field(field)
  }

  tags <- block_tags(block, "inheritParams")
  for (tag in tags) {
    field <- roxy_field_inherit(tag, list("params"))
    topic$add_field(field)
  }

  tags <- block_tags(block, "inheritSection")
  for (tag in tags) {
    field <- roxy_field_inherit_section(tag$name, tag$description)
    topic$add_field(field)
  }

  tags <- block_tags(block, "inheritDotParams")
  for (tag in tags) {
    field <- roxy_field_inherit_dot_params(tag$source, tag$args)
    topic$add_field(field)
  }
}


topic_add_value <- function(topic, block) {
  tags <- block_tags(block, "return")

  for (tag in tags) {
    topic$add_simple_field("value", tag)
  }
}

topic_add_keyword <- function(topic, block) {
  tags <- block_tags(block, "keywords")
  keywords <- unlist(str_split(str_trim(tags), "\\s+"))

  topic$add_simple_field("keyword", keywords)
}

# Prefer explicit \code{@@usage} to a \code{@@formals} list.
topic_add_usage <- function(topic, block) {
  if (is.null(block$usage)) {
    usage <- wrap_string(object_usage(attr(block, "object")), width = 75L)
  } else if (block$usage == "NULL") {
    usage <- NULL
  } else {
    # Treat user input as already escaped, otherwise they have no way
    # to enter \S4method etc.
    usage <- rd(block$usage)
  }
  topic$add_simple_field("usage", usage)
}

topic_add_slots <- function(topic, block) {
  process_def_tag(topic, block, "slot")
}

topic_add_fields <- function(topic, block) {
  process_def_tag(topic, block, "field")
}

# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
topic_add_examples <- function(topic, block, base_path) {
  examples <- block_tags(block, "examples")
  for (example in examples) {
    topic$add_simple_field("examples", example)
  }

  paths <- str_trim(unlist(block_tags(block, "example")))
  paths <- file.path(base_path, paths)

  for (path in paths) {
    # Check that haven't accidentally used example instead of examples
    nl <- str_count(path, "\n")
    if (any(nl) > 0) {
      block_warning(block, "@example spans multiple lines. Do you want @examples?")
      next
    }

    if (!file.exists(path)) {
      block_warning(block, "@example ", path, " doesn't exist")
      next
    }

    code <- read_lines(path)
    examples <- escape_examples(code)

    topic$add_simple_field("examples", examples)
  }
}

topic_add_eval_rd <- function(topic, block, env) {
  tags <- block_tags(block, "evalRd")

  for (tag in tags) {
    out <- block_eval(tag, block, env, "@evalRd")
    if (!is.null(out)) {
      topic$add_simple_field("rawRd", out)
    }
  }
}

topic_add_sections <- function(topic, block) {
  sections <- block_tags(block, "section")

  for (section in sections) {
    pieces <- str_split(section, ":", n = 2)[[1]]

    title <- str_split(pieces[1], "\n")[[1]]
    if (length(title) > 1) {
      return(block_warning(
        block,
        "Section title spans multiple lines: \n", "@section ", title[1]
      ))
    }

    topic$add_field(roxy_field_section(pieces[1], pieces[2]))
  }
}

topic_add_doc_type <- function(topic, block) {
  doctype <- block$docType
  if (is.null(doctype)) return()

  topic$add_simple_field("docType", doctype)

  if (doctype == "package") {
    name <- block$name
    if (!str_detect(name, "-package")) {
      topic$add_simple_field("alias", package_suffix(name))
    }
  }

}

package_suffix <- function(name) {
  paste0(name, "-package")
}

process_tag <- function(block, tag, f = roxy_field, ...) {
  matches <- block[names(block) == tag]
  if (length(matches) == 0) return()

  lapply(matches, function(p) f(tag, p, ...))
}

# Name + description tags ------------------------------------------------------


process_def_tag <- function(topic, block, tag) {
  tags <- block[names(block) == tag]
  if (length(tags) == 0) return()

  desc <- str_trim(sapply(tags, "[[", "description"))
  names(desc) <- sapply(tags, "[[", "name")

  topic$add_simple_field(tag, desc)
}
