#' @include tag-registry.R
#' @import stringr
NULL

register_tags(
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
  inheritParams = tag_value,
  keywords = tag_value,
  method = tag_words(2, 2),
  name = tag_value,
  md = tag_toggle,
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
  title = tag_markdown_title,
  usage = tag_value
)

#' Roclet: make Rd files.
#'
#' This roclet is the workhorse of \pkg{roxygen}, producing the Rd files that
#' document that functions in your package.
#'
#' @family roclets
#' @seealso \code{vignette("rd", package = "roxygen2")}
#' @export
rd_roclet <- function() {
  new_roclet(list(), "rd_roclet")
}

#' @export
roc_process.rd_roclet <- function(roclet, parsed, base_path, options = list()) {
  # Look at all blocks with roxygen comments
  blocks <- Filter(function(x) length(x) > 1, parsed$blocks)

  topics <- list()
  for (block in blocks) {
    rd <- block_to_rd(block, base_path, parsed$env)
    if (is.null(rd)) next

    if (rd$filename %in% names(topics)) {
      topics[[rd$filename]]$add(rd)
    } else {
      topics[[rd$filename]] <- rd
    }
  }

  # Drop any topics that don't have a title
  for (topic in names(topics)) {
    has_name_title <- topics[[topic]]$has_field(c("title", "name"))
    if (!all(has_name_title)) {
      warning(topic, " is missing name/title. Skipping", call. = FALSE)
      topics[[topic]] <- NULL
    }
  }

  topics <- process_family(topics)
  topics <- process_inherit_params(topics)
  fix_params_order(topics)
}

block_to_rd <- function(block, base_path, env) {
  # Must start by processing templates
  block <- process_templates(block, base_path)

  if (!needs_doc(block)) {
    return()
  }

  name <- block$name %||% object_topic(block$object)
  if (is.null(name)) {
    block_warning(block, "Missing name")
    return()
  }

  # Note that order of operations here doesn't matter: fields are
  # ordered by RoxyFile$format()
  rd <- RoxyTopic$new()

  topic_add_backref(rd, block)
  topic_add_name_aliases(rd, block, name)
  topic_add_methods(rd, block)
  topic_add_params(rd, block)
  topic_add_simple_tags(rd, block)
  topic_add_usage(rd, block)

  rd$add(process_slot(block))
  rd$add(process_field(block))
  rd$add(process_doc_type(block))
  rd$add(process_tag(block, "evalRd", function(tag, param) {
    expr <- parse(text = param)
    out <- eval(expr, envir = env)
    roxy_field("rawRd", as.character(out))
  }))
  rd$add(process_tag(block, "return", function(tag, param) {
    roxy_field("value", param)
  }))
  rd$add(process_tag(block, "keywords", function(tag, param) {
    roxy_field("keyword", str_split(str_trim(param), "\\s+")[[1]])
  }))
  rd$add(process_tag(block, "section", process_section, block))
  rd$add(process_examples(block, base_path))

  describe_in <- process_describe_in(block, env)
  rd$add(describe_in$tag)

  rd$filename <- paste0(describe_in$rdname %||% block$rdname %||%
    nice_name(name), ".Rd")

  rd
}

#' @export
roc_output.rd_roclet <- function(roclet, results, base_path, options = list(),
                           check = TRUE) {
  man <- normalizePath(file.path(base_path, "man"))

  contents <- vapply(results, format, wrap = options$wrap,
    FUN.VALUE = character(1))

  paths <- file.path(man, names(results))
  mapply(write_if_different, paths, contents, MoreArgs = list(check = check))

  if (check) {
    # Automatically delete any files in man directory that were generated
    # by roxygen in the past, but weren't generated in this sweep.

    old_paths <- setdiff(dir(man, full.names = TRUE), paths)
    old_paths <- old_paths[!file.info(old_paths)$isdir]
    old_roxygen <- Filter(made_by_roxygen, old_paths)
    if (length(old_roxygen) > 0) {
      cat(paste0("Deleting ", basename(old_roxygen), collapse = "\n"), "\n", sep = "")
      unlink(old_roxygen)
    }
  }

  paths
}

#' @export
clean.rd_roclet <- function(roclet, base_path) {
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
    "describeIn")

  any(names(block) %in% key_tags)
}

# Tag processing functions ------------------------------------------------

topic_add_backref <- function(topic, block) {
  backrefs <- block_tags(block, "backref") %||% block$srcref$filename

  for (backref in backrefs) {
    topic$add(roxy_field("backref", backref))
  }
}

# Simple tags can be converted directly to fields
topic_add_simple_tags <- function(topic, block) {
  simple_tags <- c(
    "author", "concept", "description", "details", "encoding", "family",
    "format", "inheritParams", "note", "rawRd", "reexport", "references",
    "seealso", "source", "title"
  )

  is_simple <- names(block) %in% simple_tags
  tag_values <- block[is_simple]
  tag_names <- names(block)[is_simple]

  for (i in seq_along(tag_values)) {
    topic$add_field(roxy_field(tag_names[[i]], tag_values[[i]]))
  }
}

topic_add_params <- function(topic, block) {
  # Used in process_inherit_params()
  if (is.function(block$object$value)) {
    formals <- formals(block$object$value)
    topic$add(roxy_field("formals", names(formals)))
  }

  topic$add(process_def_tag(block, "param"))
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
    aliases <- unique(c(name, block$object$alias, aliases))
  }

  topic$add_field(roxy_field("name", name))
  topic$add_field(roxy_field("alias", aliases))
}


topic_add_methods <- function(topic, block) {
  obj <- block$object
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

  topic$add(roxy_field("rcmethods", setNames(desc, usage)))
}

# Prefer explicit \code{@@usage} to a \code{@@formals} list.
topic_add_usage <- function(topic, block) {
  if (is.null(block$usage)) {
    usage <- wrap_string(object_usage(block$object))
  } else if (block$usage == "NULL") {
    usage <- NULL
  } else {
    # Treat user input as already escaped, otherwise they have no way
    # to enter \S4method etc.
    usage <- rd(block$usage)
  }
  topic$add_field(roxy_field("usage", usage))
}


# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
process_examples <- function(block, base_path) {
  out <- list()
  if (!is.null(block$examples)) {
    out <- c(out, process_tag(block, "examples"))
  }

  paths <- unlist(block[names(block) == "example"])
  if (length(paths) > 0) {
    paths <- file.path(base_path, str_trim(paths))

    # Check that haven't accidentally used example instead of examples
    nl <- str_count(paths, "\n")
    if (any(nl) > 0) {
      return(block_warning(
        block,
        "@example spans multiple lines. Do you want @examples?"
      ))
    }

    examples <- unlist(lapply(paths, readLines))
    examples <- escape_examples(examples)

    out <- c(out, list(roxy_field("examples", examples)))
  }
  out
}

process_section <- function(key, value, block) {
  pieces <- str_split_fixed(value, ":", n = 2)[1, ]

  title <- str_split(pieces[1], "\n")[[1]]
  if (length(title) > 1) {
    return(block_warning(
      block,
      "Section title spans multiple lines: \n", "@section ", title[1]
    ))
  }

  roxy_field("section", list(list(name = pieces[1], content = pieces[2])))
}

process_doc_type <- function(block) {
  doctype <- block$docType

  if (is.null(doctype)) return()
  tags <- list(roxy_field("docType", doctype))

  if (doctype == "package") {
    name <- block$name
    if (!str_detect(name, "-package")) {
      tags <- c(tags, list(roxy_field("alias", package_suffix(name))))
    }
  }

  tags
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

process_slot <- function(block) {
  process_def_tag(block, "slot")
}

process_field <- function(block) {
  process_def_tag(block, "field")
}

process_def_tag <- function(block, tag) {
  tags <- block[names(block) == tag]
  if (length(tags) == 0) return()

  desc <- str_trim(sapply(tags, "[[", "description"))
  names(desc) <- sapply(tags, "[[", "name")

  roxy_field(tag, desc)
}
