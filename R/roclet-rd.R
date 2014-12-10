#' @include parse-registry.R
#' @import stringr
NULL

register.preref.parsers(parse.value,
                        'backref',
                        'name',
                        'rdname',
                        'aliases',
                        'title',
                        'usage',
                        'references',
                        'concept',
                        'note',
                        'seealso',
                        'example',
                        'keywords',
                        'return',
                        'author',
                        'section',
                        'family',
                        'inheritParams',
                        'format',
                        'source',
                        'encoding',
                        'description',
                        'details')

register.preref.parsers(parse.examples,
                        "examples")

register.preref.parsers(parse.name.description,
                        'param',
                        'slot',
                        'field',
                        'method',
                        'describeIn')

register.preref.parsers(parse.name,
                        'docType')

register.preref.parsers(parse.toggle,
                        'noRd')


#' Roclet: make Rd files.
#'
#' This roclet is the workhorse of \pkg{roxygen}, producing the Rd files that
#' document that functions in your package.
#'
#' @family roclets
#' @seealso \code{vignette("rd", package = "roxygen2")}
#' @export
rd_roclet <- function() {
  new_roclet(list(), "had")
}

#' @export
roc_process.had <- function(roclet, parsed, base_path, options = list()) {
  env <- parsed$env
  partita <- parsed$blocks

  # Remove srcrefs with no attached roxygen comments
  partita <- Filter(function(x) length(x) > 1, partita)


  topics <- list()
  for (partitum in partita) {
    errors_with_srcref(partitum$srcref, {
      new <- roclet_rd_one(partitum, base_path, env)
    })

    if (is.null(new)) next

    old <- topics[[new$filename]]
    topics[[new$filename]] <- if (is.null(old)) new$rd else merge(old, new$rd)
  }

  # Second parse through to process @family
  topics <- process_family(topics)
  # Final parse to process @inheritParams
  process_inherit_params(topics)
  # Postprocessing to reset ordering of parameter documentation
  fix_params_order(topics)
}

invert <- function(x) {
  if (length(x) == 0) return()
  unstack(rev(stack(x)))
}
get_values <- function(topics, tag) {
  tags <- lapply(topics, get_tag, tag)
  tags <- Filter(Negate(is.null), tags)
  lapply(tags, "[[", "values")
}


roclet_rd_one <- function(partitum, base_path, env) {
  rd <- new_rd_file()

  # Add in templates
  partitum <- process_templates(partitum, base_path)

  has_rd <- any(names(partitum) %in% c("description", "param", "return",
    "title", "example", "examples", "name", "rdname", "usage",
    "details", "introduction", "describeIn"))
  if (!has_rd) return()

  if (any(names(partitum) == "noRd")) return()

  name <- partitum$name %||% default_topic_name(partitum$object) %||%
    stop("Missing name")

  # Process describeIn, which may affect file name
  describe_in <- process_describeIn(partitum, env)
  filename <- paste0(describe_in$rdname %||% partitum$rdname %||%
    nice_name(name), ".Rd")
  add_tag(rd, describe_in$tag)

  # Add source reference as comment
  if (!is.null(partitum$backref))
    add_tag(rd, process_had_tag(partitum, 'backref'))
  else
    add_tag(rd, new_tag("backref", partitum$srcref$filename))

  # Work out file name and initialise Rd object
  add_tag(rd, new_tag("encoding", partitum$encoding))
  add_tag(rd, new_tag("name", name))
  add_tag(rd, alias_tag(partitum, name, partitum$object$alias))

  if (is.function(partitum$object$value)) {
    formals <- formals(partitum$object$value)
    add_tag(rd, new_tag("formals", names(formals)))
  }

  add_tag(rd, process_description(partitum, base_path))
  add_tag(rd, process_methods(partitum))

  add_tag(rd, usage_tag(partitum))
  add_tag(rd, process_param(partitum))
  add_tag(rd, process_slot(partitum))
  add_tag(rd, process_field(partitum))
  add_tag(rd, process.docType(partitum))
  add_tag(rd, process_had_tag(partitum, 'note'))
  add_tag(rd, process_had_tag(partitum, 'family'))
  add_tag(rd, process_had_tag(partitum, 'inheritParams'))
  add_tag(rd, process_had_tag(partitum, 'author'))
  add_tag(rd, process_had_tag(partitum, 'format'))
  add_tag(rd, process_had_tag(partitum, 'source'))
  add_tag(rd, process_had_tag(partitum, 'seealso'))
  add_tag(rd, process_had_tag(partitum, "references"))
  add_tag(rd, process_had_tag(partitum, 'concept'))
  add_tag(rd, process_had_tag(partitum, 'return', function(tag, param) {
      new_tag("value", param)
    }))
  add_tag(rd, process_had_tag(partitum, 'keywords', function(tag, param, all, rd) {
      new_tag("keyword", str_split(str_trim(param), "\\s+")[[1]])
    }))
  add_tag(rd, process_had_tag(partitum, 'section', process.section))
  add_tag(rd, process.examples(partitum, base_path))

  list(rd = rd, filename = filename)
}

#' @export
roc_output.had <- function(roclet, results, base_path, options = list(),
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
clean.had <- function(roclet, base_path) {
  rd <- dir(file.path(base_path, "man"), full.names = TRUE)
  rd <- rd[!file.info(rd)$isdir]
  made_by_me <- vapply(rd, made_by_roxygen, logical(1))

  unlink(rd[made_by_me])
}

# Process title, description and details.
#
# Split the introductory matter into its description followed
# by details (separated by a blank line).
process_description <- function(partitum, base_path) {
  intro <- partitum$introduction

  if (!is.null(intro)) {
    paragraphs <- str_trim(str_split(intro, fixed('\n\n'))[[1]])
  } else {
    paragraphs <- NULL
  }

  # 1st paragraph = title (unless has @title)
  if (!is.null(partitum$title)) {
    title <- partitum$title
  } else if (length(paragraphs) > 0) {
    title <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  } else {
    title <- NULL
  }


  # 2nd paragraph = description (unless has @description)
  if (!is.null(partitum$description)) {
    description <- partitum$description
  } else if (length(paragraphs) > 0) {
    description <- paragraphs[1]
    paragraphs <- paragraphs[-1]
  } else {
    # Description is required, so if missing description, repeat title.
    description <- title
  }

  # Every thing else = details, combined with @details.
  details <- c(paragraphs, partitum$details)
  if (length(details) > 0) {
    details <- paste(details, collapse = "\n\n")
  } else {
    details <- NULL
  }

  c(new_tag("title", title),
    new_tag("description", description),
    new_tag("details", details))
}


process_methods <- function(block) {
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

  new_tag("rcmethods", setNames(desc, usage))
}


# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
process.examples <- function(partitum, base_path) {
  out <- list()
  if (!is.null(partitum$examples)) {
    out <- c(out, new_tag("examples", partitum$examples))
  }

  paths <- unlist(partitum[names(partitum) == "example"])
  if (length(paths) > 0) {
    paths <- file.path(base_path, str_trim(paths))
    examples <- unlist(lapply(paths, readLines))
    examples <- escape_examples(examples)

    out <- c(out, new_tag("examples", examples))
  }
  out
}

process.section <- function(key, value) {
  pieces <- str_split_fixed(value, ":", n = 2)[1, ]

  if (str_detect(pieces[1], "\n")) {
    stop("Section title spans multiple lines: \n",
      "@section ", value, call. = FALSE)
  }

  new_tag("section", list(list(name = pieces[1], content = pieces[2])))
}

process.docType <- function(partitum) {
  doctype <- partitum$docType

  if (is.null(doctype)) return()
  tags <- list(new_tag("docType", doctype))

  if (doctype == "package") {
    name <- partitum$name
    if (!str_detect(name, "-package")) {
      tags <- c(tags, new_tag("alias", paste0(name, "-package")))
    }
  }

  tags
}

process_had_tag <- function(partitum, tag, f = new_tag) {
  matches <- partitum[names(partitum) == tag]
  if (length(matches) == 0) return()

  unlist(lapply(matches, function(p) f(tag, p)), recursive = FALSE)
}

# Name + description tags ------------------------------------------------------

process_param <- function(block) {
  process_def_tag(block, "param")
}

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

  new_tag(tag, desc)
}
