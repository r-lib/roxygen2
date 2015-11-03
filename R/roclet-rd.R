#' @include tag-registry.R
#' @import stringr
NULL

register_tags(
  aliases = parse.value,
  author = parse.value,
  backref = parse.value,
  concept = parse.value,
  describeIn = parse.name.description,
  description = parse.value,
  details = parse.value,
  docType = parse.name,
  encoding = parse.value,
  evalRd = parse.code,
  example = parse.value,
  examples = parse.examples,
  family = parse.value,
  field = parse.name.description,
  format = parse.value,
  inheritParams = parse.value,
  keywords = parse.value,
  method = parse.name.description,
  name = parse.value,
  noRd = parse.toggle,
  note = parse.value,
  param = parse.name.description,
  rdname = parse.value,
  rawRd = parse.value,
  references = parse.value,
  return = parse.value,
  section = parse.value,
  seealso = parse.value,
  slot = parse.name.description,
  source = parse.value,
  title = parse.value,
  usage = parse.value
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
    new <- block_to_rd(block, base_path, parsed$env)
    if (is.null(new)) next

    old <- topics[[new$filename]]
    topics[[new$filename]] <- merge.rd_file(old, new$rd)
  }

  topics <- process_family(topics)
  topics <- process_inherit_params(topics)
  fix_params_order(topics)
}

block_to_rd <- function(block, base_path, env) {
  # Must start by processing templates
  block <- process_templates(block, base_path)

  # Does this block get an Rd file?
  if (any(names(block) == "noRd")) {
    return()
  }

  key_tags <- c("description", "param", "return", "title", "example",
    "examples", "name", "rdname", "usage", "details", "introduction",
    "describeIn")
  if (!any(names(block) %in% key_tags)) {
    return()
  }

  rd <- new_rd_file()

  # Determine name
  name <- block$name %||% default_topic_name(block$object) %||%
    stop("Missing name at ", srcref_location(block$srcref), call. = FALSE)
  add_tag(rd, new_tag("name", name))

  # Add backreference to source
  if (!is.null(block$backref)) {
    add_tag(rd, process_tag(block, "backref"))
  } else {
    add_tag(rd, new_tag("backref", block$srcref$filename))
  }

  if (is.function(block$object$value)) {
    formals <- formals(block$object$value)
    add_tag(rd, new_tag("formals", names(formals)))
  }

  # Note that order of operations here doesn't matter: always reordered
  # by format.rd_file
  add_tag(rd, new_tag("encoding", block$encoding))
  add_tag(rd, process_alias(block, name, block$object$alias))
  add_tag(rd, process_methods(block))
  add_tag(rd, process_usage(block))
  add_tag(rd, process_param(block))
  add_tag(rd, process_slot(block))
  add_tag(rd, process_field(block))
  add_tag(rd, process_doc_type(block))
  add_tag(rd, process_tag(block, "rawRd"))
  add_tag(rd, process_tag(block, "evalRd", function(tag, param) {
    expr <- parse(text = param)
    out <- eval(expr, envir = env)
    new_tag("rawRd", as.character(out))
  }))
  add_tag(rd, process_tag(block, "title"))
  add_tag(rd, process_tag(block, "description"))
  add_tag(rd, process_tag(block, "details"))
  add_tag(rd, process_tag(block, "note"))
  add_tag(rd, process_tag(block, "family"))
  add_tag(rd, process_tag(block, "inheritParams"))
  add_tag(rd, process_tag(block, "author"))
  add_tag(rd, process_tag(block, "format"))
  add_tag(rd, process_tag(block, "source"))
  add_tag(rd, process_tag(block, "seealso"))
  add_tag(rd, process_tag(block, "references"))
  add_tag(rd, process_tag(block, "concept"))
  add_tag(rd, process_tag(block, "reexport"))
  add_tag(rd, process_tag(block, "return", function(tag, param) {
    new_tag("value", param)
  }))
  add_tag(rd, process_tag(block, "keywords", function(tag, param) {
    new_tag("keyword", str_split(str_trim(param), "\\s+")[[1]])
  }))
  add_tag(rd, process_tag(block, "section", process_section))
  add_tag(rd, process_examples(block, base_path))

  describe_in <- process_describe_in(block, env)
  add_tag(rd, describe_in$tag)
  filename <- paste0(describe_in$rdname %||% block$rdname %||%
    nice_name(name), ".Rd")

  list(rd = rd, filename = filename)
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


# Tag processing functions ------------------------------------------------

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
process_examples <- function(block, base_path) {
  out <- list()
  if (!is.null(block$examples)) {
    out <- c(out, new_tag("examples", block$examples))
  }

  paths <- unlist(block[names(block) == "example"])
  if (length(paths) > 0) {
    paths <- file.path(base_path, str_trim(paths))
    examples <- unlist(lapply(paths, readLines))
    examples <- escape_examples(examples)

    out <- c(out, new_tag("examples", examples))
  }
  out
}

process_section <- function(key, value) {
  pieces <- str_split_fixed(value, ":", n = 2)[1, ]

  if (str_detect(pieces[1], "\n")) {
    stop("Section title spans multiple lines: \n",
      "@section ", value, call. = FALSE)
  }

  new_tag("section", list(list(name = pieces[1], content = pieces[2])))
}

process_doc_type <- function(block) {
  doctype <- block$docType

  if (is.null(doctype)) return()
  tags <- list(new_tag("docType", doctype))

  if (doctype == "package") {
    name <- block$name
    if (!str_detect(name, "-package")) {
      tags <- c(tags, list(new_tag("alias", package_suffix(name))))
    }
  }

  tags
}

package_suffix <- function(name) {
  paste0(name, "-package")
}

process_tag <- function(block, tag, f = new_tag) {
  matches <- block[names(block) == tag]
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
