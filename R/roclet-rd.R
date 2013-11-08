#' @include parse-registry.R
#' @import stringr
NULL

register.preref.parsers(parse.value,
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
                        'examples',
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

register.preref.parsers(parse.name.description,
                        'param',
                        'method')

register.preref.parsers(parse.name,
                        'docType')

register.preref.parsers(parse.default,
                        'noRd')



#' Roclet: make Rd files.
#'
#' This roclet is the workhorse of \pkg{roxygen}, producing the Rd files that
#' document that functions in your package.
#'
#' This roclet also automatically runs \code{\link[tools]{checkRd}} on all
#' generated Rd files so that you know as early as possible if there's a
#' problem.
#'
#' @section Required tags:
#'
#' As well as a title and description, extracted from the first sentence and
#' first paragraph respectively, all functions must have the following tags:
#'
#' \describe{
#'
#'  \item{\code{@@param name description}}{Document a parameter.
#'    Documentation is required for every parameter.}
#'
#'  \item{\code{@@inheritParams source_function}}{Alternatively, you can
#'    inherit parameter description from another function. This tag will
#'    bring in all documentation for parameters that are undocumented in the
#'    current function, but documented in the source function. The source
#'    can be a function in the current package, \code{function}, or another
#'    package \code{package::function}.}
#'
#'  \item{\code{@@method generic class}}{Required if your function is an S3
#'    method.  This helps R to distinguish between (e.g.) \code{t.test} and
#'    the \code{t} method for the \code{test} class.}
#'
#'  }
#'
#' @section Optional tags that add extra information:
#'
#' Valid tags for \code{rd_roclet} are:
#'
#' \describe{
#'
#'  \item{\code{@@examples R code...}}{Highly recommended: example code that
#'    demonstrates how to use your function.  Use \\dontrun{} to tag code
#'    that should not automatically be run.}
#'
#'  \item{\code{@@example path/relative/to/packge/root}}{Instead of including
#'    examples directly in the documentation, you can include them as
#'    separate files, and use the \code{@@example} tag to insert them into
#'    the documentation.}
#'
#'  \item{\code{@@return}}{Used to document the object returned by the
#'    function. For lists, use the \code{\\item{name a}{description a}}
#'    describe each component of the list}
#'
#'  \item{\code{@@author authors...}}{A free text string describing the
#'    authors of the function.  This is typically only necessary if the
#'    author is not the same as the package author.}
#'
#'  \item{\code{@@note contents}}{Create a note section containing additional
#'    information.}
#'
#'  \item{\code{@@section Name: contents}}{Use to add to an arbitrary section
#'    to the documentation. The name of the section will be the content before
#'    the first colon, and the contents will be everything after the colon.}
#'
#'  \item{\code{@@keywords keyword1 keyword2 ...}}{Keywords are optional,
#'    but if present, must be taken from the list in
#'    \file{file.path(R.home(), "doc/KEYWORDS")}.  Use the \code{internal}
#'    keyword for functions that should not appear in the main function
#'    listing.}
#'
#'  }
#'
#' @section Optional tags for cross-referencing:
#'
#' \describe{
#'
#'  \item{\code{@@aliases space separated aliases}}{Add additional aliases,
#'    through which the user can find the documentation with \code{?}.
#'    The topic name is always included in the list of aliases.}
#'
#'  \item{\code{@@concepts space separated concepts}}{Similar to
#'    \code{@@aliases} but for \code{\link{help.search}}}
#'
#'  \item{\code{@@references free text reference}}{Pointers to the literature
#'    related to this object.}
#'
#'  \item{\code{@@seealso Text with \\code{\\link{function}}}}{Pointers to
#'    related R objects, and why you might be interested in them.}
#'
#'  \item{\code{@@family family name}}{Automatically adds see-also
#'     cross-references between all functions in a family. A function can
#'     belong to multiple families.}
#'  }
#'
#' @template template
#' @section Optional tags that override defaults:
#'
#' These tags all override the default values that roxygen guess from
#' inspecting the source code.
#'
#' \describe{
#'
#'  \item{\code{@@rdname filename}}{Overrides the output file name (without
#'    extension). This is useful if your function has a name that is not
#'    a valid filename (e.g. \code{[[<-}), or you want to merge documentation
#'    for multiple function into a single file.}
#'
#'  \item{\code{@@title Topic title}}{Specify the topic title, which by
#'    by default is taken from the first sentence of the roxygen block.}
#'
#'  \item{\code{@@usage usage_string}}{Override the default usage string.
#'    You should not need to use this tag - if you are trying to document
#'    multiple functions in the same topic, use \code{@@rdname}.}
#'
#'  }
#'
#' @section Tags for non-functions:
#'
#' These tags are useful when documenting things that aren't functions,
#' datasets and packages.
#'
#' \describe{
#'
#'  \item{\code{@@name topicname}}{Override the default topic name, which is
#'    taken by default from the object that is assigned to in the code
#'    immediately following the roxygen block. This tag is useful when
#'    documenting datasets, and other non-function elements.}
#'
#'  \item{\code{@@docType type}}{Type of object being documented. Useful
#'    values are \code{data} and \code{package}. Package doc type will
#'    automatically add a \code{package-} alias if needed.}
#'
#'  \item{\code{@@format description}}{A textual description of the format
#'    of the object.}
#'
#'  \item{\code{@@source text}}{The original source of the data.}
#'
#'}
#' @family roclets
#' @examples
#' roclet <- rd_roclet()
#' \dontrun{roc_proc(roclet, "example.R")}
#' \dontrun{roc_out(roclet, "example.R", ".")}
#' @export
rd_roclet <- function() {
  new_roclet(list(), "had")
}

#' @export
#' @importFrom digest digest
roc_process.had <- function(roclet, partita, base_path) {
  # Remove srcrefs with no attached roxygen comments
  partita <- Filter(function(x) length(x) > 1, partita)
  templates <- dir(file.path(base_path, "max-roxygen"), full.names = TRUE)
  template_hash <- digest(lapply(templates, readLines))

  topics <- list()
  for (partitum in partita) {
    key <- c(template_hash, digest(partitum))
    new <- rd_proc_cache$compute(key, roclet_rd_one(partitum, base_path))
    if (is.null(new)) next;
    # Clone output so cached object isn't modified
    new$rd[[1]] <- list2env(as.list(new$rd[[1]]))

    old <- topics[[new$filename]]
    topics[[new$filename]] <- if (is.null(old)) new$rd else merge(old, new$rd)
  }

  # Second parse through to process @family
  invert <- function(x) {
    if (length(x) == 0) return()
    unstack(rev(stack(x)))
  }
  get_values <- function(topics, tag) {
    tags <- lapply(topics, get_tag, tag)
    tags <- Filter(Negate(is.null), tags)
    lapply(tags, "[[", "values")
  }

  family_lookup <- invert(get_values(topics, "family"))
  name_lookup <- get_values(topics, "name")

  for(family in names(family_lookup)) {
    related <- family_lookup[[family]]

    for(topic_name in related) {
      topic <- topics[[topic_name]]
      others <- setdiff(related, topic_name)

      if (length(others) < 1) next;

      other_topics <- sort(unlist(name_lookup[others], use.names = FALSE))

      links <- paste("\\code{\\link{", other_topics, "}}",
        collapse =", ", sep = "")
      seealso <- paste("Other ", family, ": ", links, sep = "")

      add_tag(topic, new_tag("seealso", seealso))
    }
  }

  # And to process @inheritParams

  # Currently no topological sort, so @inheritParams will only traverse
  # one-level - you can't inherit params that have been inherited from
  # another function (and you can't currently use multiple inherit tags)
  inherits <- get_values(topics, "inheritParams")

  for(topic_name in names(inherits)) {
    topic <- topics[[topic_name]]

    for(inheritor in inherits[[topic_name]]) {
      if (grepl("::", inheritor, fixed = TRUE)) {
        # Reference to another package
        pieces <- strsplit(inheritor, "::", fixed = TRUE)[[1]]
        params <- rd_arguments(get_rd(pieces[2], pieces[1]))

      } else {
        # Reference within this package
        rd_name <- names(Filter(function(x) inheritor %in% x, name_lookup))

        if (length(rd_name) != 1) {
          warning("@inheritParams: can't find topic ", inheritor,
            call. = FALSE, immediate. = TRUE)
          next
        }
        params <- get_tag(topics[[rd_name]], "arguments")$values
      }
      params <- unlist(params)

      missing_params <- setdiff(get_tag(topic, "formals")$values,
        names(get_tag(topic, "arguments")$values))
      matching_params <- intersect(missing_params, names(params))

      add_tag(topic, new_tag("arguments", params[matching_params]))
    }

  }


  topics
}

roclet_rd_one <- function(partitum, base_path) {
  rd <- new_rd_file()

  # Add in templates
  partitum <- process_templates(partitum, base_path)

  has_rd <- any(names(partitum) %in% c("description", "param", "return",
    "title", "example", "examples", "name", "rdname", "usage",
    "details", "introduction"))
  if (!has_rd) return()

  if (any(names(partitum) == "noRd")) return()

  # Figure out topic name
  name <- partitum$name %||% topic_name(partitum$object) %||% 
    roxygen_stop("Missing name", srcref = partitum$srcref)

  # Work out file name and initialise Rd object
  filename <- str_c(partitum$merge %||% partitum$rdname %||% nice_name(name),
    ".Rd")

  add_tag(rd, new_tag("encoding", partitum$encoding))
  add_tag(rd, new_tag("name", name))
  add_tag(rd, new_tag("alias", name))
  add_tag(rd, new_tag("formals", names(partitum$formals)))

  add_tag(rd, process_description(partitum, base_path))

  add_tag(rd, process_had_tag(partitum, 'aliases', function(tag, param) {
    new_tag('alias', str_split(str_trim(param), "\\s+")[[1]])
  }))
  add_tag(rd, usage_tag(partitum))
  add_tag(rd, process.arguments(partitum))
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
#' @importFrom tools checkRd
roc_output.had <- function(roclet, results, base_path) {
  man <- normalizePath(file.path(base_path, "man"))

  contents <- vapply(results, FUN.VALUE = character(1), function(x) {
    rd_out_cache$compute(x, format(x))
  })

  write_out <- function(filename, contents) {
    if (the_same(filename, contents)) return()

    name <- basename(filename)
    if (!str_detect(name, "^[a-zA-Z][a-zA-Z0-9_.-]*$")) {
      cat("Skipping invalid filename: ", name, "\n")
    } else {
      cat(sprintf('Writing %s\n', name))
      writeLines(contents, filename)
      try(checkRd(filename))
    }

  }
  the_same <- function(path, new) {
    if (!file.exists(path)) return(FALSE)

    old <- str_c(readLines(path), collapse = "\n")
    return(identical(old, new))
  }

  paths <- file.path(man, names(results))
  mapply(write_out, paths, contents)
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

process.arguments <- function(partitum) {
  params <- partitum[names(partitum) == "param"]
  if (length(params) == 0) return()

  desc <- str_trim(sapply(params, "[[", "description"))
  names(desc) <- sapply(params, "[[", "name")

  new_tag("arguments", desc)
}

# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
process.examples <- function(partitum, base_path) {
  out <- list()
  if (!is.null(partitum$examples)) {
    ex <- partitum$examples
    ex <- gsub("([%\\])", "\\\\\\1", ex)
    ex <- gsub("\\\\dont", "\\dont", ex)
    out <- c(out, new_tag("examples", ex))
  }

  paths <- unlist(partitum[names(partitum) == "example"])
  if (length(paths) > 0) {
    paths <- file.path(base_path, str_trim(paths))
    examples <- unlist(lapply(paths, readLines))
    examples <- gsub("([%\\])", "\\\\\\1", examples)

    out <- c(out, new_tag("examples", examples))
  }
  out
}
process.section <- function(key, value) {
  pieces <- str_split_fixed(value, ":", n = 2)[1, ]

  new_tag("section", list(list(name = pieces[1], content = pieces[2])))
}

process.docType <- function(partitum) {
  doctype <- partitum$docType

  if (is.null(doctype)) return()
  tags <- list(new_tag("docType", doctype))

  if (doctype == "package") {
    name <- partitum$name
    if (!str_detect(name, "-package")) {
      tags <- c(tags, new_tag("alias", str_c(name, "-package")))
    }
  } else if (doctype == "data") {
    if (is.null(partitum$format)) {
      tags <- c(tags, new_tag("format", partitum$str))
    }
    tags <- c(tags, new_tag("keyword", "datasets"))
  }

  tags
}

process_had_tag <- function(partitum, tag, f = new_tag) {
  matches <- partitum[names(partitum) == tag]
  if (length(matches) == 0) return()

  unlist(lapply(matches, function(p) f(tag, p)), recursive = FALSE)
}

# warning("All roxygen elements must have name: ",
#   partitum$srcref$filename, ":", partitum$srcref$lloc[1], ":",
#   partitum$srcref$lloc[2], call. = FALSE)
