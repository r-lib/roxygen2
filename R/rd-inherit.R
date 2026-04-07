# Tags --------------------------------------------------------------------

#' @export
roxy_tag_parse.roxy_tag_inherit <- function(x) tag_inherit(x)
#' @export
roxy_tag_rd.roxy_tag_inherit <- function(x, base_path, env) {
  rd_section_inherit(x$val$source, list(x$val$fields))
}

#' @export
roxy_tag_parse.roxy_tag_inheritParams <- function(x) tag_value(x)
#' @export
roxy_tag_rd.roxy_tag_inheritParams <- function(x, base_path, env) {
  rd_section_inherit(x$val, list("params"))
}

#' @export
roxy_tag_parse.roxy_tag_inheritDotParams <- function(x) {
  tag_two_part(
    x,
    "a source",
    "an argument list",
    required = FALSE,
    markdown = FALSE
  )
}
#' @export
roxy_tag_rd.roxy_tag_inheritDotParams <- function(x, base_path, env) {
  rd_section_inherit_dot_params(x$val$name, x$val$description)
}

#' @export
roxy_tag_parse.roxy_tag_inheritSection <- function(x) {
  tag_two_part(x, "a topic name", "a section title")
}
#' @export
roxy_tag_rd.roxy_tag_inheritSection <- function(x, base_path, env) {
  rd_section_inherit_section(x$val$name, x$val$description)
}


# Fields ------------------------------------------------------------------

# For each unique source, list which fields it inherits from
rd_section_inherit <- function(source, fields) {
  check_character(source)
  stopifnot(is.list(fields))
  stopifnot(!anyDuplicated(source))
  stopifnot(length(source) == length(fields))

  rd_section("inherit", list(source = source, fields = fields))
}

#' @export
merge.rd_section_inherit <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))

  dedup <- collapse(
    c(x$value$source, y$value$source),
    c(x$value$fields, y$value$fields),
    \(x) Reduce(union, x)
  )

  rd_section("inherit", list(source = dedup$key, fields = dedup$value))
}

#' @export
format.rd_section_inherit <- function(x, ...) NULL

rd_section_inherit_section <- function(source, title) {
  check_character(source)
  check_character(title)
  stopifnot(length(source) == length(title))

  rd_section("inherit_section", list(source = source, title = title))
}

#' @export
format.rd_section_inherit_section <- function(x, ...) NULL

#' @export
merge.rd_section_inherit_section <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  rd_section_inherit_section(
    c(x$value$source, y$value$source),
    c(x$value$title, y$value$title)
  )
}

rd_section_inherit_dot_params <- function(source, args) {
  check_character(source)
  check_character(args)
  stopifnot(length(source) == length(args))

  rd_section("inherit_dot_params", list(source = source, args = args))
}

#' @export
format.rd_section_inherit_dot_params <- function(x, ...) NULL

#' @export
merge.rd_section_inherit_dot_params <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  rd_section_inherit_dot_params(
    c(x$value$source, y$value$source),
    c(x$value$args, y$value$args)
  )
}


# Process inheritance -----------------------------------------------------

topics_process_inherit <- function(topics, env) {
  inherits <- function(type) {
    \(x) x$inherits_from(type)
  }

  topics$topo_apply(
    inherits("return"),
    inherit_field,
    roxy_name = "return",
    rd_name = "value"
  )
  topics$topo_apply(inherits("title"), inherit_field, "title")
  topics$topo_apply(inherits("description"), inherit_field, "description")
  topics$topo_apply(inherits("details"), inherit_field, "details")
  topics$topo_apply(inherits("seealso"), inherit_field, "seealso")
  topics$topo_apply(inherits("references"), inherit_field, "references")
  topics$topo_apply(inherits("examples"), inherit_field, "examples")
  topics$topo_apply(inherits("author"), inherit_field, "author")
  topics$topo_apply(inherits("source"), inherit_field, "source")
  topics$topo_apply(inherits("note"), inherit_field, "note")
  topics$topo_apply(inherits("note"), inherit_field, "format")

  # First inherit individual sections, then all sections.
  topics$topo_apply(function(x) x$inherits_section_from(), inherit_section)
  topics$topo_apply(inherits("sections"), inherit_sections)

  topics$topo_apply(inherits("params"), inherit_params)
  # Can't inherit ... into ... so can do in any order
  topics$apply(inherit_dot_params, env = env)

  invisible()
}

# Inherit parameters -----------------------------------------------------------

inherit_params <- function(topic, topics) {
  inheritors <- topic$inherits_from("params")
  if (length(inheritors) == 0) {
    return()
  }

  documented <- get_documented_params(topic)
  needed <- topic$get_value("formals")
  missing <- setdiff(needed, documented)
  if (length(missing) == 0) {
    warn_roxy_topic(
      topic$get_name(),
      c(
        x = "@inheritParams failed",
        i = "All parameters are already documented; none remain to be inherited."
      )
    )
    return()
  }

  # Work through inherited params seeing if any match the parameters
  # we're missing
  for (inheritor in inheritors) {
    inherited_params <- find_params(
      inheritor,
      topics,
      source = topic$get_name(),
      tag = "@inheritParams"
    )

    for (param in inherited_params) {
      match <- match_param(param$name, missing)
      if (!is.null(match)) {
        param_val <- setNames(param$value, paste(match, collapse = ","))
        topic$add(rd_section("param", param_val))
        missing <- setdiff(missing, match)
      }
    }
    if (length(missing) == 0) break
  }
}

# Ignore . prefix since it's sometimes necessary to add because a
# function uses ...

# Match parameters ignoring dots
match_param <- function(from, to) {
  flip_dot <- function(x) {
    has_dot <- grepl("^\\.", x)
    ifelse(has_dot, gsub("^\\.", "", x), paste0(".", x))
  }

  to_std <- c(to, flip_dot(to))
  if (!all(from %in% to_std)) {
    return(NULL)
  }

  union(
    setdiff(to[match(from, to)], NA),
    setdiff(to[match(from, flip_dot(to))], NA)
  )
}

inherit_dot_params <- function(topic, topics, env) {
  inheritors <- topic$get_value("inherit_dot_params")
  if (is.null(inheritors)) {
    return()
  }

  # Find documented params for each source — this determines which args are
  # available, matching how @inheritParams uses docs rather than formals.
  docs <- lapply(
    inheritors$source,
    find_params,
    topics = topics,
    source = topic$get_name(),
    tag = "@inheritDotParams"
  )

  # Get unique documented param names for arg selection
  doc_args <- lapply(docs, function(d) {
    unlist(lapply(d, "[[", "name"))
  })
  args <- map2(
    doc_args,
    inheritors$args,
    select_args_text,
    topic_name = topic$get_name()
  )

  arg_matches <- function(args, docs) {
    matched_names <- lapply(docs, \(x) match_param(x$name, args))
    match <- !map_lgl(matched_names, is.null)
    setNames(
      lapply(docs[match], "[[", "value"),
      map_chr(matched_names[match], paste, collapse = ",")
    )
  }
  docs_selected <- unlist(map2(args, docs, arg_matches))

  # Only document params under "..." that aren't otherwise documented
  documented <- get_documented_params(topic)
  non_documented_params <- setdiff(names(docs_selected), documented)
  docs_selected <- docs_selected[non_documented_params]

  if (length(docs_selected) == 0) {
    warn_roxy_topic(
      topic$get_name(),
      c(
        x = "@inheritDotParams failed",
        i = "No arguments inherited from {.fn {inheritors$source}}."
      )
    )
    return()
  }

  # Build the Rd
  # (1) Link to function(s) that was inherited from
  src <- inheritors$source
  from <- map_chr(src, function(x) {
    if (is_namespaced(x)) {
      parts <- re_split_half(x, "::")
      rd_link(parts[1], parts[2], x, code = TRUE)
    } else {
      rd_link(NA_character_, x, x, code = TRUE)
    }
  })
  from <- paste0(from, collapse = ", ")

  # (2) Show each inherited argument
  arg_names <- paste0("\\code{", names(docs_selected), "}")
  args <- paste0(
    "    \\item{",
    arg_names,
    "}{",
    docs_selected,
    "}",
    collapse = "\n"
  )

  # NOTE: this rd structure is used by RStudio for parameter completions
  rd <- paste0(
    "\n",
    "  Arguments passed on to ",
    from,
    "\n",
    "  \\describe{\n",
    args,
    "\n",
    "  }"
  )
  topic$add(rd_section("param", c("..." = rd)))
}


get_documented_params <- function(topic, only_first = FALSE) {
  documented <- names(topic$get_value("param"))
  if (length(documented) > 0) {
    documented <- strsplit(documented, ",")
    if (only_first) {
      documented <- map_chr(documented, \(x) x[[1]])
    } else {
      documented <- unlist(documented)
    }
  }

  documented[documented == "\\dots"] <- "..."
  documented
}

find_params <- function(name, topics, source, tag = "@inherits") {
  topic <- get_rd(name, topics, source = source, tag = tag)
  if (is.null(topic)) {
    return()
  }

  params <- topic_params(topic)
  if (is.null(params)) {
    return()
  }

  param_names <- trimws(names(params))
  param_names <- strsplit(param_names, ",\\s*")
  param_names <- lapply(param_names, function(x) {
    x[x == "\\dots"] <- "..."
    x
  })

  Map(list, name = param_names, value = unlist(params))
}

topic_params <- function(x) {
  if (inherits(x, "Rd")) {
    arguments <- get_tags(x, "\\arguments")
    if (length(arguments) != 1) {
      return(list())
    }
    items <- get_tags(arguments[[1]], "\\item")

    values <- map_chr(items, \(y) rd2text(y[[2]], attr(x, "package")))
    params <- map_chr(items, \(y) rd2text(y[[1]], attr(x, "package")))

    setNames(values, params)
  } else {
    x$get_value("param")
  }
}

# Inherit sections --------------------------------------------------------

inherit_sections <- function(topic, topics) {
  current_secs <- topic$get_value("section")$title

  for (inheritor in topic$inherits_from("sections")) {
    inheritor <- get_rd(inheritor, topics, source = topic$get_name())
    if (is.null(inheritor)) {
      return()
    }

    sections <- find_sections(inheritor)
    needed <- !(sections$title %in% current_secs)
    if (!any(needed)) {
      next
    }

    topic$add(
      rd_section_section(sections$title[needed], sections$content[needed])
    )
  }
}

inherit_section <- function(topic, topics) {
  sections <- topic$get_value("inherit_section")
  sources <- sections$source
  titles <- sections$title

  for (i in seq_along(sources)) {
    inheritor <- get_rd(sources[[i]], topics, source = topic$get_name())
    if (is.null(inheritor)) {
      return()
    }

    new_section <- find_sections(inheritor)
    selected <- new_section$title %in% titles[[i]]

    if (sum(selected) != 1) {
      warn_roxy_topic(
        topic$get_name(),
        "@inheritSection failed to find section {.str {titles[[i]]}} in topic {sources[[i]]}"
      )
      return()
    }

    topic$add(
      rd_section_section(
        new_section$title[selected],
        new_section$content[selected]
      )
    )
  }
}

find_sections <- function(topic) {
  if (inherits(topic, "Rd")) {
    tag <- get_tags(topic, "\\section")

    titles <- map_chr(
      map(tag, \(x) x[[1]]),
      rd2text,
      package = attr(topic, "package")
    )
    contents <- map_chr(
      map(tag, \(x) x[[2]]),
      rd2text,
      package = attr(topic, "package")
    )

    list(title = titles, content = contents)
  } else {
    topic$get_value("section")
  }
}


# Inherit from single field ----------------------------------------------------

inherit_field <- function(topic, topics, rd_name, roxy_name = rd_name) {
  # Already has the field, so don't need to inherit
  if (topic$has_section(rd_name)) {
    return()
  }

  # Otherwise, try each try function listed in inherits
  for (inherit_from in topic$inherits_from(roxy_name)) {
    inherit_topic <- get_rd(inherit_from, topics, source = topic$get_name())
    if (is.null(inherit_topic)) {
      next
    }

    inheritee <- find_field(inherit_topic, rd_name)
    if (is.null(inheritee)) {
      next
    }

    topic$add(rd_section(rd_name, inheritee))
    return()
  }
}

find_field <- function(topic, field_name) {
  if (inherits(topic, "Rd")) {
    tag <- get_tags(topic, paste0("\\", field_name))
    if (length(tag) == 0) {
      return()
    }

    value <- tag[[1]]
    attr(value, "Rd_tag") <- NULL

    trimws(rd2text(value, attr(topic, "package")))
  } else {
    topic$get_value(field_name)
  }
}

rd2text <- function(x, package) {
  x <- tweak_links(x, package)
  chr <- as_character_rd(structure(x, class = "Rd"), deparse = TRUE)
  paste(chr, collapse = "")
}

# Convert relative to absolute links
tweak_links <- function(x, package) {
  tag <- attr(x, "Rd_tag")

  if (is.list(x)) {
    if (!is.null(tag) && tag == "\\link") {
      opt <- attr(x, "Rd_option")
      if (is.null(opt)) {
        if (has_topic(x[[1]], package)) {
          attr(x, "Rd_option") <- structure(package, Rd_tag = "TEXT")
        }
      } else if (is_string(opt) && substr(opt, 1, 1) == "=") {
        topic <- substr(opt, 2, nchar(opt))

        if (has_topic(topic, package)) {
          attr(x, "Rd_option") <- structure(
            paste0(package, ":", topic),
            Rd_tag = "TEXT"
          )
        }
      }
    } else if (!is.null(tag) && tag == "\\linkS4class") {
      topic <- paste0(x[[1]], "-class")
      if (has_topic(topic, package)) {
        attr(x, "Rd_tag") <- "\\link"
        attr(x, "Rd_option") <- structure(
          paste0(package, ":", topic),
          Rd_tag = "TEXT"
        )
      }
    } else if (length(x) > 0) {
      x[] <- map(x, tweak_links, package = package)
    }
  }

  x
}


# Find info in Rd or topic ------------------------------------------------

get_rd <- function(name, topics, source, tag = "@inherits") {
  if (is_namespaced(name)) {
    # External package
    parsed <- parse_expr(name)
    pkg <- as.character(parsed[[2]])
    fun <- as.character(parsed[[3]])

    get_rd_from_help(pkg, fun, source, tag = tag)
  } else {
    # Current package
    rd_name <- topics$find_filename(name)
    if (identical(rd_name, NA_character_)) {
      warn_roxy_topic(
        source,
        "{tag} failed to find topic {.str {name}} in current package"
      )
    }
    topics$get(rd_name)
  }
}

get_rd_from_help <- function(package, alias, source, tag = "@inherits") {
  if (!is_installed(package)) {
    warn_roxy_topic(
      source,
      "{tag} failed because {.pkg {package}} is not installed"
    )
    return()
  }

  help <- utils::help((alias), (package))
  if (length(help) == 0) {
    warn_roxy_topic(source, "{tag} failed to find topic {package}::{alias}")
    return()
  }

  out <- internal_f("utils", ".getHelpFile")(help)
  attr(out, "package") <- package
  out
}
