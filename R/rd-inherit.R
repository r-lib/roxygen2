topics_process_inherit <- function(topics, env) {
  inherits <- function(type) {
    function(x) x$inherits_from(type)
  }

  topics$topo_apply(inherits("return"), inherit_field,
    roxy_name = "return", rd_name = "value")
  topics$topo_apply(inherits("title"), inherit_field, "title")
  topics$topo_apply(inherits("description"), inherit_field, "description")
  topics$topo_apply(inherits("details"), inherit_field, "details")
  topics$topo_apply(inherits("seealso"), inherit_field, "seealso")
  topics$topo_apply(inherits("references"), inherit_field, "references")
  topics$topo_apply(inherits("examples"), inherit_field, "examples")
  topics$topo_apply(inherits("author"), inherit_field, "author")
  topics$topo_apply(inherits("source"), inherit_field, "source")

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
  needed <- topic$get_field("formals")$values
  missing <- setdiff(needed, documented)
  if (length(missing) == 0) {
    warn(paste0(
      "Topic '", topic$get_name(), "': ",
      "no parameters to inherit with @inheritParams"
    ))
    return()
  }

  for (inheritor in inheritors) {
    inherited <- find_params(inheritor, topics)

    to_add <- intersect(missing, names(inherited))
    if (length(to_add) == 0) {
      # Can't warn here because @inherit inherits parameters
      next
    }

    topic$add_simple_field("param", inherited[to_add])
    missing <- setdiff(missing, to_add)
  }
}

inherit_dot_params <- function(topic, topics, env) {
  inheritors <- topic$get_field("inherit_dot_params")
  if (is.null(inheritors))
    return()

  # Need to find formals for each source
  funs <- lapply(inheritors$source, function(x) eval(parse(text = x), envir = env))
  args <- map2(funs, inheritors$args, select_args_text)

  # Then pull out the ones we need
  docs <- lapply(inheritors$source, find_params, topics = topics)
  arg_matches <- function(args, docs) {
    doc_args <- str_split(names(docs), ", ?")
    match <- map_lgl(doc_args, function(x) x %in% args)
    docs[match]
  }
  docs_selected <- unlist(map2(args, docs, arg_matches))

  # Build the arg string
  from <- paste0("\\code{", inheritors$source, "}", collapse = ", ")
  args <- paste0("  \\item{", names(docs_selected), "}{", docs_selected, "}",
    collapse = "\n")

  rd <- paste0(
    "Arguments passed on to ", from, "\n",
    "\\describe{\n",
    args, "\n",
    "}"
  )
  topic$add_simple_field("param", c("..." = rd))
}


get_documented_params <- function(topic, only_first = FALSE) {
  documented <- names(topic$get_field("param")$values)
  if (length(documented) > 0) {
    documented <- strsplit(documented, ",")
    if (only_first)
      documented <- map_chr(documented, 1)
    else
      documented <- unlist(documented)
  }

  documented[documented == "\\dots"] <- "..."
  documented
}

find_params <- function(name, topics) {
  topic <- get_rd(name, topics)
  if (is.null(topic)) {
    return()
  }

  params <- topic_params(topic)
  if (is.null(params))
    return()

  param_names <- str_trim(names(params))
  param_names[param_names == "\\dots"] <- "..."

  # Split up compound names on , (swallowing spaces) duplicating their contents
  individual_names <- strsplit(param_names, ",\\s*")
  reps <- map_int(individual_names, length)

  setNames(rep.int(params, reps), unlist(individual_names))
}

topic_params <- function(x) UseMethod("topic_params")
topic_params.Rd <- function(x) {
  arguments <- get_tags(x, "\\arguments")
  if (length(arguments) != 1) {
    return(list())
  }
  items <- get_tags(arguments[[1]], "\\item")

  values <- map_chr(items, function(x) rd2text(x[[2]]))
  params <- map_chr(items, function(x) rd2text(x[[1]]))

  setNames(values, params)
}
topic_params.RoxyTopic <- function(x) {
  x$get_field("param")$values
}


# Inherit sections --------------------------------------------------------

inherit_sections <- function(topic, topics) {
  current_secs <- topic$get_field("section")$title

  for (inheritor in topic$inherits_from("sections")) {
    inheritor <- get_rd(inheritor, topics)
    if (is.null(inheritor)) {
      return()
    }

    sections <- find_sections(inheritor)
    needed <- !(sections$title %in% current_secs)
    if (!any(needed))
      next

    topic$add_field(
      roxy_field_section(sections$title[needed], sections$content[needed])
    )
  }
}

inherit_section <- function(topic, topics) {
  sections <- topic$get_field("inherit_section")
  sources <- sections$source
  titles <- sections$title

  for (i in seq_along(sources)) {
    inheritor <- get_rd(sources[[i]], topics)
    if (is.null(inheritor)) {
      return()
    }

    new_section <- find_sections(inheritor)
    selected <- new_section$title %in% titles[[i]]

    if (sum(selected) != 1) {
      warning(
        "Can't find section '", titles[[i]], "' in ?",
        sources[[i]], call. = FALSE
      )
    }

    topic$add_field(
      roxy_field_section(new_section$title[selected], new_section$content[selected])
    )
  }
}

find_sections <- function(topic) {
  if (inherits(topic, "Rd")) {
    tag <- get_tags(topic, "\\section")

    titles <- map_chr(map(tag, 1), rd2text)
    contents <- map_chr(map(tag, 2), rd2text)

    roxy_field_section(titles, contents)
  } else {
    topic$get_field("section")
  }
}


# Inherit from single field ----------------------------------------------------

inherit_field <- function(topic, topics, rd_name, roxy_name = rd_name) {
  # Already has the field, so don't need to inherit
  if (topic$has_field(rd_name))
    return()

  # Otherwise, try each try function listed in inherits
  for (inherit_from in topic$inherits_from(roxy_name)) {
    inherit_topic <- get_rd(inherit_from, topics)
    if (is.null(inherit_topic)) {
      next
    }

    inheritee <- find_field(inherit_topic, rd_name)
    if (is.null(inheritee))
      next

    topic$add_simple_field(rd_name, inheritee)
    return()
  }
}

find_field <- function(topic, field_name) {
  if (inherits(topic, "Rd")) {
    tag <- get_tags(topic, paste0("\\", field_name))
    if (length(tag) == 0)
      return()

    value <- tag[[1]]
    attr(value, "Rd_tag") <- NULL

    str_trim(rd2text(value))
  } else {
    topic$get_field(field_name)$values
  }
}

# Find info in Rd or topic ------------------------------------------------

get_rd <- function(name, topics) {
  if (has_colons(name)) {
    # External package
    parsed <- parse_expr(name)
    pkg <- as.character(parsed[[2]])
    fun <- as.character(parsed[[3]])

    get_rd_from_help(pkg, fun)
  } else {
    # Current package
    rd_name <- topics$find_filename(name)
    if (identical(rd_name, NA_character_)) {
      warn(paste0("Can't find help topic '", name, "' in current package"))
    }
    topics$get(rd_name)
  }
}

get_rd_from_help <- function(package, alias) {
  if (!is_installed(package)) {
    warn(paste0("Can't find package '", package, "'"))
    return()
  }

  help <- eval(expr(help(!!alias, !!package)))
  if (length(help) == 0) {
    warn(paste0("Can't find help topic '", alias, "' in '", package, "' package"))
    return()
  }

  internal_f("utils", ".getHelpFile")(help)
}
