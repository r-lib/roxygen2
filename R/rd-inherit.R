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
  documented <- get_documented_params(topic)
  needed <- topic$get_field("formals")$values

  missing <- setdiff(needed, documented)
  if (length(missing) == 0) {
    return()
  }

  for (inheritor in topic$inherits_from("params")) {
    inherited <- find_params(inheritor, topics)

    to_add <- intersect(missing, names(inherited))
    if (length(to_add) == 0) {
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
  args <- Map(select_args_text, funs, inheritors$args)

  # Then pull out the ones we need
  docs <- lapply(inheritors$source, find_params, topics = topics)
  arg_matches <- function(args, docs) {
    doc_args <- str_split(names(docs), ", ?")
    match <- vapply(doc_args, function(x) x %in% args, logical(1))
    docs[match]
  }
  docs_selected <- unlist(Map(arg_matches, args, docs))

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
      documented <- vapply(documented, `[[`, character(1), 1L)
    else
      documented <- unlist(documented)
  }

  documented[documented == "\\dots"] <- "..."
  documented
}

find_params <- function(name, topics) {
  topic <- check_topic(name, topics)
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
  reps <- vapply(individual_names, length, integer(1))

  setNames(rep.int(params, reps), unlist(individual_names))
}

topic_params <- function(x) UseMethod("topic_params")
topic_params.Rd <- function(x) {
  arguments <- get_tags(x, "\\arguments")[[1]]
  items <- get_tags(arguments, "\\item")

  values <- vapply(items, function(x) rd2text(x[[2]]), character(1))
  params <- vapply(items, function(x) rd2text(x[[1]]), character(1))

  setNames(values, params)
}
topic_params.RoxyTopic <- function(x) {
  x$get_field("param")$values
}


# Inherit sections --------------------------------------------------------

inherit_sections <- function(topic, topics) {
  current_secs <- topic$get_field("section")$title

  for (inheritor in topic$inherits_from("sections")) {
    inheritor <- check_topic(inheritor, topics)
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
    inheritor <- check_topic(sources[[i]], topics)
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

    titles <- vapply(lapply(tag, `[[`, 1), rd2text, character(1))
    contents <- vapply(lapply(tag, `[[`, 2), rd2text, character(1))

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
    inherit_topic <- check_topic(inherit_from, topics)
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

find_topic <- function(name, topics) {
  if (has_colons(name)) {
    tryCatch({
      parsed <- parse(text = name)[[1]]
      pkg <- as.character(parsed[[2]])
      fun <- as.character(parsed[[3]])

      get_rd(fun, pkg)
    }, error = function(e) {
      NULL
    })
  } else {
    # Reference within this package
    rd_name <- topics$find_filename(name)
    topics$get(rd_name)
  }
}

check_topic <- function(name, topic) {
  topic <- find_topic(name, topic)
  if (is.null(topic)) {
    warning(
      "Failed to find topic '", name, "'",
      call. = FALSE,
      immediate. = TRUE
    )
  }

  topic
}
