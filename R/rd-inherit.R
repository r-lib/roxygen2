topics_process_inherit_return <- function(topics) {

  inherits_topo <- topics$topo_order(function(x) {
    x$inherits_from("return")
  })

  for (topic_name in inherits_topo) {
    topic <- topics$get(topic_name)
    inherit_return(topic, topics)
  }

  invisible()
}


topics_process_inherit_params <- function(topics) {

  inherits_topo <- topics$topo_order(function(x) {
    x$get_field("inheritParams")$values
  })

  for (topic_name in inherits_topo) {
    topic <- topics$get(topic_name)
    inherit_params(topic, topics)
  }

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

  for (inheritor in topic$get_field("inheritParams")$values) {
    inherited <- find_params(inheritor, topics)

    to_add <- intersect(missing, names(inherited))
    if (length(to_add) == 0) {
      next
    }

    topic$add_simple_field("param", inherited[to_add])
    missing <- setdiff(missing, to_add)
  }
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

  documented
}

find_params <- function(name, topics) {
  topic <- find_topic(name, topics)
  if (is.null(topic)) {
    warning(
      "Failed to find topic '", name, "'",
      call. = FALSE,
      immediate. = TRUE
    )
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

# Inherit parameters -----------------------------------------------------------

inherit_return <- function(topic, topics) {
  # Already has returns, so don't override
  if (topic$has_field("return"))
    return()

  # Try each in turn
  for (inherit_from in topic$inherits_from("return")) {
    inheritee <- find_return(inherit_from, topics)
    if (is.null(inheritee))
      next

    topic$add_simple_field("value", inheritee)
    return()
  }
}


find_return <- function(name, topics) {
  topic <- find_topic(name, topics)
  if (is.null(topic)) {
    warning(
      "Failed to find topic '", name, "'",
      call. = FALSE,
      immediate. = TRUE
    )
    return()
  }

  topic_return(topic)
}

topic_return <- function(x) UseMethod("topic_return")
topic_return.Rd <- function(x) {
  value <- get_tags(x, "\\value")[[1]]
  attr(value, "Rd_tag") <- NULL

  str_trim(rd2text(value))
}
topic_return.RoxyTopic <- function(x) {
  x$get_field("value")$values
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
