
topics_process_inherit_params <- function(topics) {

  inherits_topo <- topics$topo_order(function(x) {
    names <- x$get_field("inheritParams")$values
    vapply(names, topics$find_filename, character(1))
  })

  inherits <- topics$simple_values("inheritParams")
  inherits <- inherits[intersect(inherits_topo, names(inherits))]

  for (topic_name in names(inherits)) {
    topic <- topics$get(topic_name)

    documented <- get_documented_params(topic)

    needed <- topic$get_field("formals")$values
    missing <- setdiff(needed, documented)
    if (length(missing) == 0) next

    for (inheritor in inherits[[topic_name]]) {
      inherited <- find_params(inheritor, topics)

      to_add <- intersect(missing, names(inherited))
      if (length(to_add) == 0) next
      missing <- setdiff(missing, names(inherited))

      topic$add_simple_field("param", inherited[to_add])
    }
  }

  invisible()
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

# Find info in Rd or topic ------------------------------------------------

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
