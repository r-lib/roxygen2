
topics_process_inherit_params <- function(topics) {

  topics$topo_sort(function(x) {
    names <- x$get_field("inheritParams")$values
    vapply(names, topics$find_filename, character(1))
  })

  inherits <- topics$simple_values("inheritParams")
  names <- topics$simple_values("name")

  for (topic_name in names(inherits)) {
    topic <- topics$get(topic_name)

    documented <- get_documented_params(topic)

    needed <- topic$get_field("formals")$values
    missing <- setdiff(needed, documented)
    if (length(missing) == 0) next

    for (inheritor in inherits[[topic_name]]) {
      inherited <- find_params(inheritor, topics, names)

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


find_params <- function(inheritor, topics, name_lookup) {
  has_colons <- grepl("::", inheritor, fixed = TRUE)

  if (has_colons) {
    # Reference to another package
    parsed <- parse(text = inheritor)[[1]]
    pkg <- as.character(parsed[[2]])
    fun <- as.character(parsed[[3]])

    params <- rd_arguments(get_rd(fun, pkg))
  } else {
    # Reference within this package
    rd_name <- names(Filter(function(x) inheritor %in% x, name_lookup))

    if (length(rd_name) != 1) {
      warning("@inheritParams: can't find topic ", inheritor,
        call. = FALSE, immediate. = TRUE)
      return()
    }
    params <- topics$get(rd_name)$get_field("param")$values
  }
  params <- unlist(params)
  if (is.null(params)) return(NULL)

  param_names <- str_trim(names(params))
  param_names[param_names == "\\dots"] <- "..."

  # Split up compound names on , (swallowing spaces) duplicating their contents
  individual_names <- strsplit(param_names, ",\\s*")
  reps <- vapply(individual_names, length, integer(1))

  setNames(rep.int(params, reps), unlist(individual_names))
}
