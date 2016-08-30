process_inherit_params <- function(topics) {

  # Currently no topological sort, so @inheritParams will only traverse
  # one-level - you can't inherit params that have been inherited from
  # another function (and you can't currently use multiple inherit tags)
  inherit_index <- get_values(topics, "inheritParams")

  name_index <- get_values(topics, "name")

  for (topic_name in names(inherit_index)) {
    topic <- topics[[topic_name]]

    documented <- get_documented_params(topic)

    needed <- get_tag(topic, "formals")$values
    missing <- setdiff(needed, documented)
    if (length(missing) == 0) next

    for (inheritor in inherit_index[[topic_name]]) {
      inherited <- find_params(inheritor, topics, name_index)

      to_add <- intersect(missing, names(inherited))
      if (length(to_add) == 0) next
      missing <- setdiff(missing, names(inherited))

      topic$add(roxy_field("param", inherited[to_add]))
    }
  }

  topics
}

get_documented_params <- function(topic, only_first = FALSE) {
  documented <- names(topic$get_tag("param")$values)
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
    params <- get_tag(topics[[rd_name]], "param")$values
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

# Postprocessing to reset ordering of parameter documentation
fix_params_order <- function(topics) {
  for(topic_name in names(topics)) {
    topic <- topics[[topic_name]]

    # Compute correct ordering of parameter documentation
    # Check what's needed...
    needed <- topic$get_tag("formals")$values

    # (Workaround for dupes that can occur but perhaps shouldn't,
    #  cf. https://github.com/klutometis/roxygen/commit/83d125dce50a072534988787d49ffe206d19b232#commitcomment-6742169)
    needed <- unique(needed)

    # ...and what's documented (here we look only at the first parameter
    # in a multi-parameter documentation)
    documented <- get_documented_params(topic, only_first = TRUE)

    # We operate on indexes to make sure that no documentation is lost during
    # the reordering
    documented_indexes <- seq_along(documented)

    # We compute the indexes in the current documentation in the required order
    # and append everything that's missing in the order found
    required_order <- match(needed, documented)
    required_order <- required_order[!is.na(required_order)]
    required_order <- c(required_order, setdiff(documented_indexes, required_order))

    # Overwrite all param tags to fix order
    param <- topic$get_tag("param")$values[required_order]
    topic$add(roxy_field("param", param), overwrite = TRUE)
  }

  topics
}
