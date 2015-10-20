# Postprocessing to reset ordering of parameter documentation
fix_params_order <- function(topics) {
  for(topic_name in names(topics)) {
    topic <- topics[[topic_name]]

    # Compute correct ordering of parameter documentation
    # Check what's needed...
    needed <- get_tag(topic, "formals")$values

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
    add_tag(topic, new_tag("param", get_tag(topic, "param")$values[required_order]),
            overwrite = TRUE)
  }

  topics
}
