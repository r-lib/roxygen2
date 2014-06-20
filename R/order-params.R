fix_params_order <- function(topics) {
  for(topic_name in names(topics)) {
    topic <- topics[[topic_name]]

    # Compute correct ordering of parameter documentation
    # Check what's needed...
    needed <- get_tag(topic, "formals")$values

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
