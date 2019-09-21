#' @export
roxy_tag_parse.roxy_tag_param <- function(x) {
  tag_name_description(x)
}

#' @export
roxy_tag_rd.roxy_tag_param <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  roxy_field_simple(x$tag, value)
}

#' @export
merge.roxy_field_param <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  # When parameters appear in both x and y, keep values from y
  # This happens for example when inherit_dot_params adds a "..." param after
  # inherit_params has done the same.
  to_add <- setdiff(names(x$values), names(y$values))
  roxy_field_simple(x$field, c(x$values[to_add], y$values))
}

#' @export
format.roxy_field_param <- function(x, ..., wrap = TRUE) {
  names <- names(x$values)

  # add space to multiple arguments so they can wrap
  names <- gsub(",", ", ", names)

  items <- paste0("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  if (wrap) {
    items <- str_wrap(items, width = 60, exdent = 2, indent = 2)
  }

  rd_macro("arguments", items, space = TRUE)
}

# Other helpers -----------------------------------------------------------

# Postprocessing to reset ordering of parameter documentation
topics_fix_params_order <- function(topics) {
  for (topic in topics$topics) {
    # Compute correct ordering of parameter documentation
    # Check what's needed...
    needed <- topic$get_field("formals")$values

    # (Workaround for dupes that can occur but perhaps shouldn't,
    #  cf. https://github.com/r-lib/roxygen2/commit/83d125dce50a072534988787d49ffe206d19b232#commitcomment-6742169)
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

    # Overwrite all param fields to fix order
    param <- topic$get_field("param")$values[required_order]
    topic$add_simple_field("param", param, overwrite = TRUE)
  }

  invisible()
}
