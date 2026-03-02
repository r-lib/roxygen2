#' @export
roxy_tag_parse.roxy_tag_param <- function(x) {
  tag_two_part(x, "an argument name", "a description")
}

#' @export
roxy_tag_rd.roxy_tag_param <- function(x, base_path, env) {
  value <- setNames(x$val$description, x$val$name)
  rd_section(x$tag, value)
}

#' @export
merge.rd_section_param <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  # When parameters appear in both x and y, keep values from y
  # This happens for example when inherit_dot_params adds a "..." param after
  # inherit_params has done the same.
  to_add <- setdiff(names(x$value), names(y$value))
  rd_section(x$type, c(x$value[to_add], y$value))
}

#' @export
format.rd_section_param <- function(x, ...) {
  names <- names(x$value)

  # add space to multiple arguments so they can wrap
  names <- gsub(",", ", ", names)
  items <- paste0("\\item{", names, "}{", x$value, "}", collapse = "\n\n")

  rd_macro("arguments", items, space = TRUE)
}

# Other helpers -----------------------------------------------------------

# Postprocessing to reset ordering of parameter documentation
topics_fix_params_order <- function(topics) {
  for (topic in topics$topics) {
    # Compute correct ordering of parameter documentation
    # Check what's needed...
    needed <- topic$get_value("formals")

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
    required_order <- c(
      required_order,
      setdiff(documented_indexes, required_order)
    )

    # Overwrite all param fields to fix order
    param <- topic$get_value("param")[required_order]
    topic$add(rd_section("param", param), overwrite = TRUE)
  }

  invisible()
}
