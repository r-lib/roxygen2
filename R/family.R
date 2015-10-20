process_family <- function(topics) {
  family_lookup <- invert(get_values(topics, "family"))
  alias_lookup <- get_values(topics, "alias")

  for(family in names(family_lookup)) {
    related <- family_lookup[[family]]

    for(topic_name in related) {
      topic <- topics[[topic_name]]
      others <- setdiff(related, topic_name)

      if (length(others) < 1) next;

      by_file <- vapply(alias_lookup[others], function(x) {
        paste0("\\code{\\link{", escape(x[1]), "}}")
      }, FUN.VALUE = character(1))
      links <- paste(sort_c(by_file), collapse =", ")

      seealso <- paste("Other ", family, ": ", sep = "")
      out <- strwrap(links, initial = seealso, width = 60, exdent = 2)

      add_tag(topic, new_tag("seealso", paste(out, collapse = "\n")))
    }
  }

  topics
}

invert <- function(x) {
  if (length(x) == 0) return()
  utils::unstack(rev(utils::stack(x)))
}

get_values <- function(topics, tag) {
  tags <- lapply(topics, get_tag, tag)
  tags <- Filter(Negate(is.null), tags)
  lapply(tags, "[[", "values")
}
