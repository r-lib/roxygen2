process_family <- function(topics) {
  family_lookup <- invert(get_values(topics, "family"))
  alias_lookup <- get_values(topics, "alias")

  for (topic_name in names(topics)) {
    topic <- topics[[topic_name]]
    families <- topic$get_tag("family")$values

    for (family in families) {
      related <- family_lookup[[family]]

      others <- setdiff(related, topic_name)
      if (length(others) < 1)
        next

      by_file <- vapply(alias_lookup[others], function(x) {
        paste0("\\code{\\link{", escape(x[1]), "}}")
      }, FUN.VALUE = character(1))
      links <- paste(sort_c(by_file), collapse = ", ")

      seealso <- paste("Other ", family, ": ", sep = "")
      out <- strwrap(links, initial = seealso, width = 60, exdent = 2)

      topic$add_tags(new_tag("seealso", paste(out, collapse = "\n")))
    }
  }

  topics
}

invert <- function(x) {
  if (length(x) == 0) return()
  stacked <- utils::stack(x)
  tapply(as.character(stacked$ind), stacked$values, list)
}

get_values <- function(topics, tag) {
  tags <- lapply(topics, function(rd) rd$get_tag(tag))
  tags <- Filter(Negate(is.null), tags)
  lapply(tags, "[[", "values")
}
