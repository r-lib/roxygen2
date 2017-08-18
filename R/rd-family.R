topics_process_family <- function(topics) {
  family_index <- invert(topics$simple_values("family"))
  aliases <- topics$simple_values("alias")

  for (topic_name in names(topics$topics)) {
    topic <- topics$get(topic_name)
    families <- topic$get_field("family")$values

    for (family in families) {
      related <- family_index[[family]]
      topic$add_simple_field("concept", family)

      others <- setdiff(related, topic_name)
      if (length(others) < 1)
        next

      by_file <- vapply(aliases[others], function(x) {
        paste0("\\code{\\link{", escape(x[1]), "}}")
      }, FUN.VALUE = character(1))
      links <- paste(sort_c(by_file), collapse = ", ")

      seealso <- paste("Other ", family, ": ", sep = "")
      out <- strwrap(links, initial = seealso, width = 60, exdent = 2)

      topic$add_simple_field("seealso", paste(out, collapse = "\n"))
    }
  }

  invisible()
}
