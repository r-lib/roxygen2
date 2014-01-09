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
        paste0("\\code{\\link{", sort_c(escape(x)), "}}", collapse = ", ")
      }, FUN.VALUE = character(1))
      links <- paste(sort_c(by_file), collapse ="; ")
      
      seealso <- paste("Other ", family, ": ", sep = "")
      out <- strwrap(links, initial = seealso, width = 60, exdent = 2)
      
      add_tag(topic, new_tag("seealso", paste(out, collapse = "\n")))
    }
  }
  
  topics
}
