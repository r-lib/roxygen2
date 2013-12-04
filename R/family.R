process_family <- function(topics) {
  family_lookup <- invert(get_values(topics, "family"))
  name_lookup <- get_values(topics, "name")
  
  for(family in names(family_lookup)) {
    related <- family_lookup[[family]]
    
    for(topic_name in related) {
      topic <- topics[[topic_name]]
      others <- setdiff(related, topic_name)
      
      if (length(others) < 1) next;
      
      other_topics <- sort(unlist(name_lookup[others], use.names = FALSE))
      
      links <- paste("\\code{\\link{", other_topics, "}}",
        collapse =", ", sep = "")
      seealso <- paste("Other ", family, ": ", sep = "")
      out <- strwrap(links, initial = seealso, width = 60, exdent = 2)
      
      add_tag(topic, new_tag("seealso", paste(out, collapse = "\n")))
    }
  }
  
  topics
}