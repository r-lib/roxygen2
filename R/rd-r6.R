topic_add_r6_methods <- function(rd, block, env, base_path) {
  docs <- r6_class_from_block(block, env)

  # Add class-level tags
  for (tag in block$tags) {
    if (r6_tag_type(tag, block) == "class") {
      rd$add(roxy_tag_rd(tag, env = env, base_path = base_path))
    }
  }

  # Add docs for each method/field/binding
  rd_lines <- format(docs)
  rd$add(rd_section("rawRd", paste(rd_lines, collapse = "\n")))

  # Add combined examples for all methods
  ex_lines <- r6_all_examples(docs$methods)
  if (length(ex_lines) > 0) {
    ex_txt <- paste0(ex_lines, collapse = "\n")
    rd$add(rd_section("examples", ex_txt), overwrite = FALSE)
  }
}

# Classify an R6 block tag:
# - "class": top-level Rd (e.g. @title, @description before class body)
# - "method": inline tag associated with a method
# - "other": @field/@param tags consumed by field/param extraction
r6_tag_type <- function(tag, block) {
  inline <- !is.na(tag$line) && tag$line >= block$line
  method_tags <- c(
    "description",
    "details",
    "param",
    "return",
    "returns",
    "examples"
  )

  if (tag$tag == "field") {
    "other"
  } else if (tag$tag %in% method_tags && inline) {
    "method"
  } else if (tag$tag == "param") {
    "other"
  } else {
    "class"
  }
}
