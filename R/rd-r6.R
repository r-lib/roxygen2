topic_add_r6_methods <- function(rd, block, env, base_path) {
  docs <- r6_class_from_block(block, env)

  # Add class-level tags
  for (tag in block$tags) {
    if (is_class_tag(tag, block)) {
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

is_class_tag <- function(tag, block) {
  if (tag$tag %in% c("param", "field")) {
    FALSE
  } else if (tag$tag %in% c("description", "details", "return", "examples")) {
    !is.na(tag$line) && tag$line >= block$line
  } else {
    TRUE
  }
}
