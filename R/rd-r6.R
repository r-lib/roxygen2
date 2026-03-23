topic_add_r6_methods <- function(rd, block, env, base_path) {
  docs <- r6_class_from_block(block, env)
  block <- r6_fix_intro(block)

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

# When an R6 class has inline @description tags for methods, parse_description()
# parser puts the class description into @details instead of @description.
# This function detects that case and promotes the class-level @details back
# to @description.
r6_fix_intro <- function(block) {
  types <- map_chr(block$tags, \(t) r6_tag_type(t, block))
  tags <- map_chr(block$tags, \(t) t$tag)

  has_class_desc <- any(tags == "description" & types == "class")
  has_class_details <- any(tags == "details" & types == "class")
  has_method_desc <- any(tags == "description" & types == "method")

  if (!has_class_desc && has_class_details && has_method_desc) {
    # Promote the first class-level @details to @description
    for (i in seq_along(block$tags)) {
      if (tags[[i]] == "details" && types[[i]] == "class") {
        block$tags[[i]]$tag <- "description"
        break
      }
    }
  }

  block
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
    "return",
    "returns",
    "examples",
    "noRd"
  )

  if (tag$tag %in% c("field", "name", "title")) {
    "other"
  } else if (tag$tag %in% method_tags && inline) {
    "method"
  } else if (tag$tag == "param") {
    # class-level @param are used as defaults for all methods
    if (inline) "method" else "other"
  } else {
    "class"
  }
}

tag_is <- function(tag, name) {
  tag$tag == name
}

tag_names <- function(tag) {
  str_trim(strsplit(tag$val$name, ",")[[1]])
}

tag_has_name <- function(tag, names) {
  any(tag_names(tag) %in% names)
}
