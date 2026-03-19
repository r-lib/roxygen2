topic_add_r6_methods <- function(rd, block, env, base_path) {
  docs <- r6_class_from_block(block, env)

  # Store resolved docs so subclasses can inherit
  classname <- block$object$value$classname
  if (!is.null(classname)) {
    r6_docs <- roxy_meta_get("r6_docs", list())
    r6_docs[[classname]] <- docs
    roxy_meta_set("r6_docs", r6_docs)
  }

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
  method_tags <- c("description", "details", "return", "returns", "examples")

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

# Topological sort ---------------------------------------------------------

# Sort blocks so R6 superclasses are processed before subclasses.
r6_topo_sort_blocks <- function(blocks) {
  is_r6 <- map_lgl(blocks, \(b) inherits(b, "roxy_block_r6class"))
  if (sum(is_r6) <= 1) {
    return(blocks)
  }

  r6_indices <- which(is_r6)
  r6_blocks <- blocks[r6_indices]

  classnames <- map_chr(r6_blocks, \(b) b$object$value$classname %||% "")
  parents <- map_chr(r6_blocks, \(b) {
    inherit <- b$object$value$inherit
    if (is.null(inherit)) NA_character_ else as.character(inherit)
  })

  topo <- TopoSort$new()
  for (i in seq_along(classnames)) {
    topo$add(classnames[i])
    if (!is.na(parents[i]) && parents[i] %in% classnames) {
      topo$add_ancestor(classnames[i], parents[i])
    }
  }
  sorted <- topo$sort()

  blocks[r6_indices] <- r6_blocks[match(sorted, classnames)]
  blocks
}
