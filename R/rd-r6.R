# Pass 1: within-class resolution ---------------------------------------------
# Resolves params from method @param -> class-level @param -> @field.
# See r6_resolve_params() for the core logic.

topic_add_r6_methods <- function(rd, block, env, base_path) {
  docs <- r6_class_from_block(block, env)
  block <- r6_fix_intro(block)

  # Add class-level tags (skip tags stamped for a specific method)
  for (tag in block$tags) {
    if (r6_tag_type(tag, block) == "class") {
      rd$add(roxy_tag_rd(tag, env = env, base_path = base_path))
    }
  }

  # Store unresolved R6 docs; pass 2 will inherit and format
  rd$add(rd_section("r6_class", docs))
}

#' @export
format.rd_section_r6_class <- function(x, ...) NULL

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
  if (!is.null(tag$r6method)) {
    return("method")
  }

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
  trimws(strsplit(tag$val$name, ",")[[1]])
}

tag_has_name <- function(tag, names) {
  any(tag_names(tag) %in% names)
}

# Pass 2: R6 inheritance resolution -----------------------------------------

# Resolve R6 superclass inheritance after all blocks have been converted
# to topics. Mirrors topics_process_inherit() but for R6 field/param docs.
topics_process_r6_inherit <- function(topics) {
  r6_deps <- function(topic) {
    docs <- topic$get_value("r6_class")
    docs$superclasses$classname
  }

  topics$topo_apply(r6_deps, r6_resolve_topic)
}

r6_resolve_topic <- function(topic, topics) {
  docs <- topic$get_value("r6_class")
  if (is.null(docs)) {
    return()
  }

  topic_name <- topic$get_name()

  # Collect resolved parent docs from already-processed topics
  parent_docs <- list()
  for (classname in docs$superclasses$classname) {
    parent_file <- topics$find_filename(classname)
    if (is.na(parent_file)) {
      next
    }
    parent_topic <- topics$get(parent_file)
    if (is.null(parent_topic)) {
      next
    }
    parent_docs[[classname]] <- parent_topic$get_value("r6_class")
  }

  # Inherit fields and active bindings
  docs$fields <- r6_resolve_fields(docs$fields, parent_docs, topic_name)
  docs$active_bindings <- r6_resolve_fields(
    docs$active_bindings,
    parent_docs,
    topic_name
  )

  # Inherit method params
  docs$methods$self <- lapply(docs$methods$self, function(method) {
    r6_resolve_method_params(method, parent_docs, topic_name)
  })

  # Update stored docs (now resolved, so child classes can read them)
  topic$sections$r6_class <- rd_section("r6_class", docs)

  # Format and inject into topic
  rd_lines <- format(docs)
  topic$add(rd_section("rawRd", paste(rd_lines, collapse = "\n")))

  # Add combined examples for all methods
  ex_lines <- r6_all_examples(docs$methods)
  if (length(ex_lines) > 0) {
    ex_txt <- paste0(ex_lines, collapse = "\n")
    topic$add(rd_section("examples", ex_txt), overwrite = FALSE)
  }
}

# Field inheritance ----------------------------------------------------------

r6_resolve_fields <- function(fields_obj, parent_docs, topic_name) {
  label <- if (fields_obj$type == "field") "field" else "active binding"
  section <- if (fields_obj$type == "field") "fields" else "active_bindings"

  docd <- r6_field_names(fields_obj$fields)
  expected <- fields_obj$expected

  miss <- setdiff(expected, docd)
  inherited <- r6_find_super_fields(miss, parent_docs, section)
  fields_obj$fields <- c(fields_obj$fields, inherited)
  docd <- c(docd, r6_field_names(inherited))

  miss <- setdiff(expected, docd)
  if (length(miss) > 0) {
    warn_roxy_topic(topic_name, "Undocumented R6 {label}{?s}: {miss}")
  }

  fields_obj
}

r6_find_super_fields <- function(missing, parent_docs, section) {
  if (length(missing) == 0 || length(parent_docs) == 0) {
    return(list())
  }

  result <- list()
  for (super_doc in parent_docs) {
    if (is.null(super_doc)) {
      next
    }

    for (field in super_doc[[section]]$fields) {
      if (field$name %in% missing) {
        result <- c(result, list(field))
        missing <- setdiff(missing, field$name)
      }
    }
    if (length(missing) == 0) break
  }

  result
}

# Param inheritance ----------------------------------------------------------

# Inherit params from parent classes (child -> parent -> grandparent).
# Within-class param resolution (method -> class-level -> @field) is handled
# earlier by r6_resolve_params().
r6_resolve_method_params <- function(method, parent_docs, topic_name) {
  fnames <- names(method$formals)
  if (length(fnames) == 0) {
    return(method)
  }

  miss <- setdiff(fnames, r6_param_names(method$params))
  if (length(miss) > 0) {
    inherited <- r6_find_super_params(method$name, miss, parent_docs)
    method$params <- c(method$params, inherited)

    # Re-order according to formals
    firstnames <- map_chr(
      strsplit(map_chr(method$params, \(x) x$name), ","),
      \(x) trimws(x[[1]])
    )
    method$params <- method$params[order(match(firstnames, fnames))]
  }

  # Only now, after two rounds of resolution can we warn about missing params.
  miss <- setdiff(fnames, r6_param_names(method$params))
  for (m in miss) {
    warn_roxy_topic(
      topic_name,
      c(
        "Must use one @param for each argument",
        x = "{method$name}({m}) is not documented"
      )
    )
  }

  method
}

r6_find_super_params <- function(method_name, missing, parent_docs) {
  if (length(parent_docs) == 0) {
    return(list())
  }

  for (super_doc in parent_docs) {
    if (is.null(super_doc)) {
      next
    }

    super_method <- Find(
      \(m) m$name == method_name,
      super_doc$methods$self
    )
    if (is.null(super_method)) {
      next
    }

    result <- list()
    for (param in super_method$params) {
      param_names <- trimws(unlist(strsplit(param$name, ",")))
      if (any(param_names %in% missing)) {
        result <- c(result, list(param))
        missing <- setdiff(missing, param_names)
      }
    }

    if (length(result) > 0) return(result)
  }

  list()
}
