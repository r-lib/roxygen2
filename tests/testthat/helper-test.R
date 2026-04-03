expect_equivalent_rd <- function(out1, out2) {
  out1$sections$backref <- NULL
  out2$sections$backref <- NULL
  expect_equal(out1, out2)
}

expect_equal_strings <- function(s1, s2, ignore_ws = TRUE) {
  if (ignore_ws) {
    s1 <- gsub("\\s", "", s1, perl = TRUE)
    s2 <- gsub("\\s", "", s2, perl = TRUE)
  }
  expect_equal(s1, s2)
}

expect_parse_failure <- function(code) {
  (expect_condition(expect_null(code)))
}

r6_doc <- function(text, env = new.env(parent = globalenv()), n = NULL) {
  eval(parse(text = text, keep.source = TRUE), envir = env)
  blocks <- merge_external_r6methods(parse_text(text, env = env))

  # Pass 1: extract local docs (no inheritance)
  r6_blocks <- Filter(
    function(b) inherits(b, "roxy_block_r6class"),
    blocks
  )
  all_docs <- lapply(r6_blocks, r6_class_from_block, env = env)
  names(all_docs) <- map_chr(
    r6_blocks,
    \(b) b$object$value$classname %||% b$object$alias %||% ""
  )

  # Pass 2: resolve inheritance in dependency order
  target <- n %||% length(r6_blocks)
  parent_docs <- list()
  for (i in seq_along(all_docs)) {
    docs <- all_docs[[i]]
    topic_name <- names(all_docs)[[i]]
    docs$fields <- r6_resolve_fields(docs$fields, parent_docs, topic_name)
    docs$active_bindings <- r6_resolve_fields(
      docs$active_bindings,
      parent_docs,
      topic_name
    )
    docs$methods$self <- lapply(docs$methods$self, function(method) {
      r6_resolve_method_params(method, parent_docs, topic_name)
    })
    all_docs[[i]] <- docs
    parent_docs[[topic_name]] <- docs
  }

  all_docs[[target]]
}

local_package_copy <- function(path, env = caller_env(), set_version = TRUE) {
  temp_path <- withr::local_tempdir(.local_envir = env)

  file.copy(path, temp_path, recursive = TRUE)
  pkg_path <- dir(temp_path, full.names = TRUE)[[1]]

  if (set_version) {
    desc::desc_set(
      file = pkg_path,
      RoxygenNote = as.character(packageVersion("roxygen2"))
    )
  }

  normalizePath(pkg_path, winslash = "/")
}
