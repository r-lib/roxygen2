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
  blocks <- parse_text(text, env = env)

  # Sort blocks so superclasses are processed first
  blocks <- r6_topo_sort_blocks(blocks)

  # Process all R6 blocks before the target, storing resolved docs
  local_roxy_meta_set("r6_docs", list())
  target <- n %||% length(blocks)
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    if (!inherits(block, "roxy_block_r6class")) {
      next
    }
    docs <- r6_class_from_block(block, env)
    classname <- block$object$value$classname
    if (!is.null(classname)) {
      r6_docs <- roxy_meta_get("r6_docs", list())
      r6_docs[[classname]] <- docs
      roxy_meta_set("r6_docs", r6_docs)
    }
    if (i == target) return(docs)
  }

  r6_class_from_block(blocks[[target]], env)
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
