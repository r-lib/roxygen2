
# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
topic_add_examples <- function(topic, block, base_path) {
  examples <- block_tags(block, "examples")
  for (example in examples) {
    topic$add_simple_field("examples", example)
  }

  paths <- str_trim(unlist(block_tags(block, "example")))
  paths <- file.path(base_path, paths)

  for (path in paths) {
    # Check that haven't accidentally used example instead of examples
    nl <- str_count(path, "\n")
    if (any(nl) > 0) {
      block_warning(block, "@example spans multiple lines. Do you want @examples?")
      next
    }

    if (!file.exists(path)) {
      block_warning(block, "@example ", path, " doesn't exist")
      next
    }

    code <- read_lines(path)
    examples <- escape_examples(code)

    topic$add_simple_field("examples", examples)
  }
}

# Works like escape, but unescapes special rd example commands.
# Also unescapes quotes because they must already be in strings and hence
# don't need an additional layer of quoting.
escape_examples <- function(x) {
  x <- escape(x)
  x <- gsub("\\\\dont", "\\dont", x, fixed = TRUE)
  x <- gsub("\\\\'", "\\'", x, fixed = TRUE)
  x <- gsub('\\\\"', '\\"', x, fixed = TRUE)
  x
}
