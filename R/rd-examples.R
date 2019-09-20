topic_add_examples <- function(topic, block, base_path) {
  tags <- block_get_tags(block, c("examples", "example"))

  for (tag in tags) {
    if (tag$tag == "examples") {
      example <- tag$val
    } else {
      example <- read_example_from_path(tag, base_path)
    }
    topic$add_simple_field("examples", example)
  }
}

read_example_from_path <- function(tag, base_path) {
  path <- str_trim(tag$val)
  nl <- str_count(path, "\n")
  if (any(nl) > 0) {
    roxy_tag_warning(tag, "spans multiple lines. Do you want @examples?")
    return()
  }

  path <- file.path(base_path, path)
  if (!file.exists(path)) {
    roxy_tag_warning(tag, "'", path, "' doesn't exist")
    return()
  }

  code <- read_lines(path)
  escape_examples(code)
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
