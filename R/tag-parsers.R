# Simple parsers ----------------------------------------------------------

tag_value <- function(x) {
  if (x$val == "") {
    tag_warning(x, "requires a value")
  } else if (!rdComplete(x$val)) {
    tag_warning(x, "mismatched braces or quotes")
  } else {
    x$val <- str_trim(x$val)
    x
  }
}

tag_name <- function(x) {
  if (x$val == "") {
    tag_warning("requires a name")
  } else if (!rdComplete(x$val)) {
    tag_warning("mismatched braces or quotes")
  } else if (str_count(x$val, "\\s+") > 1) {
    tag_warning("should have only a single argument")
  } else {
    x$val <- str_trim(x$val)
    x
  }
}

tag_name_description <- function(x) {
  x$val <- full_markdown(x$val)

  if (x$val == "") {
    tag_warning(x, "requires a value")
  } else if (!str_detect(x$val, "[[:space:]]+")) {
    tag_warning(x, "requires name and description")
  } else if (!rdComplete(x$val)) {
    tag_warning(x, "mismatched braces or quotes")
  } else {
    pieces <- str_split_fixed(str_trim(x$val), "[[:space:]]+", 2)

    x$val <- list(
      name = pieces[, 1],
      description = trim_docstring(pieces[, 2])
    )
    x
  }
}

tag_words <- function(min = 0, max = Inf) {
  function(x) {
    if (!rdComplete(x$val)) {
      return(tag_warning(x, "mismatched braces or quotes"))
    }

    words <- str_split(str_trim(x$val), "\\s+")[[1]]
    if (length(words) < min) {
      tag_warning(x,  " needs at least ", min, " words")
    } else if (length(words) > max) {
      tag_warning(x,  " can have at most ", max, " words")
    }

    x$val <- words
    x
  }
}

tag_words_line <- function(x) {
  x$val <- str_trim(x$val)

  if (str_detect(x$val, "\n")) {
    tag_warning(x, "may only span a single line")
  } else if (!rdComplete(x$val)) {
    tag_warning(x, "mismatched braces or quotes")
  } else {
    x$val <- str_split(x$val, "\\s+")[[1]]
    x
  }
}

tag_toggle <- function(x) {
  x$val <- str_trim(x$val)

  if (x$val != "") {
    tag_warning(x, "has no parameters")
  } else {
    x
  }
}

tag_code <- function(x) {
  if (x$val == "") {
    tag_warning(x, "requires a value")
  } else {
    tryCatch({
      parse(text = x$val)
      x
    }, error = function(e) {
      tag_warning(x, "code failed to parse.\n", e$message)
    })
  }
}

# Examples need special parsing because escaping rules are different
tag_examples <- function(x) {
  if (x$val == "") {
    return(tag_warning(x, "requires a value"))
  }

  x$val <- escape_examples(gsub("^\n", "", x$val))
  if (!rdComplete(x$val, TRUE)) {
    tag_warning(x, "mismatched braces or quotes")
  } else {
    x
  }
}

tag_markdown <- function(x) {
  x$val <- full_markdown(x$val)
  tag_value(x)
}

tag_markdown_title <- function(x) {
  x$val <- restricted_markdown(x$val)
  tag_value(x)
}

