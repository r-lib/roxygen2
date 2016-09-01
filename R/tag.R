#' Parsing tags.
#'
#' `roxy_tag` constructs a tag object, and `roxy_tag_warning` makes
#' an informative warning using the location information stored in the tag.
#' The remainder of the tag functions parse the tag value, convert a string
#' into a richer R object, or providing informative warnings and returning
#' valid if the value is invalid.
#'
#' One exception to the rule is `tag_words`: you call it with
#' arguments giving the `min` and `max` words, and it will
#' generate a tag parser for you.
#'
#' @keywords internal
#' @md
#' @export
#' @param tag Tag name
#' @param val Tag value. When read from the file, this will be a string,
#'   but after parsing can be a more complicated structure (typically
#'   a character vector, but sometimes a list).
#' @param file,line Location of the tag
roxy_tag <- function(tag, val, file = "", line = 0) {
  structure(
    list(
      file = file,
      line = line,
      tag = tag,
      val = val
    ),
    class = "roxy_tag"
  )
}

is.roxy_tag <- function(x) inherits(x, "roxy_tag")

#' @export
print.roxy_tag <- function(x, ...) {
  cat("[", x$file, ":", x$line, "] @", x$tag, " ", encodeString(x$val), "\n",
    sep = "")
}

make_tag_message <- function(x, message) {
  paste0(
    "@",
    x$tag,
    if (x$file != "") paste0(" [", x$file, "#", x$line, "]"),
    ": ",
    message
  )
}

#' @export
#' @rdname roxy_tag
roxy_tag_warning <- function(x, ...) {
  warning(make_tag_message(x, paste0(...)), call. = FALSE, immediate. = TRUE)
  NULL
}


#' @export
#' @rdname roxy_tag
tag_value <- function(x) {
  if (x$val == "") {
    roxy_tag_warning(x, "requires a value")
  } else if (!rdComplete(x$val)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    x$val <- str_trim(x$val)
    x
  }
}

#' @export
#' @rdname roxy_tag
tag_name <- function(x) {
  if (x$val == "") {
    roxy_tag_warning("requires a name")
  } else if (!rdComplete(x$val)) {
    roxy_tag_warning("mismatched braces or quotes")
  } else if (str_count(x$val, "\\s+") > 1) {
    roxy_tag_warning("should have only a single argument")
  } else {
    x$val <- str_trim(x$val)
    x
  }
}

#' @export
#' @rdname roxy_tag
tag_name_description <- function(x) {
  x$val <- full_markdown(x$val)

  if (x$val == "") {
    roxy_tag_warning(x, "requires a value")
  } else if (!str_detect(x$val, "[[:space:]]+")) {
    roxy_tag_warning(x, "requires name and description")
  } else if (!rdComplete(x$val)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    pieces <- str_split_fixed(str_trim(x$val), "[[:space:]]+", 2)

    x$val <- list(
      name = pieces[, 1],
      description = trim_docstring(pieces[, 2])
    )
    x
  }
}

#' @export
#' @rdname roxy_tag
tag_words <- function(min = 0, max = Inf) {
  function(x) {
    if (!rdComplete(x$val)) {
      return(roxy_tag_warning(x, "mismatched braces or quotes"))
    }

    words <- str_split(str_trim(x$val), "\\s+")[[1]]
    if (length(words) < min) {
      roxy_tag_warning(x,  " needs at least ", min, " words")
    } else if (length(words) > max) {
      roxy_tag_warning(x,  " can have at most ", max, " words")
    }

    x$val <- words
    x
  }
}

#' @export
#' @rdname roxy_tag
tag_words_line <- function(x) {
  x$val <- str_trim(x$val)

  if (str_detect(x$val, "\n")) {
    roxy_tag_warning(x, "may only span a single line")
  } else if (!rdComplete(x$val)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    x$val <- str_split(x$val, "\\s+")[[1]]
    x
  }
}

#' @export
#' @rdname roxy_tag
tag_toggle <- function(x) {
  x$val <- str_trim(x$val)

  if (x$val != "") {
    roxy_tag_warning(x, "has no parameters")
  } else {
    x
  }
}

#' @export
#' @rdname roxy_tag
tag_code <- function(x) {
  if (x$val == "") {
    roxy_tag_warning(x, "requires a value")
  } else {
    tryCatch({
      parse(text = x$val)
      x
    }, error = function(e) {
      roxy_tag_warning(x, "code failed to parse.\n", e$message)
    })
  }
}

# Examples need special parsing because escaping rules are different
#' @export
#' @rdname roxy_tag
tag_examples <- function(x) {
  if (x$val == "") {
    return(roxy_tag_warning(x, "requires a value"))
  }

  x$val <- escape_examples(gsub("^\n", "", x$val))
  if (!rdComplete(x$val, TRUE)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    x
  }
}

#' @export
#' @rdname roxy_tag
tag_markdown <- function(x) {
  x$val <- full_markdown(x$val)
  tag_value(x)
}

#' @export
#' @rdname roxy_tag
tag_markdown_restricted <- function(x) {
  x$val <- restricted_markdown(x$val)
  tag_value(x)
}

