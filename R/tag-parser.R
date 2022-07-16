#' Parse tags
#'
#' These functions parse the `raw` tag value, convert a string into a richer R
#' object and storing it in `val`, or provide an informative warning and
#' returning `NULL`.
#'
#' @section New tag:
#' To create a new `@mytag` define `roxy_tag_parse.roxy_tag_mytag()`. It should
#' either call one of the functions here, or directly set `x$val`.
#'
#' @param x A [roxy_tag] object to parse
#' @returns A [roxy_tag] object with the `val` field set to the parsed value.
#' @name tag_parsers
#' @keywords internal
NULL

#' @export
#' @rdname tag_parsers
tag_value <- function(x) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    x$val <- str_trim(x$raw)
    x
  }
}

# Also recorded in tags.yml
inherit_components <- c(
  "params", "return", "title", "description", "details", "seealso",
  "sections", "references", "examples", "author", "source", "note"
)

#' @export
#' @rdname tag_parsers
tag_inherit <- function(x) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    pieces <- str_split(str_trim(x$raw), "\\s+")[[1]]
    fields <- pieces[-1]

    all <- inherit_components
    if (length(fields) == 0) {
      fields <- all
    } else {
      unknown <- setdiff(fields, all)
      if (length(unknown) > 0) {
        warn_roxy_tag(x, "attempts to inherit from unknown type {.str {unknown}}")
        fields <- intersect(fields, all)
      }
    }

    x$val <- list(
      source = pieces[1],
      fields = fields
    )

    x
  }
}

#' @export
#' @rdname tag_parsers
tag_name <- function(x) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    n <- str_count(x$raw, "\\s+")
    if (n > 1) {
      warn_roxy_tag(x, "must have only one argument, not {n}")
      NULL
    } else {
      x$val <- str_trim(x$raw)
      x
    }
  }
}

#' @export
#' @rdname tag_parsers
#' @param first,second Name of first and second parts of two part tags
#' @param required Is the second part required (TRUE) or can it be blank
#'   (FALSE)?
#' @param markdown Should the second part be parsed as markdown?
tag_two_part <- function(x, first, second, required = TRUE, markdown = TRUE) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else if (required && !str_detect(x$raw, "[[:space:]]+")) {
    warn_roxy_tag(x, "requires {first} and {second}")
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    pieces <- str_split_fixed(str_trim(x$raw), "[[:space:]]+", 2)
    pieces[is.na(pieces)] <- ""

    if (markdown) {
      pieces[,2] <- markdown_if_active(pieces[,2], x)
    }

    x$val <- list(
      pieces[, 1],
      trim_docstring(pieces[,2])
    )
    names(x$val) <- c(first, second)
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_name_description <- function(x) {
  tag_two_part(x, "name", "description")
}

#' @export
#' @rdname tag_parsers
#' @param min,max Minimum and maximum number of words
tag_words <- function(x, min = 0, max = Inf) {
  if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    return(NULL)
  }

  words <- str_split(str_trim(x$raw), "\\s+")[[1]]
  if (length(words) < min) {
    warn_roxy_tag(x, "must have at least {min} word{?s}, not {length(words)}")
    NULL
  } else if (length(words) > max) {
    warn_roxy_tag(x, "must have at most {max} word{?s}, not {length(words)}")
    NULL
  } else {
    x$val <- words
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_words_line <- function(x) {
  x$val <- str_trim(x$raw)

  n_lines <- str_count(x$val, "\n")
  if (n_lines >= 1) {
    first_line <- str_split(x$val, "\n")[[1]][[1]]
    warn_roxy_tag(x, c(
      "must be a single line, not {n_lines + 1}",
      i = "The first line is {.str {first_line}}"
    ))
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    x$val <- str_split(x$val, "\\s+")[[1]]
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_toggle <- function(x) {
  x$val <- str_trim(x$raw)

  if (x$val != "") {
    warn_roxy_tag(x, "must not be followed by any text")
    NULL
  } else {
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_code <- function(x) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else {
    tryCatch({
      x$val <- parse(text = x$raw)
      x
    }, error = function(e) {
      warn_roxy_tag(x, "failed to parse", parent = e)
      NULL
    })
  }
}

# Examples need special parsing because escaping rules are different
#' @export
#' @rdname tag_parsers
tag_examples <- function(x) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    return(NULL)
  }

  x$val <- escape_examples(gsub("^\n", "", x$raw))
  if (!rdComplete(x$val, is_code = TRUE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_markdown <- function(x) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else {
    x$val <- markdown_if_active(x$raw, x)
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_markdown_with_sections <- function(x) {
  if (str_trim(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    return(NULL)
  }

  x$val <- markdown_if_active(x$raw, x, sections = TRUE)
  x
}

markdown_if_active <- function(text, tag, sections = FALSE) {
  if (markdown_on()) {
    out <- markdown(text, tag, sections)

    for (i in seq_along(out)) {
      if (sections && !rdComplete(out[[i]], is_code = FALSE)) {
        warn_roxy_tag(tag, "has mismatched braces or quotes")
        out[[i]] <- ""
      } else {
        out[[i]] <- str_trim(out[[i]])
      }
    }
    out
  } else {
    if (!rdComplete(text, is_code = FALSE)) {
      warn_roxy_tag(tag, "has mismatched braces or quotes")
      ""
    } else {
      str_trim(text)
    }
  }
}
