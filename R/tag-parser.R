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
#' @family extending
NULL

#' @export
#' @rdname tag_parsers
#' @param multiline If `FALSE` (the default), tags that span multiple lines
#'   will generate a warning. Set to `TRUE` for tags where multiline content
#'   is expected (e.g., `@usage`, `@rawRd`).
tag_value <- function(x, multiline = FALSE) {
  x$val <- trimws(x$raw)

  if (x$val == "") {
    warn_roxy_tag(x, "requires a value")
    return(NULL)
  }

  if (!multiline && warn_if_multiline(x, x$val)) {
    return(NULL)
  }

  if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    return(NULL)
  }

  x
}

# Also recorded in tags.yml
inherit_components <- c(
  "params",
  "return",
  "title",
  "description",
  "details",
  "seealso",
  "sections",
  "references",
  "examples",
  "author",
  "source",
  "note",
  "format"
)

#' @export
#' @rdname tag_parsers
tag_inherit <- function(x) {
  if (trimws(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    pieces <- strsplit(trimws(x$raw), "\\s+")[[1]]
    fields <- pieces[-1]

    all <- inherit_components
    if (length(fields) == 0) {
      fields <- all
    } else {
      unknown <- setdiff(fields, all)
      if (length(unknown) > 0) {
        warn_roxy_tag(
          x,
          "attempts to inherit from unknown type {.str {unknown}}"
        )
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
  val <- trimws(x$raw)
  if (val == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else if (grepl("\\s", val)) {
    n <- length(strsplit(val, "\\s+")[[1]])
    warn_roxy_tag(x, "must have only one argument, not {n}")
    NULL
  } else {
    x$val <- val
    x
  }
}

#' @export
#' @rdname tag_parsers
#' @param first,second Name of first and second parts of two part tags
#' @param required Is the second part required (TRUE) or can it be blank
#'   (FALSE)?
#' @param markdown Should the second part be parsed as markdown?
tag_two_part <- function(x, first, second, required = TRUE, markdown = TRUE) {
  if (trimws(x$raw) == "") {
    if (!required) {
      warn_roxy_tag(x, "requires {first}")
    } else {
      warn_roxy_tag(x, "requires two parts: {first} and {second}")
    }
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    pieces <- split_two_part(trimws(x$raw))

    if (required && pieces[[2]] == "") {
      warn_roxy_tag(x, "requires two parts: {first} and {second}")
      return(NULL)
    }

    pieces[, 2] <- trim_docstring(pieces[, 2])
    if (markdown) {
      pieces[, 2] <- markdown_if_active(pieces[, 2], x)
    }

    x$val <- list(
      name = pieces[, 1],
      description = pieces[, 2]
    )
    x
  }
}

# Split a string into two parts: a name and a description.
# Handles backtick-quoted names that may contain spaces (e.g. `arg 1`).
split_two_part <- function(x) {
  if (grepl("^`", x)) {
    match <- regexpr("^`[^`]*`", x)
    if (match == -1L || attr(match, "match.length") == -1L) {
      # No closing backtick; fall back to space splitting
      split_first_word(x)
    } else {
      end <- attr(match, "match.length")
      # Strip backticks so name matches names(formals(fn))
      name <- substr(x, 2, end - 1)
      rest <- trimws(substr(x, end + 1, nchar(x)))
      matrix(c(name, rest), nrow = 1)
    }
  } else {
    split_first_word(x)
  }
}

split_first_word <- function(x) {
  m <- regexpr("[[:space:]]+", x)
  if (m == -1L) {
    matrix(c(x, ""), nrow = 1)
  } else {
    matrix(
      c(substr(x, 1, m - 1L), substr(x, m + attr(m, "match.length"), nchar(x))),
      nrow = 1
    )
  }
}

#' @export
#' @rdname tag_parsers
tag_name_description <- function(x) {
  tag_two_part(x, "a name", "a description")
}

#' @export
#' @rdname tag_parsers
#' @param min,max Minimum and maximum number of words
tag_words <- function(x, min = 0, max = Inf, multiline = FALSE) {
  val <- trimws(x$raw)

  if (!multiline && warn_if_multiline(x, val)) {
    return(NULL)
  }

  if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    return(NULL)
  }

  words <- if (nzchar(val)) strsplit(val, "\\s+")[[1]] else ""
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
  lifecycle::deprecate_warn("7.4.0", "tag_words_line()", "tag_words()")
  tag_words(x)
}

# Returns TRUE (and warns) if val contains multiple lines, FALSE otherwise.
warn_if_multiline <- function(x, val) {
  lines <- strsplit(val, "\n", fixed = TRUE)[[1]]
  n_lines <- length(lines) - 1L
  if (n_lines >= 1) {
    first_line <- lines[[1]]
    warn_roxy_tag(
      x,
      c(
        "must be only 1 line long, not {n_lines + 1}",
        i = "The first line is {.str {first_line}}"
      )
    )
    TRUE
  } else {
    FALSE
  }
}

#' @export
#' @rdname tag_parsers
tag_toggle <- function(x) {
  x$val <- trimws(x$raw)

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
  if (trimws(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else {
    tryCatch(
      {
        x$val <- parse(text = x$raw)
        x
      },
      error = function(e) {
        warn_roxy_tag(x, "failed to parse", parent = e)
        NULL
      }
    )
  }
}

# Examples need special parsing because escaping rules are different
#' @export
#' @rdname tag_parsers
tag_examples <- function(x) {
  if (trimws(x$raw) == "") {
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
  if (trimws(x$raw) == "") {
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
  if (trimws(x$raw) == "") {
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
        out[[i]] <- trimws(out[[i]])
      }
    }
    out
  } else {
    if (!rdComplete(text, is_code = FALSE)) {
      warn_roxy_tag(tag, "has mismatched braces or quotes")
      ""
    } else {
      trimws(text)
    }
  }
}
