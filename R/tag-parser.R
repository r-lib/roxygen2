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
#' @param multiline Controls how the tag may span multiple lines:
#'   * `"never"` (the default): the tag must be a single line, and spanning
#'     multiple lines generates a warning.
#'   * `"indent"`: the tag may span multiple lines, but continuation lines must
#'     use a hanging indent (i.e. be indented more than the first line). The
#'     first line that is not indented (including a blank line) ends the tag,
#'     and anything after it is ignored, with a warning. Use this for tags where
#'     multiline input is convenient but a flush line almost always signals a
#'     missing tag (e.g., `@importFrom`).
#'   * `"always"`: the tag may span any number of lines and paragraphs. Use this
#'     for tags where multiline content is expected (e.g., `@usage`, `@rawRd`).
#'
#'   For backward compatibility, `FALSE` and `TRUE` are accepted as synonyms for
#'   `"never"` and `"always"` respectively.
tag_value <- function(x, multiline = "never") {
  x$val <- trimws(x$raw)

  if (x$val == "") {
    warn_roxy_tag(x, "requires a value")
    return(NULL)
  }

  x$val <- check_multiline(x, x$val, multiline)

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
  if (trimws(x$raw) == "") {
    warn_roxy_tag(x, "requires a value")
    NULL
  } else if (!rdComplete(x$raw, is_code = FALSE)) {
    warn_roxy_tag(x, "has mismatched braces or quotes")
    NULL
  } else {
    n <- re_count(x$raw, "\\s+")
    if (n > 1) {
      warn_roxy_tag(x, "must have only one argument, not {n + 1}")
      NULL
    } else {
      x$val <- trimws(x$raw)
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
tag_two_part <- function(
  x,
  first,
  second,
  required = TRUE,
  markdown = TRUE,
  multiline = "never"
) {
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
    val <- check_multiline(x, trimws(x$raw), multiline)
    pieces <- split_two_part(val)

    if (required && pieces[[2]] == "") {
      warn_roxy_tag(x, "requires two parts: {first} and {second}")
      return(NULL)
    }

    pieces[[2]] <- trim_docstring(pieces[[2]])
    if (markdown) {
      pieces[[2]] <- markdown_if_active(pieces[[2]], x)
    }

    x$val <- list(
      name = pieces[[1]],
      description = pieces[[2]]
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
      re_split_half(x, "[[:space:]]+")
    } else {
      end <- attr(match, "match.length")
      # Strip backticks so name matches names(formals(fn))
      name <- substr(x, 2, end - 1)
      rest <- trimws(substr(x, end + 1, nchar(x)))
      c(name, rest)
    }
  } else {
    re_split_half(x, "[[:space:]]+")
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
tag_words <- function(x, min = 0, max = Inf, multiline = "never") {
  val <- trimws(x$raw)

  val <- check_multiline(x, val, multiline)

  if (!rdComplete(val, is_code = FALSE)) {
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
  lifecycle::deprecate_warn("8.0.0", "tag_words_line()", "tag_words()")
  tag_words(x)
}

# Normalises the `multiline` argument to one of "never", "indent", or "always",
# silently translating the legacy `FALSE`/`TRUE` values for backward
# compatibility.
as_multiline <- function(multiline, error_call = caller_env()) {
  if (isTRUE(multiline)) {
    return("always")
  }
  if (isFALSE(multiline)) {
    return("never")
  }

  arg_match0(multiline, c("never", "indent", "always"), error_call = error_call)
}

# Applies the multiline policy for a tag's value, warning when it is violated
# and returning the value to use (possibly truncated to its hanging-indented
# continuation). See the `multiline` parameter of `tag_value()` for the meaning
# of each mode.
check_multiline <- function(x, val, multiline) {
  multiline <- as_multiline(multiline)

  if (multiline == "always") {
    return(val)
  }

  if (multiline == "indent") {
    return(check_indent(x, val))
  }

  n_lines <- re_count(val, "\n")
  if (n_lines >= 1) {
    first_line <- re_split_half(val, "\n")[[1]]
    warn_roxy_tag(
      x,
      c(
        "must be only 1 line long, not {n_lines + 1}",
        i = "The first line is {.str {first_line}}"
      )
    )
  }

  val
}

# Keeps the first line of `val` plus any immediately following lines that use a
# hanging indent (indented more than the first line). The first flush or blank
# line ends the tag; anything after it is dropped with a warning, since a flush
# line usually signals a forgotten tag (e.g. a missing `@examples`).
check_indent <- function(x, val) {
  lines <- strsplit(val, "\n", fixed = TRUE)[[1]]
  if (length(lines) <= 1) {
    return(val)
  }

  indent <- leadingSpaces(lines)
  continues <- nzchar(trimws(lines[-1])) & indent[-1] > indent[[1]]

  ends_at <- if (all(continues)) length(lines) else which(!continues)[[1]]
  if (ends_at < length(lines)) {
    warn_roxy_tag(
      x,
      c(
        "must use a hanging indent to span multiple lines",
        i = "Continuation lines must be indented; did you forget a tag like {.code @examples}?"
      )
    )
  }

  paste(lines[seq_len(ends_at)], collapse = "\n")
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
