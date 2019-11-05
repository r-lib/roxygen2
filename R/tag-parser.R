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
  if (x$raw == "") {
    roxy_tag_warning(x, "requires a value")
  } else if (!rdComplete(x$raw)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    x$val <- str_trim(x$raw)
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_inherit <- function(x) {
  if (x$raw == "") {
    roxy_tag_warning(x, "requires a value")
  } else if (!rdComplete(x$raw)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    pieces <- str_split(str_trim(x$raw), "\\s+")[[1]]
    fields <- pieces[-1]

    # Also recorded in `rd.Rmd`
    all <- c("params", "return", "title", "description", "details", "seealso",
      "sections", "references", "examples", "author", "source")
    if (length(fields) == 0) {
      fields <- all
    } else {
      unknown <- setdiff(fields, all)
      if (length(unknown) > 0) {
        types <- paste0(unknown, collapse = ", ")
        roxy_tag_warning(x, "Unknown inherit type: ", types)
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
  if (x$raw == "") {
    roxy_tag_warning("requires a name")
  } else if (!rdComplete(x$raw)) {
    roxy_tag_warning("mismatched braces or quotes")
  } else if (str_count(x$raw, "\\s+") > 1) {
    roxy_tag_warning("should have only a single argument")
  } else {
    x$val <- str_trim(x$raw)
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
  if (str_trim(x$raw) == "") {
    roxy_tag_warning(x, "requires a value")
  } else if (required && !str_detect(x$raw, "[[:space:]]+")) {
    roxy_tag_warning(x, "requires ", first, " and ", second)
  } else if (!rdComplete(x$raw)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    pieces <- str_split_fixed(str_trim(x$raw), "[[:space:]]+", 2)

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
  if (!rdComplete(x$raw)) {
    return(roxy_tag_warning(x, "mismatched braces or quotes"))
  }

  words <- str_split(str_trim(x$raw), "\\s+")[[1]]
  if (length(words) < min) {
    roxy_tag_warning(x,  " needs at least ", min, " words")
  } else if (length(words) > max) {
    roxy_tag_warning(x,  " can have at most ", max, " words")
  }

  x$val <- words
  x
}

#' @export
#' @rdname tag_parsers
tag_words_line <- function(x) {
  x$val <- str_trim(x$raw)

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
#' @rdname tag_parsers
tag_toggle <- function(x) {
  x$val <- str_trim(x$raw)

  if (x$val != "") {
    roxy_tag_warning(x, "has no parameters")
  } else {
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_code <- function(x) {
  if (x$raw == "") {
    roxy_tag_warning(x, "requires a value")
  } else {
    tryCatch({
      parse(text = x$raw)
    }, error = function(e) {
      roxy_tag_warning(x, "code failed to parse.\n", e$message)
    })

    x$val <- x$raw
    x
  }
}

# Examples need special parsing because escaping rules are different
#' @export
#' @rdname tag_parsers
tag_examples <- function(x) {
  if (x$raw == "") {
    return(roxy_tag_warning(x, "requires a value"))
  }

  x$val <- escape_examples(gsub("^\n", "", x$raw))
  if (!rdComplete(x$val, TRUE)) {
    roxy_tag_warning(x, "mismatched braces or quotes")
  } else {
    x
  }
}

#' @export
#' @rdname tag_parsers
tag_markdown <- function(x) {
  x$val <- markdown_if_active(x$raw, x)
  x
}

#' @export
#' @rdname tag_parsers
tag_markdown_with_sections <- function(x) {
  if (x$raw == "") {
    return(roxy_tag_warning(x, "requires a value"))
  }

  x$val <- markdown_if_active(x$raw, x, sections = TRUE)
  for (i in seq_along(x$val)) {
    if (!rdComplete(x$val[i])) {
      roxy_tag_warning(x, "mismatched braces or quotes")
      x$val[i] <- ""
    } else {
      x$val[i] <- str_trim(x$val[i])
    }
  }

  x
}

markdown_if_active <- function(text, tag, sections = FALSE) {
  if (markdown_on()) {
    markdown(text, tag, sections)
  } else {
    if (!rdComplete(text)) {
      roxy_tag_warning(tag, "mismatched braces or quotes")
      ""
    } else {
      str_trim(text)
    }
  }
}
