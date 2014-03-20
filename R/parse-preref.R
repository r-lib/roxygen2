# Parse a preref
parse.preref <- function(lines) {
  delimited.lines <- lines[str_detect(lines, LINE.DELIMITER)]
  trimmed.lines <- str_trim(str_replace(delimited.lines, LINE.DELIMITER, ""),
    "right")

  if (length(trimmed.lines) == 0) return(NULL)

  joined.lines <- paste0(trimmed.lines, collapse = '\n')
  ## Thanks to Fegis at #regex on Freenode for the
  ## lookahead/lookbehind hack; as he notes, however, "it's not
  ## proper escaping though... it will not split a@@@b."
  elements <- strsplit(joined.lines, '(?<!@)@(?!@)', perl = TRUE)[[1]]

  ## Compress the escaped delimeters.
  elements <- str_replace_all(elements, fixed("@@"), "@")

  parsed <- parse_elements(elements[-1])

  if (elements[[1]] != "") {
    check_rd(NULL, elements[[1]])
    parsed$introduction <- str_trim(elements[[1]])
  }
  parsed
}

# Sequence that distinguishes roxygen comment from normal comment.
LINE.DELIMITER <- '\\s*#+\' ?'

parse_elements <- function(elements) {
  pieces <- str_split_fixed(elements, "[[:space:]]+", 2)

  parse_element <- function(tag, value) {
    tag_parser <- preref.parsers[[tag]] %||% parse.unknown
    tag_parser(tag, value)
  }

  Map(parse_element, pieces[, 1], pieces[, 2])
}

parse.unknown <- function(key, rest) {
  stop("@", key, " is an unknown key")
}

parse.value <- function(key, rest) {
  check_rd(key, rest)
  if (is.null.string(rest)) {
    stop("@", key, ' requires a value')
  }

  str_trim(rest)
}

# Examples need special parsing because escaping rules are different
parse.examples <- function(key, rest) {
  rest <- str_trim(rest)
  if (rest == "") {
    stop("@example requires a value", call. = FALSE)
  }

  rest <- escape_examples(rest)
  check_rd("example", rest)

  rest
}

words_parser <- function(min = 0, max = Inf) {
  function(key, rest) {
    check_rd(key, rest)

    words <- str_split(str_trim(rest), "\\s+")[[1]]
    if (length(words) < min) {
      stop("@", key, " needs at least ", min, " words", call. = FALSE)
    }
    if (length(words) > max) {
      stop("@", key, " can have at most ", max, " words", call. = FALSE)
    }

    words
  }
}

parse.words.line <- function(key, rest) {
  check_rd(key, rest)
  rest <- str_trim(rest)
  if (str_detect(rest, "\n")) {
    stop("@", key, " may only span a single line", call. = FALSE)
  }

  str_split(rest, "\\s+")[[1]]
}


parse.name.description <- function(key, rest) {
  check_rd(key, rest)
  pieces <- str_split_fixed(rest, "[[:space:]]+", 2)

  name <- pieces[, 1]
  rest <- trim_docstring(pieces[, 2])

  if (is.null.string(name)) {
    stop("@", key, ' requires a name and description')
  }

  list(name = name, description = rest)
}

parse.name <- function(key, name) {
  check_rd(key, name)
  name <- str_trim(name)

  if (is.null.string(name)) {
    stop("@", key, ' requires a name')
  } else if (str_count(name, "\\s+") > 1) {
    stop("@", key, ' should only have a single argument')
  }

  word(name, 1)
}

parse.toggle <- function(key, rest) {
  TRUE
}


check_rd <- function(key, text) {
  if (rdComplete(text)) return(TRUE)

  text <- str_trim(text)
  if (!is.null(key)) {
    stop("Mismatched braces: \"@", key, " ", text, "\"", call. = FALSE)
  } else {
    stop("Mismatched braces: \"", text, "\"", call. = FALSE)
  }

}
