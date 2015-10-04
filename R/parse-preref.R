# Parse a preref
parse.preref <- function(lines) {
  delimited.lines <- lines[str_detect(lines, LINE.DELIMITER)]
  trimmed.lines <- str_trim(str_replace(delimited.lines, LINE.DELIMITER, ""),
    "right")

  if (length(trimmed.lines) %==% 0L) return(NULL)

  joined.lines <- paste0(trimmed.lines, collapse = '\n')
  ## Thanks to Fegis at #regex on Freenode for the
  ## lookahead/lookbehind hack; as he notes, however, "it's not
  ## proper escaping though... it will not split a@@@b."
  elements <- strsplit(joined.lines, '(?<!@)@(?!@)', perl = TRUE)[[1]]

  ## Compress the escaped delimeters.
  elements <- str_replace_all(elements, fixed("@@"), "@")

  parse_elements(elements)
}

# Sequence that distinguishes roxygen comment from normal comment.
LINE.DELIMITER <- '\\s*#+\' ?'

parse_elements <- function(elements) {

  pieces <- str_split_fixed(elements[-1], "[[:space:]]+", 2)
  desc <- process_description(str_trim(elements[1]), pieces)
  pieces <- rbind(desc, pieces, deparse.level = 0)

  ## Merge multiple @details tags, one might have come from the intro
  didx <- which(pieces[,1] == "details")
  if (length(didx) > 1) {
    pieces[didx[1], 2] <- paste(pieces[didx, 2], collapse = "\n\n")
    pieces <- pieces[- didx[-1], , drop = FALSE]
  }

  parse_element <- function(tag, value) {
    tag_parser <- preref.parsers[[tag]] %||% parse.unknown
    tag_parser(tag, value)
  }

  Map(parse_element, pieces[, 1], pieces[, 2])
}

# Process title, description and details.
#
# Split the introductory matter into its description followed
# by details (separated by a blank line).
process_description <- function(intro, other_pieces) {

  if (length(intro) == 0 || intro == "") return(NULL)

  paragraphs <- str_trim(str_split(intro, fixed('\n\n'))[[1]])

  piece <- function(p) {
    pc <- paste(other_pieces[other_pieces[,1] == p, 2], collapse = "\n")
    if (length(pc) > 0 && !identical(pc, "")) pc else NULL
  }

  # 1st paragraph = title (unless has @title)
  if (!is.null(piece("title"))) {
    title <- NULL
  } else if (length(paragraphs) > 0) {
    title <- c("title", paragraphs[1])
    paragraphs <- paragraphs[-1]
  } else {
    title <- c("title", "")
  }

  # 2nd paragraph = description (unless has @description)
  if (!is.null(piece("description"))) {
    description <- NULL
  } else if (length(paragraphs) > 0) {
    description <- c("description", paragraphs[1])
    paragraphs <- paragraphs[-1]
  } else {
    # Description is required, so if missing description, repeat title.
    description <- c("description", title[2] %||% piece("title"))
  }

  # Every thing else = details, combined with @details, in parse_elements
  if (length(paragraphs) == 0 || paragraphs == "") paragraphs <- NULL
  if (length(paragraphs) > 0) {
    details <- c("details", paste(paragraphs, collapse = "\n\n"))
  } else {
    details <- NULL
  }

  rbind(title, description, details, deparse.level = 0)
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
  if (rest %==% "") {
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
