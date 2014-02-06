# Parse a preref
parse.preref <- function(lines) {
  delimited.lines <- lines[str_detect(lines, LINE.DELIMITER)]
  trimmed.lines <- str_trim(str_replace(delimited.lines, LINE.DELIMITER, ""),
    "right")

  if (length(trimmed.lines) == 0) return(NULL)

  joined.lines <- str_c(trimmed.lines, collapse = '\n')
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

#' Parsers.
#'
#' These function implement parsing different tag types.
#'
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @keywords internal
#' @name parsers
NULL

#' @details \code{parse.default}: emit value unchanged.
#' @export
#' @rdname parsers
parse.default <- function(key, rest) {
  check_rd(key, rest)
  str_trim(rest)
}

#' @details \code{parse.default}: throws errors about unknown tag.
#' @export
#' @rdname parsers
parse.unknown <- function(key, rest) {
  stop("@", key, " is an unknown key")
}

#' @details \code{parse.value}: fail if empty
#' @export
#' @rdname parsers
parse.value <- function(key, rest) {
  check_rd(key, rest)
  if (is.null.string(rest)) {
    stop("@", key, ' requires a value')
  }

  str_trim(rest)
}

#' @details \code{parse.words}: parse values into words separated by space
#' @export
#' @rdname parsers
parse.words <- function(key, rest) {
  check_rd(key, rest)
  str_split(str_trim(rest), "\\s+")[[1]]
}

#' @details \code{parse.description}: parse mandatory name and description
#' @export
#' @rdname parsers
parse.name.description <- function(key, rest) {
  check_rd(key, rest)
  pieces <- str_split_fixed(rest, "[[:space:]]+", 2)

  name <- pieces[, 1]
  rest <- str_trim(pieces[, 2])

  if (is.null.string(name)) {
    stop("@", key, ' requires a name and description')
  }

  list(name = name, description = rest)
}

#' @details \code{parse.name}: one and only one word
#' @export
#' @rdname parsers
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

#' @details \code{parse.toggle}: turn binary element on
#' @export
#' @rdname parsers
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
