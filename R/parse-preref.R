# Parse a preref
parse.preref <- function(lines) {
  # Extract srcrefs (needed for error messages)
  srcrefs <- attr(lines, 'srcref')
  srcrefs$lloc[1] <- srcrefs$lloc[1] + 1

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

  parsed <- parse_elements(elements[-1], srcrefs)
  if (elements[[1]] != "") {
    parsed$introduction <- str_trim(elements[[1]])
  }
  parsed
}

# Sequence that distinguishes roxygen comment from normal comment.
LINE.DELIMITER <- '\\s*#+\' ?'

parse_elements <- function(elements, srcref) {
  pieces <- str_split_fixed(elements, "[[:space:]]+", 2)
  
  parse_element <- function(tag, value) {
    tag_parser <- preref.parsers[[tag]] %||% parse.unknown
    tag_parser(tag, value, srcref)
  }
  
  Map(parse_element, pieces[, 1], pieces[, 2])
}

#' Parsers.
#' 
#' These function implement parsing different tag types.
#' 
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @param srcref srcref providing location of file name and line number
#' @keywords internal
#' @name parsers
NULL

#' @details \code{parse.default}: emit value unchanged.
#' @export
#' @rdname parsers
parse.default <- function(key, rest, srcref) {
  str_trim(rest)
}

#' @details \code{parse.default}: warns about unknown tag.
#' @export
#' @rdname parsers
parse.unknown <- function(key, rest, srcref) {
  roxygen_warning(key, ' is an unknown key', srcref = srcref)
  str_trim(rest)
}

#' @details \code{parse.value}: fail if empty
#' @export
#' @rdname parsers
parse.value <- function(key, rest, srcref) {
  if (is.null.string(rest)) {
    roxygen_stop(key, ' requires a value', srcref = srcref)
  }
  
  str_trim(rest)
}

#' @details \code{parse.words}: parse values into words separated by space
#' @export
#' @rdname parsers
parse.words <- function(key, rest, srcref) {
  str_split(str_trim(rest), "\\s+")[[1]]
}

#' @details \code{parse.description}: parse mandatory name and description
#' @export
#' @rdname parsers
parse.name.description <- function(key, rest, srcref) {
  pieces <- str_split_fixed(rest, "[[:space:]]+", 2)

  name <- pieces[, 1]
  rest <- str_trim(pieces[, 2])

  if (is.null.string(name)) {
    roxygen_stop(key, ' requires a name and description', srcref = srcref)
  }
  
  list(name = name, description = rest)
}

#' @details \code{parse.name}: one and only one word
#' @export
#' @rdname parsers
parse.name <- function(key, name, srcref) {
  name <- str_trim(name)

  if (is.null.string(name)) {
    roxygen_stop(key, ' requires a name', srcref = srcref)
  } else if (str_count(name, "\\s+") > 1) {
    roxygen_warning(key, ' ignoring extra arguments', srcref = srcref)
  }
  
  word(name, 1)
}

#' @details \code{parse.toggle}: turn binary element on
#' @export
#' @rdname parsers
parse.toggle <- function(key, rest, srcref) {
  TRUE
}
