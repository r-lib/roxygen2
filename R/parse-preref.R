# Parse a preref
parse.preref <- function(lines) {
  # Extract srcrefs
  srcrefs <- attr(lines, 'srcref')
  srcrefs$lloc[1] <- srcrefs$lloc[1] + 1

  delimited.lines <- lines[str_detect(lines, LINE.DELIMITER)]
  trimmed.lines <- str_trim(str_replace(delimited.lines, LINE.DELIMITER, ""),
    "right")

  if (length(trimmed.lines) == 0) return(list())

  joined.lines <- str_c(trimmed.lines, collapse = '\n')
  ## Thanks to Fegis at #regex on Freenode for the
  ## lookahead/lookbehind hack; as he notes, however, "it's not
  ## proper escaping though... it will not split a@@@b."
  elements <- strsplit(joined.lines, '(?<!@)@(?!@)', perl = TRUE)[[1]]

  ## Compress the escaped delimeters.
  elements <- str_replace_all(elements, fixed("@@"), "@")

  parsed.introduction <- parse.introduction(elements[[1]])
  parsed.elements <- parse_elements(elements[-1], srcrefs)

  c(parsed.introduction, parsed.elements)
}

# Sequence that distinguishes roxygen comment from normal comment.
LINE.DELIMITER <- '\\s*#+\' ?'

# Comment blocks (possibly null) that precede a file's expressions.
#
# @param srcfile result of running \code{srcfile} on an interesting file
# @param srcrefs the resultant srcrefs
# @return A list of prerefs that resemble srcrefs in form, i.e. with srcfile
#   and lloc
prerefs <- function(srcfile, srcrefs) {
  if (length(srcrefs) == 0) return(list())

  src_start <- vapply(srcrefs, "[[", integer(1), 1) - 1
  src_end <- vapply(srcrefs, "[[", integer(1), 3) + 1

  comments_start <- c(1, src_end[-length(src_end)])
  comments_end <- src_start

  src <- readLines(srcfile$filename, warn = FALSE)

  extract <- function(start, end) {
    srcref <- list(filename = srcfile$filename, lloc = c(start, 0 , end, 0))
    structure(src[start:end], srcref = srcref)
  }

  Map(extract, comments_start, comments_end)
}

parse_elements <- function(elements, srcref) {
  pieces <- str_split_fixed(elements, "[[:space:]]+", 2)
  
  parse_element <- function(tag, value) {
    tag_parser <- preref.parsers[[tag]] %||% parse.unknown
    tag_parser(tag, value, srcref)
  }
  
  Map(parse_element, pieces[, 1], pieces[, 2])
}

# Parse introduction: the premier part of a roxygen block
# containing description and option details separated by
# a blank roxygen line.
#
# @param expression the description to be parsed
# @return A list containing the parsed description
parse.introduction <- function(expression) {
  if (is.null.string(expression)) return(NULL)
  list(introduction = str_trim(expression))
}

#' Default parser which simply emits the key and expression.
#'
#' Used for elements with optional values (like \code{@@export})
#' where roclets can do more sophisticated things with \code{NULL}.
#'
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @param srcref srcref providing location of file name and line number
#' @return A list containing the key and expression (possibly null)
#' @keywords internal
#' @family preref parsing functions
#' @export
parse.default <- function(key, rest, srcref) {
  str_trim(rest)
}

#' Parse an unknown tag.
#'
#' Resorts to the default parser but with a warning about the
#' unknown tag.
#'
#' @inheritParams parse.default
#' @return A list containing the key and expression (possibly null)
#' @family preref parsing functions
#' @keywords internal
#' @export
parse.unknown <- function(key, rest, srcref) {
  roxygen_warning(key, ' is an unknown key', srcref = srcref)
  rest
}

#' Parse an element with a mandatory value.
#'
#' @inheritParams parse.default
#' @return A list containing the key and value
#' @family preref parsing functions
#' @keywords internal
#' @export
parse.value <- function(key, rest, srcref) {
  if (is.null.string(rest)) roxygen_stop(key, ' requires a value', srcref = srcref)
  
  str_trim(rest)
}

#' Parse an element with a least one word
#'
#' @inheritParams parse.default
#' @return A list containing the key and value
#' @family preref parsing functions
#' @keywords internal
#' @export
parse.words <- function(key, rest, srcref) {
  str_trim(str_split(rest, fixed(" "))[[1]])
}

#' Parse an element containing a mandatory name
#' and description (such as \code{@@param}).
#'
#' @inheritParams parse.default
#' @return A list containing the key, name and description
#' @family preref parsing functions
#' @keywords internal
#' @export
parse.name.description <- function(key, rest, srcref) {
  pieces <- str_split_fixed(rest, "[[:space:]]+", 2)

  name <- pieces[, 1]
  rest <- str_trim(pieces[, 2])

  if (is.null.string(name)) {
    roxygen_stop(key, ' requires a name and description', srcref = srcref)
  }
  
  list(name = name, description = rest)
}

#' Parse an element containing a single name and only a name.
#'
#' Extra material will be ignored and a warning issued.
#'
#' @inheritParams parse.default
#' @param name the name to be parsed
#' @return A list containing key and name
#' @family preref parsing functions
#' @keywords internal
#' @export
parse.name <- function(key, name, srcref) {
  name <- str_trim(name)

  if (is.null.string(name)) {
    roxygen_stop(key, ' requires a name', srcref = srcref)
  } else if (str_count(name, "\\s+") > 1) {
    roxygen_warning(key, ' ignoring extra arguments', srcref = srcref)
  }
  
  word(name, 1)
}

#' Turn a binary element on; parameters are ignored.
#'
#' @inheritParams parse.default
#' @return A list with the key and \code{TRUE}
#' @family preref parsing functions
#' @keywords internal
#' @export
parse.toggle <- function(key, rest, srcref) {
  TRUE
}
