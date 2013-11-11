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
  str_trim(str_split(rest, fixed(" "))[[1]])
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
