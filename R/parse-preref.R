# Parse a preref
parse.preref <- function(ref) {
  lines <- str_trim(getSrcLines(attributes(ref)$srcfile, ref[[1]], ref[[3]]))
  
  delimited.lines <- lines[str_detect(lines, LINE.DELIMITER)]
  trimmed.lines <- str_trim(str_replace(delimited.lines, LINE.DELIMITER, ""))

  if (length(trimmed.lines) == 0) return(list())

  joined.lines <- str_c(trimmed.lines, collapse = '\n')
  ## Thanks to Fegis at #regex on Freenode for the
  ## lookahead/lookbehind hack; as he notes, however, "it's not
  ## proper escaping though... it will not split a@@@b."
  elements <- strsplit(joined.lines, '(?<!@)@(?!@)', perl = TRUE)[[1]]

  ## Compress the escaped delimeters.
  elements <- str_replace_all(elements, fixed("@@"), "@")

  parsed.description <- parse.description(elements[[1]])
  parsed.elements <- unlist(lapply(elements[-1], parse.element), 
    recursive = FALSE)
  
  c(parsed.description, parsed.elements)
} 

# Sequence that distinguishes roxygen comment from normal comment.
LINE.DELIMITER <- '#+\''

# Comment blocks (possibly null) that precede a file's expressions.
#
# @param srcfile result of running \code{srcfile} on an interesting file
# @param srcrefs the resultant srcrefs
# @return A list of prerefs that resemble srcrefs in form, i.e. with srcfile
#   and lloc
prerefs <- function(srcfile, srcrefs) {
  if (length(srcrefs) == 0) return(list())
  
  length.line <- function(lineno)
    nchar(getSrcLines(srcfile, lineno, lineno))

  pair.preref <- function(start, end) {
    structure(srcref(srcfile, c(start, 1, end, length.line(end))),
      class = 'preref')
  }
  
  src_start <- vapply(srcrefs, "[[", integer(1), 1) - 1
  src_end <- vapply(srcrefs, "[[", integer(1), 3) + 1
  
  comments_start <- c(1, src_end[-length(src_end)])
  comments_end <- src_start

  Map(pair.preref, comments_start, comments_end)
}

# Parse a raw string containing key and expressions.
#
# @param element the string containing key and expressions
# @return A list containing the parsed constituents
parse.element <- function(element) {
  pieces <- str_split_fixed(element, "[[:space:]]+", 2)
  
  tag <- pieces[, 1]
  rest <- pieces[, 2]
  
  tag_parser <- preref.parsers[[tag]] %||% parse.unknown 
  tag_parser(tag, rest)
}

# Parse description: the premier part of a roxygen block
# containing description and option details separated by
# a blank roxygen line.
#
# @param expression the description to be parsed
# @return A list containing the parsed description
parse.description <- function(expression) {
  if (is.null.string(expression)) return(NULL)
  list(description = str_trim(expression))
}

#' Default parser which simply emits the key and expression;
#' used for elements with optional values (like \code{@@export})
#' where roclets can do more sophisticated things with \code{NULL}.
#'
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key and expression (possibly null)
#' @keywords internal
#' @export
parse.default <- function(key, rest)
  as.list(structure(str_trim(rest), names=key))

# Resorts to the default parser but with a warning about the
# unknown key.
#
# @param key the parsing key
# @param rest the expression to be parsed
# @return A list containing the key and expression (possibly
# null)
# @seealso \code{\link{parse.default}}
parse.unknown <- function(key, rest) {
  warning(key, ' is an unknown key', call. = FALSE)
  parse.default(key, rest)
}

#' Parse an element with a mandatory value.
#'
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key and value
#' @keywords internal
#' @export
parse.value <- function(key, rest) {
  if (is.null.string(rest))
    stop(key, 'requires a value', call. = FALSE)
  else
    parse.default(key, rest)
}
  
#' Parse an element containing a mandatory name
#' and description (such as \code{@@param}).
#'
#' @param key the parsing key
#' @param rest the expression to be parsed
#' @return A list containing the key, name and description
#' @keywords internal
#' @export
parse.name.description <- function(key, rest) {
  pieces <- str_split_fixed(rest, "[[:space:]]+", 2)
  
  name <- pieces[, 1]
  rest <- str_trim(pieces[, 2])

  if (is.null.string(name))
    stop(key, 'requires a name and description', call. = FALSE)
  else
    as.list(structure(list(list(name=name,
                                description=rest)),
                      names=key))
}

#' Parse an element containing a single name and only a name;
#' extra material will be ignored and a warning issued.
#'
#' @param key parsing key
#' @param name the name to be parsed
#' @return A list containing key and name
#' @keywords internal
#' @export
parse.name <- function(key, name) {
  name <- str_trim(name)
  
  if (is.null.string(name)) {
    stop(key, ' requires a name', call. = FALSE)
  } else if (str_count(name, "\\s+") > 1) {
    warning(key, ' ignoring extra arguments', call. = FALSE)
  }
    
  parse.default(key, word(name, 1))
}

#' Turn a binary element on; parameters are ignored.
#'
#' @param key parsing key
#' @param rest the expression to be parsed
#' @return A list with the key and \code{TRUE}
#' @keywords internal
#' @export
parse.toggle <- function(key, rest)
  as.list(structure(TRUE, names=key))
