#' @import memoise
NULL

# Sequence that distinguishes roxygen comment from normal comment.
LINE.DELIMITER <- '#+\''

# Symbol that delimits tags.
TAG.DELIMITER <- '@'

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

  do.call(parser.preref(tag), list(tag, rest))
}

# Parse description: the premier part of a roxygen block
# containing description and option details separated by
# a blank roxygen line.
#
# @param expression the description to be parsed
# @return A list containing the parsed description
parse.description <- function(expression)
  list(description=str_trim(expression))

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
parse.preref <- function(key, rest) {
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

# Preref parser-lookup; defaults to \code{parse.preref}.
# @param key the key upon which to look
# @return The parser
parser.preref <- function(key, default) {
  preref.parsers[[key]] %||% default
}

# Srcref parser-lookup; defaults to \code{parse.srcref}.
# @param key the key upon which to look
# @return The parser
parser.srcref <- function(key, default) {
  srcref.parsers[[key]] %||% default
}

#' Parse either srcrefs, prerefs or pairs of the same.
#'
#' @param ref the srcref, preref or pair of the same
#' @param \dots ignored
#' @return List containing the parsed srcref/preref
#' @keywords internal
#' @export
parse.ref <- function(ref, ...) UseMethod('parse.ref')
cached.parse.ref <- memoize(parse.ref)

#' Parse a preref/srcrefs pair
#'
#' @method parse.ref list
#' @param ref the preref/srcref pair
#' @param \dots ignored
#' @return List combining the parsed preref/srcref
#' @keywords internal
#' @export
parse.ref.list <- function(ref, ...)
  append(parse.ref(ref[[1]]),
         parse.ref(ref[[2]]))


#' Parse a preref
#' @method parse.ref preref
#' @param ref the preref to be parsed
#' @param \dots ignored
#' @return List containing the parsed preref
#' @keywords internal
#' @export
parse.ref.preref <- function(ref, ...) {
  lines <- str_trim(getSrcLines(attributes(ref)$srcfile, ref[[1]], ref[[3]]))
  delimited.lines <- lines[str_detect(lines, LINE.DELIMITER)]
  trimmed.lines <- str_trim(str_replace(delimited.lines, LINE.DELIMITER, ""))

  if (length(trimmed.lines) == 0)
    list()
  else {
    joined.lines <- str_c(trimmed.lines, collapse = '\n')
    ## Thanks to Fegis at #regex on Freenode for the
    ## lookahead/lookbehind hack; as he notes, however, "it's not
    ## proper escaping though... it will not split a@@@b."
    elements <- strsplit(joined.lines,
                             sprintf('(?<!%s)%s(?!%s)',
                                     TAG.DELIMITER,
                                     TAG.DELIMITER,
                                     TAG.DELIMITER),
                             perl=TRUE)[[1]]
    ## Compress the escaped delimeters.
    elements <- Map(function(element)
                    gsub(sprintf('%s{2}', TAG.DELIMITER),
                         TAG.DELIMITER,
                         element),
                    elements)
    description <- elements[[1]]
    parsed.elements <- Reduce(function(parsed, element)
                              append(parsed, parse.element(element)),
                              elements[-1],
                              if (is.null.string(description)) NULL
                              else parse.description(description))
  }
} 

#' Parse a srcref
#'
#' @method parse.ref srcref
#' @param ref the srcref to be parsed
#' @param \dots ignored
#' @return List containing the parsed srcref
#' @keywords internal
#' @export
parse.ref.srcref <- function(ref, ...) {
  srcfile <- attributes(ref)$srcfile
  srcref <- list(srcref = 
    list(filename = srcfile$filename, lloc = as.vector(ref)))

  # Get code from source and parse to extract first call
  lines <- getSrcLines(srcfile, ref[[1]], ref[[3]])
  call <- parse(text = lines)[[1]]
  
  if (!is.call(call)) {
    return(c(srcref, list(value = deparse(call))))
  }

  # Dispatch to registered srcref parsers based on call
  name <- as.character(call[[1]])
  
  parser <- srcref.parsers[[name]]
  if (is.null(parser)) return(srcref)
  
  f <- eval(call[[1]])
  # If not a primitive function, use match.call so argument handlers
  # can use argument names
  if (!is.primitive(f)) {
    call <- match.call(eval(call[[1]]), call)    
  }
  c(srcref, parser(call))
}

# Parse each of a list of preref/srcref pairs.
# @param preref.srcrefs list of preref/srcref pairs
# @return List combining parsed preref/srcrefs
parse.refs <- function(preref.srcrefs) {
  lapply(preref.srcrefs, cached.parse.ref)
}
  

#' Parse a source file containing roxygen directives.
#'
#' @param file string naming file to be parsed
#' @return List containing parsed directives
#' @keywords internal
#' @export
parse.file <- function(file) {
  srcfile <- srcfile(file)
  
  res <- try(cached.parse.srcfile(srcfile), silent = TRUE)
  if (inherits(res, "try-error")) {
    stop("Can't parse ", file, "\n", res, call. = FALSE)
  }
  res
}

parse.srcfile <- function(srcfile) {
  src_refs <- attributes(parse(srcfile$filename, srcfile = srcfile))$srcref
  pre_refs <- prerefs(srcfile, src_refs)

  if (length(src_refs) == 0) return(list())
  
  parse.refs(mapply(list, src_refs, pre_refs, SIMPLIFY = FALSE))
}
cached.parse.srcfile <- memoize(parse.srcfile)

#' Parse many files at one.
#'
#' @param \dots files to be parsed
#' @return List containing parsed directives
#' @seealso \code{\link{parse.file}}
#' @keywords internal
#' @export
parse.files <- function(paths) {
  unlist(lapply(paths, parse.file), recursive = FALSE)
}
  
#' Text-parsing hack using tempfiles for more facility.
#'
#' @param \dots lines of text to be parsed
#' @return The parse tree
#' @keywords internal
#' @export
parse.text <- function(...) {
  file <- tempfile()
  cat(..., sep='\n', file=file)
  parse.file(file)
}
