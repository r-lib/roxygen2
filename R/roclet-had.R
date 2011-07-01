#' @include parse.R
#' @import stringr
NULL

register.preref.parsers(parse.value,
                        'name', 
                        'rdname',
                        'merge',
                        'aliases',
                        'title',
                        'usage',
                        'references',
                        'concept',
                        'note',
                        'seealso',
                        'example',
                        'examples',
                        'keywords',
                        'return',
                        'author',
                        'TODO',
                        'format',
                        'source')

register.preref.parsers(parse.name.description,
                        'param',
                        'method')

register.preref.parsers(parse.name,
                        'docType')

register.srcref.parser('setClass',
                       function(pivot, expression)
                       list(S4class=expression[[1]]))

register.srcref.parser('setGeneric',
                       function(pivot, expression)
                       list(S4generic=expression[[1]]))

register.srcref.parser('setMethod',
                       function(pivot, expression)
                       list(S4method=expression[[1]],
                            signature=expression[[2]]))

#' \itemize{
#'  \item \code{@@merge topicname}: Merges contents of this topic with
#'    contents of specific topic, silently dropping name and title components.
#' }
#' @export
had_roclet <- function() {
  new_roclet(list(), "had")
}

#' @S3method roc_process had
roc_process.had <- function(roclet, partita) {
  topics <- list()
  for (partitum in partita) {
    has_rd <- any(names(partitum) %in% c("description", "param", "return",
      "title", "examples", "docType"))
    if (!has_rd) next()
    
    # Figure out topic name
    name <- partitum$name %||% partitum$S4class %||% partitum$S4method %||%
      partitum$S4generic
    # Only use assignee if it's a single element
    if (is.null(name) && length(partitum$assignee) == 1) {
       name <- partitum$assignee
    }
    if (is.null(name)) stop("Missing name")

    # Work out file name and initialise Rd object
    filename <- str_c(partitum$merge %||% partitum$rdname %||% name, ".Rd")
    
    rd <- topics[[filename]]
    if (is.null(rd)) {
      rd <- character()
    }

    title <- partitum$title %||% first.sentence(partitum$description) %||%
      name
    rd <- c(rd, 
      rd_tag('name', name),
      if (is.null(partitum$aliases)) rd_tag('alias', name),
      process_had_tag(partitum, 'aliases', function(tag, param) {
        process.split('alias', param)
      }),
      rd_tag('title', title),
      process.usage(partitum),
      process.arguments(partitum),
      process_had_tag(partitum, 'docType'),
      process_had_tag(partitum, 'description', process.description),
      process_had_tag(partitum, 'note'),
      process_had_tag(partitum, 'author'),
      process_had_tag(partitum, 'seealso'),
      process_had_tag(partitum, "references"),
      process_had_tag(partitum, 'concept'),
      process_had_tag(partitum, 'return', function(tag, param) {
        rd_tag('value', param)
      }),
      process_had_tag(partitum, 'keywords', function(tag, param, all, rd) {
        process.split('keyword', param)
      }),
      process_had_tag(partitum, 'TODO', process.todo),
      process.examples(partitum)
    )
    topics[[filename]] <- rd
  }
  topics
}

#' @S3method roc_output had
roc_output.had <- function(roclet, results, path) { 
  man <- normalizePath(file.path(path, "man"))
  contents <- vapply(results, paste, character(1), collapse = "")
  
  write_out <- function(filename, contents) {
    if (the_same(filename, contents)) return()
    
    name <- basename(filename)
    if (!str_detect(name, "^[a-zA-Z][a-zA-Z0-9_.-]*$")) {
      cat("Skipping invalid filename: ", name, "\n")
    } else {
      cat(sprintf('Writing %s\n', name))
      writeLines(contents, filename)        
    }
    
  }
  the_same <- function(path, new) {
    if (!file.exists(path)) return(FALSE)

    old <- str_c(readLines(path), collapse = "\n")
    return(identical(old, new))
  }
  
  paths <- file.path(man, names(results))
  mapply(write_out, paths, contents)    
}


# Translate a key and expressions into an Rd expression;
# multiple expressions take their own braces.
rd_tag <- function(key, ..., space = FALSE) {
  if (space) {
    values <- str_c("\n", str_c(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }
  str_c("\\", key, str_c("{", values, "}", collapse = ""), "\n")                         
}

# Prefer explicit \code{@@usage} to a \code{@@formals} list.
process.usage <- function(partitum) {
  if (!is.null(partitum$usage)) {
    return(rd_tag('usage', partitum$usage))
  }
  
  formals <- partitum$formals
  if (length(formals) == 0) return()
  
  args <- usage(formals)
  
  fun_name <- if (!is.null(partitum$method)) {
    rd_tag('method', partitum$method[[1]], partitum$method[[2]])
  } else {
    partitum$assignee
  }
  usage <- str_c(fun_name, "(", args, ")")
  
  rd_tag('usage', str_wrap(usage, width = 60, exdent = 4))
}

# Split the introductory matter into its description followed
# by details (separated by a blank line).
process.description <- function(key, expressions) {
  paragraphs <- strsplit(expressions[[1]], '\n\n', fixed=TRUE)[[1]]
  desc <- paragraphs[[1]]
  details <- do.call(paste, append(paragraphs[-1], list(sep='\n\n')))

  desc <- paste(strwrap(desc, exdent = 2, indent = 2, width = 60),
    collapse = "\n")
  desc_rd <- rd_tag("description", desc, space = TRUE)
  
  if (length(details) > 0 && !is.null.string(details)) {
    details <- str_wrap(details, exdent = 2, indent = 2, width = 60)
    
    details_rd <- rd_tag("details", details, space = FALSE)
  } else {
    details_rd <- NULL
  }
  c(desc_rd, details_rd)
}

process.arguments <- function(partitum) {
  params <- partitum[names(partitum) == "param"]
  if (length(params) == 0) return() 

  name <- sapply(params, "[[", "name")
  desc <- str_trim(sapply(params, "[[", "description"))
  
  params_str <- paste(
    "  \\item{", name, "}{",  desc, "}", 
    sep = "", collapse = "\n")
  
  rd_tag("arguments", params_str, space = TRUE)
}

# If \code{@@examples} is provided, use that; otherwise, concatenate
# the files pointed to by each \code{@@example}.
process.examples <- function(partitum) {
  if (!is.null(partitum$examples)) {      
    ex <- partitum$examples
    ex <- gsub("([%\\])", "\\\\\\1", ex)
    ex <- gsub("\\\\dont", "\\dont", ex)
    return(rd_tag('examples', ex))
  } 
  
  paths <- partitum[names(partitum) == "example"]
  if (length(paths) > 0) {
    paths <- file.path(package.dir, str_trim(paths))
    examples <- unlist(lapply(readLines, paths))
    
    return(rd_tag('examples', str_c(examples, collapse = "\n")))
  }
}
process.split <- function(key, expressions) {
  pieces <- str_split(str_trim(expressions), "\\s+")[[1]]
  unlist(lapply(pieces, rd_tag, key = key))
}
process.todo <- function(key, value) {
  rd_tag('section', 'TODO', value)
}

process_had_tag <- function(partitum, tag, f = rd_tag) {
  params <- partitum[[tag]]
  if (is.null(params)) return()
  
  f(tag, params)
}


# @note Doesn't work recursively!
de.tex <- function(string)
  gsub('\\\\[^{]*\\{([^}]*)(}|)',
       '\\1',
       string,
       perl=TRUE)

# First sentence of a string, defined as first
# period, question mark or newline.
# @param description the string to be first-sentenced
# @return The first sentence
first.sentence <- function(description) {
  description <- de.tex(description)
  r <- regexpr('[^.?\n]*(\\.(?!\\w)|\\?|\n|)',
               description,
               perl=TRUE)
  sentence <- substr(description, r, attr(r, 'match.length'))
  if (length(sentence) == 0 || is.null.string(sentence))
    NULL
  else {
    chars <- nchar(sentence)
    last.char <- substr(sentence, chars, chars)
    if (last.char == '.' || last.char == '?')
      sentence
    else
      paste(str_trim(sentence), '...', sep='')
  }
}

# warning("All roxygen elements must have name: ",
#   partitum$srcref$filename, ":", partitum$srcref$lloc[1], ":",
#   partitum$srcref$lloc[2], call. = FALSE)
