#' @include list.R
#' @include string.R
#' @include roclet.R
#' @include parse.R
#' @import stringr
NULL

register.preref.parsers(parse.value,
                        'name', 
                        'rdname',
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


#' @export
had_roclet <- function(package.dir, roxygen.dir, subdir = NULL) {
                             
  if (is.null(subdir)) {
    subdir <- file.path(roxygen.dir, "man")
  }
  subdir <- normalizePath(subdir)
  
  #' Translate a key and expressions into an Rd expression;
  #' multiple expressions take their own braces.
  #' @param key the expression's key
  #' @param \dots the arguments
  #' @return A string containing the key and arguments
  #' in LaTeX-like gestalt.
  Rd.expression <- function(key, ...) {
    str_c("\\", key, str_c("{", str_trim(c(...)), "}", collapse = ""), "\n")                         
  }
  
  
  #' Push the Rd-expression to standard out (or current
  #' sink).
  #' @param key the expression's key
  #' @param \dots the arguments
  #' @return \code{NULL}
  process.expression <- function(key, ...) {
    expr <- Rd.expression(key, c(...))
    add.output(expr)
  }

  #' Save all Rd files to disk
  save.Rd <- function() {
    to_write <- names(Filter(isTRUE, has_contents))
    topics <- as.list(output, all.names = TRUE)[to_write]
    contents <- vapply(topics, paste, character(1), collapse = "")
    
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
    
    paths <- file.path(subdir, names(topics))
    mapply(write_out, paths, contents)    
  }

  filename <- NULL
  output <- new.env(TRUE, emptyenv())
  has_contents <- list()
  
  add.output <- function(expr) {
    if (is.null(filename)) return()
   
    output[[filename]] <- c(output[[filename]], expr)
  }
  should_write <- function() {
    has_contents[filename] <<- TRUE
  }

  #' Reconstruct the \name directive from amongst
  #' \code{@@name}, \code{@@setMethod}, \code{@@setClass},
  #' \code{@@setGeneric}, assignee, etc.
  #' @param partitum the pre-processd elements
  #' @return \code{NULL}
  process.name <- function(partitum) {
    name <- partitum$name %||% partitum$assignee %||% partitum$S4class %||% 
      partitum$S4method %||% partitum$S4generic
    
    if (is.null(name)) {
      # No name, so skip this file.  Do this silently because of other
      # roclets: e.g. collate and namespace.  Should really check that the
      # next line is NULL.
      filename <<- NULL
      return()
    }
    filename <<- sprintf('%s.Rd', partitum$rdname %||% name)
    
    process.expression('name', name)

    # If no aliases, use name
    if (is.null(partitum$aliases)) {
      process.expression('alias', name)
    }

    title <- partitum$title %||% first.sentence(partitum$description) %||%
      name
    process.expression('title', title)
  }
  
  #' Prefer explicit \code{@@usage} to a \code{@@formals} list.
  #' @param partitum the pre-processd elements
  #' @return \code{NULL}
  process.usage <- function(partitum) {
    if (!is.null(partitum$usage)) {
      should_write()
      process.expression('usage', partitum$usage)   
      return()   
    }
    
    formals <- partitum$formals
    if (length(formals) == 0) return()
    
    args <- usage(formals)
    
    fun_name <- if (!is.null(partitum$method)) {
      Rd.expression('method', partitum$method[[1]], partitum$method[[2]])
    } else {
      partitum$assignee
    }
    usage <- str_c(fun_name, "(", args, ")")
    
    process.expression('usage', str_wrap(usage, width = 60, exdent = 4))
  }

  #' Reset params; process name and usage.
  #' @param partitum the pre-processd elements
  #' @return \code{NULL}
  pre.process <- function(partitum) {
    process.name(partitum)
    process.usage(partitum)
  }

  #' process params.
  #' @param partitum the pre-processd elements
  #' @return \code{NULL}
  post.process <- function(partitum) {
    process.arguments(partitum)
    process.examples(partitum)
  }


  #' Split the introductory matter into its description followed
  #' by details (separated by a blank line).
  #' @param key ignored
  #' @param expressions the to-be-processd description and details
  #' @return \code{NULL}
  process.description <- function(key, expressions) {
    should_write()
    paragraphs <- strsplit(expressions[[1]], '\n\n', fixed=TRUE)[[1]]
    description <- paragraphs[[1]]
    details <- do.call(paste, append(paragraphs[-1], list(sep='\n\n')))

    description <- paste(strwrap(description, exdent = 2, indent = 2, width = 60),
      collapse = "\n")
    expr <- paste("\n\\description{\n", description, "\n}\n", sep = "")
    add.output(expr)
    
    if (length(details) > 0 && !is.null.string(details)) {
      details <- paste(strwrap(details, exdent = 2, indent = 2, width = 60), collapse = "\n")
      expr <- paste("\n\\details{\n", details, "\n}\n", sep = "")
      add.output(expr)
    }
      
  }
  process.arguments <- function(partitum) {
    params <- partitum[names(partitum) == "param"]
    if (length(params) == 0) return() 

    name <- sapply(params, "[[", "name")
    desc <- str_trim(sapply(params, "[[", "description"))
    
    params_str <- paste(
      "  \\item{", name, "}{",  desc, "}", 
      sep = "", collapse = "\n")
    
    expr <- paste("\\arguments{\n", params_str, "\n}\n", sep = "")
    
    add.output(expr)
  }

  #' If \code{@@examples} is provided, use that; otherwise, concatenate
  #' the files pointed to by each \code{@@example}.
  #' @param partitum the processd elements
  #' @return \code{NULL}
  process.examples <- function(partitum) {
    if (!is.null(partitum$examples)) {
      should_write()
      
      ex <- partitum$examples
      ex <- gsub("([%\\])", "\\\\\\1", ex)
      ex <- gsub("\\\\dont", "\\dont", ex)
      process.expression('examples', ex)
    } 
    
    paths <- partitum[names(partitum) == "example"]
    if (length(paths) > 0) {
      paths <- file.path(package.dir, str_trim(paths))
      examples <- unlist(lapply(readLines, paths))
      
      process.expression('examples', paste(examples, collapse = "\n"))
    }
  }

  process.todo <- function(key, value)
    process.expression('section', 'TODO', value)


  roclet <- make.roclet(package.dir,
                        roxygen.dir, 
                        process.expression,
                        pre.process,
                        post.process, 
                        post.files = save.Rd)

  roclet$register.default.parsers('references',
                                  'note',
                                  'author',
                                  'seealso',
                                  'concept',
                                  'docType')
  roclet$register.parser('description', process.description)  
  roclet$register.parser('return', function(key, expressions) {
    process.expression('value', expressions)
  })

  process.split <- function(key, expressions) {
    pieces <- str_split(str_trim(expressions), "\\s+")[[1]]
    lapply(pieces, process.expression, key = key)
  }
  roclet$register.parser('aliases', function(key, expressions) {
    process.split('alias', expressions)
  })
  roclet$register.parser('keywords', function(key, expressions) {
    process.split('keyword', expressions)
  })
  roclet$register.parser('TODO', process.todo)

  roclet
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
