#' @include list.R
#' @include string.R
#' @include roclet.R
#' @include parse.R
#' @import stringr
NULL

register.preref.parsers(parse.value,
                        'name',
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
make.had.roclet <- function(package.dir, 
                           roxygen.dir, 
                           subdir=NULL,
                           verbose=TRUE) {
                             
  if (is.null(subdir)) {
    subdir <- file.path(roxygen.dir, MAN.DIR)
  }
  subdir <- normalizePath(subdir)
  
  #' Translate a key and expressions into an Rd expression;
  #' multiple expressions take their own braces.
  #' @param key the expression's key
  #' @param \dots the arguments
  #' @return A string containing the key and arguments
  #' in LaTeX-like gestalt.
  Rd.expression <- function(key, ...)
    sprintf('\\%s%s\n',
            key,
            Reduce.paste(function(expression)
                         sprintf('{%s}', trim(expression)),
                         c(...),
                         ''))
  
  
  #' Push the Rd-expression to standard out (or current
  #' sink).
  #' @param key the expression's key
  #' @param \dots the arguments
  #' @return \code{NULL}
  parse.expression <- function(key, ...) {
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
      
      cat(sprintf('Writing %s\n', basename(filename)))
      writeLines(contents, filename)
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
  params <- NULL
  examples <- NULL
  has_contents <- list()
  
  add.output <- function(expr) {
    if (is.null(filename)) stop("Trying to write to unknown file")
   
    output[[filename]] <- c(output[[filename]], expr)
  }
  should_write <- function() {
    has_contents[filename] <<- TRUE
  }

  reset <- function() {
    filename <<- NULL
    params <<- NULL
    examples <<- NULL
  }

  first.source.line <- function(partitum) {
    srcfile <- srcfile(partitum$srcref$filename)
    first.line <- partitum$srcref$lloc[[1]]
    getSrcLines(srcfile, first.line, first.line)
  }

  #' What does the noop look like?
  NULL.STATEMENT <- 'NULL'

  #' Does the statement contain a noop-like?
  #' @param source.line the line of source code
  #' @return Whether the statement contains a noop
  is.null.statement <- function(source.line)
    length(grep(NULL.STATEMENT, source.line) > 0)

  #' @note Doesn't work recursively!
  de.tex <- function(string)
    gsub('\\\\[^{]*\\{([^}]*)(}|)',
         '\\1',
         string,
         perl=TRUE)

  #' First sentence of a string, defined as first
  #' period, question mark or newline.
  #' @param description the string to be first-sentenced
  #' @return The first sentence
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
        paste(trim(sentence), '...', sep='')
    }
  }

  #' If \code{@@title} is specified, use it; or
  #' take the first sentence of the description;
  #' or, lastly, take the name.
  #' @param partitum the parsed elements
  #' @param name the calculated name-fallback
  #' @return The parsed title
  parse.title <- function(partitum, name) {
    if (!is.null(partitum$title)) {
      should_write()
      partitum$title      
    } 
    else if (!is.null(first.sentence <-
                      first.sentence(partitum$description)))
      first.sentence
    else
      name
  }
  
  #' Reconstruct the \name directive from amongst
  #' \code{@@name}, \code{@@setMethod}, \code{@@setClass},
  #' \code{@@setGeneric}, \code{@@assignee}, etc.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.name <- function(partitum) {
    name <- guess.name(partitum)

    if (is.null(name) && !is.null(subdir)) {
      filename <<- partitum$srcref$filename
      first.line <- partitum$srcref$lloc[[1]]
      first.source.line <- first.source.line(partitum)
      # if (!is.null.statement(first.source.line))
      #   warning(sprintf(paste('No name found for the',
      #                         'following expression in %s',
      #                         'line %s:\n  `%s . . .\''),
      #                   filename,
      #                   first.line,
      #                   first.source.line),
      #           immediate.=TRUE)
    } else if (!is.null(name)) {
      name <- trim(name)
      filename <<- sprintf('%s.Rd', name)
      parse.expression('name', name)
      if (is.null(partitum$aliases))
        parse.expression('alias', name)
    }

    if ((!is.null(name) || !is.null(partitum$title)) &&
        !is.null(title <- parse.title(partitum, name)))
      parse.expression('title', title)
  }
  
  parse.function.name <- function(partitum) {
    if (!is.null(partitum$method))
      Rd.expression('method',
          partitum$method[[1]],
          partitum$method[[2]])
    else
      partitum$assignee
  }

  #' Turn a list of formal arguments into a human-readable
  #' function-like.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.formals <- function(partitum) {
    formals <- partitum$formals
    if (length(formals) == 0) return()
    
    args <- usage(formals)
    usage <- str_c(parse.function.name(partitum), "(", args, ")")
    
    parse.expression('usage', str_wrap(usage, width = 60, exdent = 4))
  }

  #' Prefer explicit \code{@@usage} to a \code{@@formals} list.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.usage <- function(partitum) {
    if (is.null(partitum$usage))
      parse.formals(partitum)
    else {
      should_write()
      parse.expression('usage', partitum$usage)      
    }
  }

  #' Reset params; parse name and usage.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  pre.parse <- function(partitum) {
    reset()
    
    parse.name(partitum)
    parse.usage(partitum)
  }

  #' Parse params.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  post.parse <- function(partitum) {
    parse.arguments()
    parse.examples(partitum)
  }

  roclet <- make.roclet(package.dir,
                        roxygen.dir, 
                        parse.expression,
                        pre.parse,
                        post.parse, 
                        post.files = save.Rd)

  roclet$register.default.parsers('references',
                                  'note',
                                  'author',
                                  'seealso',
                                  'concept',
                                  'docType')

  roclet$register.parser('return',
                         function(key, expressions)
                         parse.expression('value', expressions))

  #' Split a plural into its constituent singulars.
  #' @param key the singular key
  #' @param expressions the plurality of expressions
  #' @return \code{NULL}
  parse.split <- function(key, expressions) {
    expression <- strcar(expressions)
    rest <- strcdr(expressions)
    parse.expression(key, expression)
    if (!is.null.string(rest))
      parse.split(key, rest)
  }

  roclet$register.parser('aliases',
                         function(key, expressions)
                         parse.split('alias', expressions))

  roclet$register.parser('keywords',
                         function(key, expressions)
                         parse.split('keyword', expressions))

  #' Split the introductory matter into its description followed
  #' by details (separated by a blank line).
  #' @param key ignored
  #' @param expressions the to-be-parsed description and details
  #' @return \code{NULL}
  parse.description <- function(key, expressions) {
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

  roclet$register.parser('description', parse.description)

  #' Add a parameter to the global param list.
  #' @param key ignored
  #' @param expression the parameter
  #' @return \code{NULL}
  parse.param <- function(key, expression) {
    params <<- append(params, list(expression))
  }
    

  parse.arguments <- function() {
    if (length(params) > 0) {
      
      name <- sapply(params, "[[", "name")
      desc <- trim(sapply(params, "[[", "description"))
      
      params_str <- paste(
        "  \\item{", name, "}{",  desc, "}", 
        sep = "", collapse = "\n")
      
      expr <- paste("\\arguments{\n", params_str, "\n}\n", sep = "")
      
      add.output(expr)
    }
  }

  roclet$register.parser('param', parse.param)

  #' Parse individual \code{@@example} clauses by adding the
  #' pointed-to file to a global store.
  #' @param key ignored
  #' @param expression the file containing the example(s)
  #' @return \code{NULL}
  parse.example <- function(key, expression) {
    examples <<- append(examples, expression)
  }

  #' If \code{@@examples} is provided, use that; otherwise, concatenate
  #' the files pointed to by each \code{@@example}.
  #' @param partitum the parsed elements
  #' @return \code{NULL}
  parse.examples <- function(partitum) {
    if (!is.null(partitum$examples)) {
      ex <- partitum$examples
      ex <- gsub("([%\\])", "\\\\\\1", ex)
      ex <- gsub("\\\\dont", "\\dont", ex)
      parse.expression('examples', ex)
    } else {
      examples <- Reduce(c, Map(function(file)
                                tryCatch(readLines(trim(file)),
                                         error=function(e) NULL),
                                examples),
                         NULL)
      if (!is.null(examples))
        parse.expression('examples',
            do.call(paste, c(as.list(examples), sep='\n')))
    }
  }

  roclet$register.parser('example', parse.example)

  parse.todo <- function(key, value)
    parse.expression('section', 'TODO', value)

  roclet$register.parser('TODO', parse.todo)

  roclet
}
