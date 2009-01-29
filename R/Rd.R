#' @include parse.R
#' @include list.R
#' @include string.R
#' @include roclet.R
#' @include parse.R
roxygen()

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
                       list(S4class=car(expression)))

register.srcref.parser('setGeneric',
                       function(pivot, expression)
                       list(S4generic=car(expression)))

register.srcref.parser('setMethod',
                       function(pivot, expression)
                       list(S4method=car(expression),
                            signature=cadr(expression)))

#' Make an Rd roclet which parses the given files and, if specified, populates
#' the given subdirectory with Rd files; or writes to standard out.  See
#' \cite{Writing R Extensions}
#' (\url{http://cran.r-project.org/doc/manuals/R-exts.pdf}) for details.
#'
#' The first paragraph of a roxygen block constitutes its description, the
#' subsequent paragraphs its details; moreover, the Rd roclet supports these
#' tags:
#'
#' \tabular{ll}{
#' Roxygen tag \tab Rd analogue\cr
#' \code{@@author} \tab \code{\\author}\cr
#' \code{@@aliases} \tab \code{\\alias, ...}\cr
#' \code{@@concept} \tab \code{\\concept}\cr
#' \code{@@example} \tab \emph{n/a}\cr
#' \code{@@examples} \tab \code{\\examples}\cr
#' \code{@@format} \tab \code{\\format}\cr
#' \code{@@keywords} \tab \code{\\keyword, ...}\cr
#' \code{@@method} \tab \code{\\method}\cr
#' \code{@@name} \tab \code{\\name}\cr
#' \code{@@note} \tab \code{\\note}\cr
#' \code{@@param} \tab \code{\\arguments{\\item, ...}}\cr
#' \code{@@references} \tab \code{\\references}\cr
#' \code{@@return} \tab \code{\\value}\cr
#' \code{@@seealso} \tab \code{\\seealso}\cr
#' \code{@@source} \tab \code{\\source}\cr
#' \code{@@title} \tab \code{\\title}\cr
#' \code{@@TODO} \tab \emph{n/a}\cr
#' \code{@@usage} \tab \code{\\usage}\cr
#' }
#'
#' \enumerate{
#' \item{\code{@@author}}{See \dQuote{2.1.1 Documenting functions} from
#'                        \cite{Writing R Extensions}.}
#' \item{\code{@@aliases}}{A default alias is plucked from the \code{@@name} or
#'                         assignee; otherwise, \code{@@alias a b ...} translates
#'                         to \code{\alias{a}}, \code{\alias{b}}, &c.
#'                         If you specify one alias, however, specify them all.}
#' \item{\code{@@concept}}{See \dQuote{2.8 Indices} from
#'                         \cite{Writing R Extensions}.}
#' \item{\code{@@example}}{Each \code{@@example} tag specifies an example file
#'                         relative to the package head; if the file resides in
#'                         \file{tests}, for instance, it will be checked with
#'                         \command{R CMD check}.
#'                         The contents of the file will
#'                         be concatenated under \code{\examples{...}}.}
#' \item{\code{@@examples}}{Verbatim examples; see \dQuote{2.1.1
#'                          Documenting functions} from \cite{Writing R
#'                          Extensions}.}
#' \item{\code{@@format}}{See \dQuote{2.1.2 Documenting data sets} from
#'                        \cite{Writing R Extensions}.}
#' \item{\code{@@keywords}}{\code{@@keywords a b ...} translates to
#'                          \code{\keyword{a}}, \code{\keyword{b}}, &c.}
#' \item{\code{@@method}}{Use \code{@@method <generic> <class>} to document
#'                        S3 functions.}
#' \item{\code{@@name}}{In the absense of an explicit \code{@@name} tag, the
#'                      name of an assignment is plucked from the assignee.}
#' \item{\code{@@note}}{See \dQuote{2.1.1 Documenting functions} from
#'                      \cite{Writing R Extensions}.}
#' \item{\code{@@param}}{Each function variable should have a
#'                       \code{@@param <variable> <description>} specified.}
#' \item{\code{@@references}}{See \dQuote{2.1.1 Documenting functions} from
#'                            \cite{Writing R Extensions}.}
#' \item{\code{@@return}}{The return value of the function, or \code{NULL}.}
#' \item{\code{@@seealso}}{See \dQuote{2.1.1 Documenting functions} from
#'                         \cite{Writing R Extensions}.}
#' \item{\code{@@source}}{See \dQuote{2.1.2 Documenting data sets} from
#'                        \cite{Writing R Extensions}.}
#' \item{\code{@@title}}{A default title is plucked from the first sentence
#'                       of the description; that is, the first phrase ending
#'                       with a period, question mark or newline.
#'                       In the absence of a description, the title becomes
#'                       the \code{@@name} or assignee; lastly, it can be
#'                       overridden with \code{@@title}.}
#' \item{\code{@@TODO}}{Note to developers to get off their asses.}
#' \item{\code{@@usage}}{A default usage is construed from a function's formals,
#'                       but can be overridden with \code{@@usage} (e.g. in the case
#'                       of multiple functions in one Rd unit).}
#' }
#'
#' @param subdir directory into which to place the Rd files; if
#' \code{NULL}, standard out.
#' @param verbose whether to declare what we're doing in the
#' \var{subdir}
#' @return Rd roclet
#' @examples
#' #' This sentence describes the function.
#' #'
#' #' Here are the details (notice the preceding blank
#' #' line); the name, title, usage and alias will be
#' #' automatically generated.
#' #'
#' #' @@param a a parameter
#' #' @@return NULL
#' f <- function(a=1) NULL
#'
#' #' S3 functions require a @@method tag for
#' #' the time being.
#' #'
#' #' @@method specialize foo
#' #' @@param f a generic foo
#' #' @@param ... ignored
#' #' @@return The specialized foo
#' specialize.foo <- function(f, ...)
#'   actually.specialize(f)
#'
#' roclet <- make.Rd.roclet('man')
#' \dontrun{roclet$parse('example.R')}
#' @export
#' @aliases name aliases title usage references concept
#' note seealso example examples keywords return author
#' @TODO param method setClass setGeneric setMethod
#' make.Rd.roclet
make.Rd.roclet <- function(subdir=NULL,
                           verbose=TRUE) {
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
  parse.expression <- function(key, ...)
    cat(Rd.expression(key, c(...)), file=filename, append=TRUE)

  filename <- ''

  reset.filename <- function()
    assign.parent('filename', '', environment())

  first.source.line <- function(partitum) {
    srcfile <- srcfile(partitum$srcref$filename)
    first.line <- car(partitum$srcref$lloc)
    getSrcLines(srcfile, first.line, first.line)
  }

  #' What does the noop look like?
  NULL.STATEMENT <- 'roxygen[[:space:]]*()'

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
    if (!is.null(partitum$title))
      partitum$title
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
      filename <- partitum$srcref$filename
      first.line <- car(partitum$srcref$lloc)
      first.source.line <- first.source.line(partitum)
      if (!is.null.statement(first.source.line))
        warning(sprintf(paste('No name found for the',
                              'following expression in %s',
                              'line %s:\n  `%s . . .\''),
                        filename,
                        first.line,
                        first.source.line),
                immediate.=TRUE)
    } else if (!is.null(name)) {
      name <- trim(name)
      if (!is.null(subdir)) {
        assign.parent('filename',
                      file.path(subdir, sprintf('%s.Rd', name)),
                      environment())
        if (verbose)
          cat(sprintf('Writing %s to %s\n', name, filename))
        unlink(filename)
      }
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
          car(partitum$method),
          cadr(partitum$method))
    else
      partitum$assignee
  }

  #' Turn a list of formal arguments into a human-readable
  #' function-like.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.formals <- function(partitum) {
    formals <- partitum$formals
    if (!is.null(formals)) {
      name.defaults <- zip.c(names(formals), formals)
      args <-
        do.call(paste, c(Map(function(name.default) {
          name <- car(name.default)
          default <- cadr(name.default)
          if (is.null.string(default))
            name
          else
            sprintf('%s=%s', name, default)
        },
                             name.defaults),
                         sep=', '))
      parse.expression('usage',
          do.call(paste,
                  c(as.list(strwrap
                            (sprintf('%s(%s)',
                                     parse.function.name(partitum),
                                     args),
                             exdent=4)),
                    sep='\n')))
    }
  }

  #' Prefer explicit \code{@@usage} to a \code{@@formals} list.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  parse.usage <- function(partitum) {
    if (is.null(partitum$usage))
      parse.formals(partitum)
    else
      parse.expression('usage', partitum$usage)
  }

  #' Reset params; parse name and usage.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  pre.parse <- function(partitum) {
    assign.parent('params', NULL, environment())
    assign.parent('examples', NULL, environment())
    parse.name(partitum)
    parse.usage(partitum)
  }

  #' Parse params.
  #' @param partitum the pre-parsed elements
  #' @return \code{NULL}
  post.parse <- function(partitum) {
    parse.arguments()
    parse.examples(partitum)
    ## Assuming the previous sink was successful;
    ## if not, it will destroy the sink stack.
    ## (Should fail if unwritable, anyway.)
    reset.filename()
  }

  roclet <- make.roclet(parse.expression,
                        pre.parse,
                        post.parse)

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
    paragraphs <- car(strsplit(car(expressions), '\n\n', fixed=TRUE))
    description <- car(paragraphs)
    details <- do.call(paste, append(cdr(paragraphs), list(sep='\n\n')))
    parse.expression('description', description)
    if (length(details) > 0 && !is.null.string(details))
      parse.expression('details', details)
  }

  roclet$register.parser('description', parse.description)

  params <- NULL

  #' Add a parameter to the global param list.
  #' @param key ignored
  #' @param expression the parameter
  #' @return \code{NULL}
  parse.param <- function(key, expression)
    assign.parent('params',
                  append(params, list(expression)),
                  environment())
    
  #' Reduce and paste together the various parameters
  #' in an Rd-readable list (with \code{\\item}s, etc.).
  #' @param name.param name-param pair
  #' @return A list of Rd-readable expressions
  parse.params <- function()
    Reduce.paste(function(name.param)
                 Rd.expression('item',
                     car(name.param),
                     cadr(name.param)),
                 params,
                 '')

  #' Paste and label the Rd-readable expressions
  #' returned by \code{parse.params}.
  #' @return \code{NULL}
  parse.arguments <- function()
    if (length(params) > 0)
      parse.expression('arguments', parse.params())

  roclet$register.parser('param', parse.param)

  examples <- NULL

  #' Parse individual \code{@@example} clauses by adding the
  #' pointed-to file to a global store.
  #' @param key ignored
  #' @param expression the file containing the example(s)
  #' @return \code{NULL}
  parse.example <- function(key, expression)
    assign.parent('examples',
                  append(examples, expression),
                  environment())

  #' If \code{@@examples} is provided, use that; otherwise, concatenate
  #' the files pointed to by each \code{@@example}.
  #' @param partitum the parsed elements
  #' @return \code{NULL}
  parse.examples <- function(partitum) {
    if (!is.null(partitum$examples))
      parse.expression('examples', partitum$examples)
    else {
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
