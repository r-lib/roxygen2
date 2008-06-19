LINE.DELIMITER <- '#\''
TAG.DELIMITER <- '@'

trim <- function(string)
  gsub('^[[:space:]]+', '',
       gsub('[[:space:]]+$', '', string))

paste.list <- function(list) {
  do.call(paste, list)
}

#' Comment blocks (possibly null) that precede a file's expressions.
prerefs <- function(srcfile, srcrefs) {
  length.line <- function(lineno)
    nchar(getSrcLines(srcfile, lineno, lineno))

  pair.preref <- function(pair) {
    start <- car(pair)
    end <- cadr(pair)
    structure(srcref(srcfile, c(start, 1, end, length.line(end))),
              class='preref')
  }

  lines <- unlist(Map(function(srcref)
                      c(car(srcref) - 1,
                        caddr(srcref) + 1),
                      srcrefs))
  pairs <- pairwise(c(1, lines))
  Map(pair.preref, pairs)
}

## preref parsers

parse.message <- function(key, message)
  sprintf('@%s %s.', key, message)

parse.error <- function(key, message)
  stop(parse.message(key, message))

parse.warning <- function(key, message)
  warning(parse.message(key, message))

parse.element <- function(element) {
  tokens <- car(strsplit(element, ' ', fixed=T))
  parser <- parser.preref(car(tokens))
  do.call(parser, as.list(cdr(tokens)))
}

parse.description <- function(expression)
  list(description=expression)

is.empty <- function(...) is.nil(c(...)) || is.na(car(as.list(...)))

args.to.string <- function(...)
  ifelse(is.empty(...), NA, paste(...))

parse.default <- function(key, ...)
  as.list(structure(args.to.string(...), names=key))

parse.preref <- function(key, ...) {
  parse.warning(key, 'is an unknown key')
  parse.default(key, ...)
}

## Possibly NA; in which case, the Roclets can do something more
## sophisticated with the srcref.
parse.export <- Curry(parse.default, key='export')

parse.value <- function(key, ...) {
  if (is.empty(...))
    parse.error(key, 'requires a value')
  else
    parse.default(key, ...)
}
  
parse.prototype <- Curry(parse.value, key='prototype')

parse.exportClass <- Curry(parse.value, key='exportClass')

parse.exportMethod <- Curry(parse.value, key='exportMethod')

parse.exportPattern <- Curry(parse.value, key='exportPattern')

parse.S3method <- Curry(parse.value, key='S3method')

parse.import <- Curry(parse.value, key='import')

parse.importFrom <- Curry(parse.value, key='importFrom')

parse.importClassesFrom <- Curry(parse.value, key='importClassesFrom')

parse.importMethodsFrom <- Curry(parse.value, key='importMethodsFrom')

parse.return <- Curry(parse.value, key='return')

parse.reference <- Curry(parse.value, key='reference')

parse.author <- Curry(parse.value, key='author')

parse.name.description <- function(key, name, ...) {
  if (any(is.na(name),
          is.empty(...)))
    parse.error(key, 'requires a name and description')
  else
    as.list(structure(list(list(name=name,
                                description=args.to.string(...))),
                      names=key))
}

parse.slot <- Curry(parse.name.description, key='slot')

parse.param <- Curry(parse.name.description, key='param')

parse.name <- function(key, name, ...) {
  if (is.na(name))
    parse.error(key, 'requires a name')
  else if (Negate(is.empty)(...))
    parse.warning(key, 'discards extra-nominal entries')
  parse.default(key, name)
}

parse.S3class <- Curry(parse.name, key='S3class')

parse.returnType <- Curry(parse.name, key='returnType')

parse.toggle <- function(key, ...)
  as.list(structure(T, names=key))

parse.listObject <- Curry(parse.toggle, key='listObject')

parse.attributeObject <- Curry(parse.toggle, key='attributeObject')

parse.environmentObject <- Curry(parse.toggle, key='environmentObject')

## srcref parsers

parse.srcref <- function(...) nil

parse.setClass <- function(expression)
  list(class=cadr(car(expression)))

parse.setGeneric <- function(expression)
  list(generic=cadr(car(expression)))

parse.setMethod <- function(expression)
  list(method=cadr(car(expression)),
       signature=caddr(car(expression)))

## Parser lookup

parser.default <- function(key, default) {
  f <- ls(1, pattern=sprintf('parse.%s', key))[1]
  if (is.na(f)) Curry(default, key=key) else f
}

parser.preref <- Curry(parser.default, default=parse.preref)

parser.srcref <- Curry(parser.default, default=parse.srcref)

## File -> {src,pre}ref mapping

parse.ref <- function(x, ...)
  UseMethod('parse.ref')

parse.ref.list <- function(preref.srcref)
  append(parse.ref(car(preref.srcref)),
         parse.ref(cadr(preref.srcref)))

parse.ref.preref <- function(preref) {
  lines <- getSrcLines(attributes(preref)$srcfile,
                       car(preref),
                       caddr(preref))
  delimited.lines <-
    Filter(function(line) grep(LINE.DELIMITER, line), lines)
  trimmed.lines <-
    Map(function(line) substr(line, nchar(LINE.DELIMITER) + 1, nchar(line)),
        delimited.lines)
  ## Presumption: white-space is insignificant; there are no
  ## multi-line elements. This contradicts, for instance, verbatim or
  ## latex.
  joined.lines <- gsub(' {2,}', ' ', paste.list(trimmed.lines))
  elements <- Map(trim, car(strsplit(joined.lines, TAG.DELIMITER, fixed=T)))
  parsed.elements <- Reduce(function(parsed, element)
                            append(parsed, parse.element(element)),
                            cdr(elements), parse.description(car(elements)))
} 

parse.ref.srcref <- function(srcref) {
  srcfile <- attributes(srcref)$srcfile
  lines <- getSrcLines(srcfile, car(srcref), caddr(srcref))
  expression <- parse(text=lines)
  pivot <- caar(expression)
  parser <- parser.srcref(as.character(pivot))
  append(do.call(parser, list(expression)),
         list(srcref=list(filename=srcfile$filename,
                lloc=as.vector(srcref))))
         
}

parse.refs <- function(prerefs.srcrefs)
  Map(parse.ref, prerefs.srcrefs)

parse.file <- function(file) {
  srcfile <- srcfile(file)
  srcrefs <- attributes(parse(srcfile$filename,
                              srcfile=srcfile))$srcref
  parse.refs(zip.list(prerefs(srcfile, srcrefs), srcrefs))
}
