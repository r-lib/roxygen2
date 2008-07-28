source('../R/roxygen.R')
source('../R/functional.R')
source('../R/list.R')
source('../R/parse.R')
source('../R/string.R')
source('../R/roclet.R')
source('../R/Rd.R')
source('../R/namespace.R')
source('../R/collate.R')
source('../R/roxygenize.R')

make.stack <- function() {
  stack <- new.env(parent=emptyenv())
  stack$top <- 0
  stack$max.depth <- 0
  stack$elements <- NULL
  stack$is.empty <- function() stack$top == 0
  stack$push <- function(x) {
    stack$top <- stack$top + 1
    stack$max.depth <- max(stack$max.depth,
                            stack$top)
    stack$elements[stack$top] <- x
  }
  stack$pop <- function() {
    if (stack$is.empty())
      stop('Stack underflow')
    stack$top <- stack$top - 1
    stack$elements[[stack$top + 1]]
  }
  stack$peek <- function() {
    if (stack$is.empty())
      stop('Stack underflow')
    stack$elements[[stack$top]]
  }
  stack
}

call.stack <- make.stack()

subcalls <- new.env(parent=emptyenv())

calls <- NULL

is.callable <- function(name)
  exists(name, mode='function')

exprofundum <- expression(roxygenize)

discover.subcalls <- function(exprofundum, depth=2)
  if (is.name(exprofundum)) {
    subcall <- as.character(exprofundum)
    if (is.callable(subcall) && call.stack$top <= depth) {
      supercall <-
        if (call.stack$is.empty())
          NULL
        else
          call.stack$peek()
      if (!is.null(supercall)) {
        subsupercalls <- subcalls[[supercall]]
        if (!subcall %in% subsupercalls)
          subcalls[[supercall]] <<-
            append(subsupercalls, subcall)
      }
      if (!subcall %in% calls) {
        call.stack$push(subcall)
        calls <<- append(subcall, calls)
        subcalls[[subcall]] <<- NULL
        body <- tryCatch(body(subcall), error=function(e) NULL)
        if (!is.null(body))
          preorder.walk.expression(discover.subcalls, body)
        call.stack$pop()
      }
    }
  }

preorder.walk.expression(discover.subcalls, exprofundum)

graphviz <- function(subcalls) {
  cat('digraph G { graph [splines=true]; node [fontname=monospace]; ')
  supercalls <- ls(subcalls)
  for (supercall in supercalls) {
    cat(sprintf('"%s"; ', supercall))
    subsupercalls <- subcalls[[supercall]]
    for (subsupercall in subsupercalls)
      cat(sprintf('"%s" -> "%s"; ',
                  supercall,
                  subsupercall))
  }
  cat('}')
}

graphviz(subcalls)
