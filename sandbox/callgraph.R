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

preorder.walk.expression <- function(proc, expression) {
  if (length(expression) > 0)
    for (i in c(1:length(expression))) {
      member <- tryCatch(expression[[i]], error=function(e) NULL)
      if (!is.null(member) && !identical(member, expression)) {
        proc(member)
        try(preorder.walk.expression(proc, member),
            silent=TRUE)
      }
    }
}

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

exprofundum <- expression(append)
exprofundum <- expression(is.callable)
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
  cat('digraph G { graph[splines=true]; ')
  nodes <- NULL
  i <- 0
  supercalls <- ls(subcalls)
  for (supercall in supercalls) {
    i <- i + 1
    nodes[[supercall]] <- i
    cat(sprintf('%s [label="%s"]; ', i, supercall))
  }
  for (supercall in supercalls) {
    subsupercalls <- subcalls[[supercall]]
    for (subsupercall in subsupercalls) {
      cat(sprintf('%s -> %s; ',
                  nodes[[supercall]],
                  tryCatch(nodes[[subsupercall]],
                           error=function(e) 'UNKNOWN')))
    }
  }
  cat('}')
}

graphviz(subcalls)
