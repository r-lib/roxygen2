library(graph)
library(Rgraphviz)

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

is.callable <- function(name, include.primitives) {
  f <- tryCatch(get(name, mode='function'), error=function(e) NULL)
  !is.null(f) && ifelse(include.primitives, TRUE, !is.primitive(f))
}

exprofundum <- expression(roxygenize)
exprofundum <- as.expression(call('roxygenize'))

discover.subcalls <- function(exprofundum,
                              depth=2,
                              include.primitives=FALSE)
  if (is.name(exprofundum)) {
    subcall <- as.character(exprofundum)
    if (is.callable(subcall, include.primitives) &&
        call.stack$top <= depth) {
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

## discover.subcalls <- Curry(include.primitives=TRUE,
##                            depth=1)

preorder.walk.expression(discover.subcalls, exprofundum)

formals(toFile) <- alist(graph=,
                         layoutType=c("dot", "neato", "twopi",
                           "circo", "fdp"),
                         filename=,
                         fileType=c("canon", "dot", "xdot",
                           "dia", "fig", "gd", "gd2", "gif", "hpgl",
                           "imap", "cmapx", "ismap", "mif", "mp", "pcl",
                           "pdf", "pic", "plain", "plain-ext", "png", "ps",
                           "ps2", "svg", "svgz", "vrml", "vtx", "wbmp"))
                         

remove.edge.separators <- function(string)
  gsub('\\|', '&#x7c;', string)

PHI <- (1 + sqrt(5)) / 2

graphviz <- function(subcalls) {
  supercalls <- ls(subcalls)
  ## Check for is.null(supercalls)
  graph <- new('graphNEL', nodes=unlist(Map(remove.edge.separators,
                             supercalls)))
  for (supercall in supercalls)
    for (subsupercall in subcalls[[supercall]])
      try(graph <- addEdge(remove.edge.separators(supercall),
                           remove.edge.separators(subsupercall),
                           graph))
  ag <- agopenSimple(graph, 'roxygenize')
  graphDataDefaults(ag, 'ratio') <- PHI
  graphDataDefaults(ag, 'splines') <- 'true'
  nodeDataDefaults(ag, 'fontname') <- 'monospace'
  edgeDataDefaults(ag, 'arrowtail') <- 'normal'
  toFile(ag,
         layoutType='fdp',
         filename='test.pdf',
         fileType='pdf')
}

graphviz(subcalls)
