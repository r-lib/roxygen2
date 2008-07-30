#' @include roxygen.R
#' @include roclet.R
#' @include functional.R
roxygen()

#' Make a callgraph roclet.
#'
#' The callgraph roclet is awkward in the sense that
#' it requires a function's package to be loadable;
#' which means, like calling LaTeX multiple times,
#' one has to run roxygen on a package, install it,
#' run roxygen again to get the callgraphs, and possibly
#' install the package again.
#'
#' @param dependencies packages required to evaluate
#' interesting functions
#' @param dir the directory to place the callgraphs in
#' @param verbose anounce what we're doing
#' @importFrom Rgraphviz toFile
#' @export
make.callgraph.roclet <- function(dependencies=NULL,
                                  dir='.',
                                  verbose=TRUE) {
  DEFAULT.DEPTH <- 2

  do.callgraph <- FALSE
  do.callgraph.primitives <- FALSE
  depth <- DEFAULT.DEPTH
  name <- NULL

  load.dependencies <- function() {
    successes <-
      mapply(function(package)
             tryCatch(require(package,
                              character.only=TRUE,
                              quietly=TRUE),
                      warning=function(e) FALSE),
             dependencies)

    if (!all(successes))
      warning(sprintf(paste('Package(s) %s wouldn\'t load;',
                            'callgraphs might be incomplete.'),
                      do.call(Curry(paste, sep=', '),
                              Map(Curry(sprintf, fmt='`%s\''),
                                  dependencies[!successes]))))
  }
  
  parse.default <- function(key, expression) NULL

  reset.state <- function(partitum) {
    do.callgraph <<- FALSE
    do.callgraph.primitives <<- FALSE
    depth <<- DEFAULT.DEPTH
    call.stack <<- make.stack()
    subcalls <<- new.env(parent=emptyenv())
    calls <<- NULL
    name <<- guess.name(partitum)
  }

  post.parse <- function(partitum) {
    if (do.callgraph || do.callgraph.primitives) {
      if (is.null(name))
        stop('Callgraph needs a name')
      else {
        preorder.walk.expression(discover.subcalls,
            parse(text=src.lines(partitum)))
        graphviz(subcalls)
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

  is.callable <- function(name, include.primitives) {
    f <- tryCatch(get(name, mode='function'), error=function(e) NULL)
    !is.null(f) && ifelse(include.primitives, TRUE, !is.primitive(f))
  }

  discover.subcalls <- function(exprofundum)
    if (is.name(exprofundum)) {
      subcall <- as.character(exprofundum)
      if (is.callable(subcall, do.callgraph.primitives) &&
          call.stack$top <= depth) {
        supercall <-
          if (call.stack$is.empty())
            name
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

  remove.edge.separators <- function(string)
    gsub('\\|', '&#x7c;', string)

  PHI <- (1 + sqrt(5)) / 2

  formals(toFile) <- alist(graph=,
                           layoutType=c("dot", "neato", "twopi",
                             "circo", "fdp"),
                           filename=,
                           fileType=c("canon", "dot", "xdot",
                             "dia", "fig", "gd", "gd2", "gif", "hpgl",
                             "imap", "cmapx", "ismap", "mif", "mp", "pcl",
                             "pdf", "pic", "plain", "plain-ext", "png", "ps",
                             "ps2", "svg", "svgz", "vrml", "vtx", "wbmp"))

  OUTFILE <- '%s-callgraph.pdf'

  graphviz <- function(subcalls) {
    supercalls <- ls(subcalls)
    if (length(supercalls) < 1 || is.null(supercalls))
      warning(sprintf('Omitting call-less call-graph for `%s\'.',
                      name))
    else {
      graph <- new('graphNEL',
                   nodes=unlist(Map(remove.edge.separators, supercalls)),
                   edgemode='directed')
      for (supercall in supercalls)
        for (subsupercall in subcalls[[supercall]])
          tryCatch(graph <- addEdge(remove.edge.separators(supercall),
                                    remove.edge.separators(subsupercall),
                                    graph),
                   error=function(e)
                   warning(sprintf('Unknown node %s', subsupercall)))
      ag <- agopenSimple(graph, 'roxygenize')
      graphDataDefaults(ag, 'ratio') <- PHI
      graphDataDefaults(ag, 'splines') <- 'true'
      nodeDataDefaults(ag, 'fontname') <- 'monospace'
      outfile <- file.path(dir, sprintf(OUTFILE, name))
      if (verbose)
        cat(sprintf('Outputting call graph to %s\n', outfile))
      toFile(ag,
             layoutType='fdp',
             filename=outfile,
             fileType='pdf')
    }
  }

  roclet <- make.roclet(parse.default,
                        pre.parse=reset.state,
                        post.parse=post.parse)

  parse.callgraph <- function(key, expression)
    do.callgraph <<- TRUE

  parse.callgraph.primitives <- function(key, expression)
    do.callgraph.primitives <<- TRUE

  parse.callgraph.depth <- function(key, expression) {
    depth <- tryCatch(as.numeric(expression),
                      warning=function(e) NULL,
                      error=function(e) NULL)
    if (is.null(depth))
      warning('@callGraphDepth should be a number; resorting to default.')
    else
      assign.parent('depth', depth, environment())
  }

  roclet$register.parser('callGraph',
                         parse.callgraph)
  roclet$register.parser('callGraphPrimitives',
                         parse.callgraph.primitives)
  roclet$register.parser('callGraphDepth',
                         parse.callgraph.depth)

  load.dependencies()

  roclet
}
