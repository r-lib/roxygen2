### How to create a external roclet.

require(roxygen)

#' Exemplar class: the JAVA bicycle class.
#' @classGraph
setClass('Bicycle',
         representation=representation(
           cadence='numeric',
           speed='numeric',
           gear='numeric'))

## Register tag with the main parser
register.preref.parser('classGraph', parse.toggle)

make.classGraph.roclet <- function(package, dir='.') {

  ### Prerequirements:
  
  if ( !require(classGraph) )
    stop('This roclet needs the ', sQuote('classGraph'), 'package!')
  
  ### Roclet parser:
  
  class <- NULL
  file <- NULL
  
  pre.parse <- function(partitum) {
    if ( !is.null(partitum$S4class) ) {
      class <<- partitum$S4class
      file <<- partitum$srcref$filename
    }
  }

  ## parse.classGraph only called on @classGraph;
  ## no need to check the key
  parse.classGraph <- function(key, value) {
    if ( !is.null(class) )
      create.callGraph()
    else
      warning('@classGraph set but no S4 class far and wide.')
  }
  
  post.parse <- function(partitum) {
    class <<- NULL
    file <<- NULL
  }

  ## No default parser
  roclet <- roxygen:::make.roclet(pre.parse=pre.parse,
                                  post.parse=post.parse)

  roclet$register.parser('classGraph', parse.classGraph)

  ### Action:

  create.callGraph <- function() {
    print("Hello")
  }

  
  return(roclet)
}


### Usage:

r <- make.classGraph.roclet()
r$parse('classGraph.R')
r$parse('Bicycle.R')
