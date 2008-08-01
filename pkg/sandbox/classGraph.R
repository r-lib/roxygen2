### How to create a external roclet.

require(roxygen)



#' Exemplar class: the JAVA bicycle class.
#' @classGraph
setClass('Bicycle',
         representation=representation(
           cadence='numeric',
           speed='numeric',
           gear='numeric'))



#source('../R/parse.R')
### Roclet:
register.preref.parser('classGraph', parse.toggle)

make.classGraph.roclet <- function(package, dir='.') {

  ### Prerequirements:
  
  if ( !require(classGraph) )
    stop('This roclet needs the ', sQuote('classGraph'), 'package!')

  
  ### Roclet parser:
  
  class <- NULL
  file <- NULL
  
  pre.parse <- function(partitum) {
    if ( partitum$S4class != '' ) {
      class <<- partitum$S4class
      file <<- partitum$srcref$filename
    }
  }

  parse.classGraph <- function(key, value) {
    if ( key == 'classGraph' )
      if ( !is.null(class) )
        create.callGraph()
      else
        warning('@classGraph set but no S4 class far and wide.')
  }
  
  post.parse <- function(partitum) {
    class <<- NULL
    file <<- NULL
  }

  roclet <- roxygen:::make.roclet(parse.classGraph,
                                  pre.parse,
                                  post.parse)

  roclet$register.default.parsers('classGraph')


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
