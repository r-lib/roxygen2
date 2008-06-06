# Function documentation using Roxygen.



#' A monte carlo algorithm to calculate PI.
#'
#' @param trials The number of trials to throw
#' @param verbose Show some information during the tossing
#'
#' @return The approximation of PI
#'
#' @reference http://www.datastructures.info/the-monte-carlo-algorithmmethod/
#' @author Manuel J. A. Eugster
mcpi <- function(trials, verbose=FALSE) {
  hits <- 0                             #' Number of successfull trials

  for ( i in 1:trials ) {
    if ( verbose )
      cat('.')
    
    xy <- runif(2, min=, max=1)         #' Simulate the throwing, sample (x,y)

    if ( xy[1]*xy[1] + xy[2]*xy[2] <= 1 )
      hits <- hits + 1
  }

  pi <- 4 * hits / trials

  if ( verbose )
    cat('done\n')
  
  return(pi)
}

