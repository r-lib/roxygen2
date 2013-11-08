# S3 documentation using Roxygen.

#' Container for the number of throws and pi estimation.
#'
#' @S3class mcpi
#' @slot pi Estimation of pi
#' @slot throws Number of throws
#' @listObject
roxygen

#' A monte carlo algorithm to calculate PI. Extended
#' version which returns the throws.
#'
#' @param trials The number of trials to throw
#' @returnType mcpi
#' @return a list containing the approximated
#'         PI value and the throws
#' @export
mcpiX <- function(trials) {

  throws <- matrix(NA, nrow=trials,
                   ncol=3,
                   dimnames=list(NULL, c('x','y','hit')))
  hits <- 0

  for ( i in 1:trials ) {
    xy <- runif(2, min=, max=1)

    hit <- (xy[1]*xy[1] + xy[2]*xy[2] <= 1)
    hits <- hits + hit
    throws[i,] <- c(xy, hit)
  }

  pi <- 4 * hits / trials

  res <- list(pi=pi,
              throws=throws)
  class(res) <- 'mcpi'

  return(res)
}

#' Print function for 'mcpi' objects.
#'
#' @param x The 'mcpi' object
#' @param ... Ignored
#' @return NULL
#' @export
print.mcpi <- function(x, ...) {
  cat('Approximated PI value (using', nrow(x$throws), 'throws) =', x$pi, '\n')
}

#' Plot function for 'mcpi' objects.
#'
#' @param x The 'mcpi' object
#' @return NULL
#' @importFrom pgirmess polycircle
#' @export
plot.mcpi <- function(x, ...) {
  plot(x$throws[,1:2], col=x$throws[,3]+1,
       xlim=c(0,1), ylim=c(0,1), pch=19,
       main=paste('pi =', x$pi), ...)

  # require(pgirmess)
  lines(polycirc(1, pts=c(0, 0), nbr=100), lwd=2)
}

#' Return base data of a monte carlo algorithm.
#'
#' @param x A object created with a monte carlo algorithm
mcbase <- function(x, ...) {
  UseMethod('mcbase')
}

#' Base of PI approximation with motne carlo algorithm.
#'
#' @param x The 'mcpi' object
#' @return The throws
#' @export
mcbase.mcpi <- function(x, ...) {
  return(x$throws)
}


