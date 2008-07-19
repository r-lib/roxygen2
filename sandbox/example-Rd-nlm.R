#' This function carries out a minimization of the function \code{f}
#' using a Newton-type algorithm.  See the references for details.
#'
#'
#' Note that arguments after \code{\dots} must be matched exactly.
#' 
#' If a gradient or hessian is supplied but evaluates to the wrong mode
#' or length, it will be ignored if \code{check.analyticals = TRUE} (the
#' default) with a warning.  The hessian is not even checked unless the
#' gradient is present and passes the sanity checks.
#'
#' From the three methods available in the original source, we always use
#' method \dQuote{1} which is line search.
#' 
#' The functions supplied must always return finite (including not
#' \code{NA} and not \code{NaN}) values.
#'
#' @name nlm
#' @aliases nlm
#' @title Non-Linear Minimization
#' @concept optimization
#' @usage
#' nlm(f, p, \dots, hessian = FALSE, typsize = rep(1, length(p)),
#'     fscale = 1, print.level = 0, ndigit = 12, gradtol = 1e-6,
#'     stepmax = max(1000 * sqrt(sum((p/typsize)^2)), 1000),
#'     steptol = 1e-6, iterlim = 100, check.analyticals = TRUE)
#'
#' @param f the function to be minimized.  If the function value has
#'   an attribute called \code{gradient} or both \code{gradient} and
#'   \code{hessian} attributes, these will be used in the calculation of
#'   updated parameter values.  Otherwise, numerical derivatives are
#'   used. \code{\link{deriv}} returns a function with suitable
#'   \code{gradient} attribute.  This should be a function of a vector of
#'   the length of \code{p} followed by any other arguments specified
#'   by the \code{\dots} argument.
#' @param p starting parameter values for the minimization.
#' @param \dots additional arguments to \code{f}.
#' @param hessian if \code{TRUE}, the hessian of \code{f}
#'   at the minimum is returned.
#' @param typsize an estimate of the size of each parameter
#'   at the minimum.
#' @param fscale an estimate of the size of \code{f} at the minimum.
#' @param print.level this argument determines the level of printing
#'   which is done during the minimization process.  The default
#'   value of \code{0} means that no printing occurs, a value of \code{1}
#'   means that initial and final details are printed and a value
#'   of 2 means that full tracing information is printed.
#' @param ndigit the number of significant digits in the function \code{f}.
#' @param gradtol a positive scalar giving the tolerance at which the
#'   scaled gradient is considered close enough to zero to
#'   terminate the algorithm.  The scaled gradient is a
#'   measure of the relative change in \code{f} in each direction
#'   \code{p[i]} divided by the relative change in \code{p[i]}.
#' @param stepmax a positive scalar which gives the maximum allowable
#'   scaled step length.  \code{stepmax} is used to prevent steps which
#'   would cause the optimization function to overflow, to prevent the
#'   algorithm from leaving the area of interest in parameter space, or to
#'   detect divergence in the algorithm. \code{stepmax} would be chosen
#'   small enough to prevent the first two of these occurrences, but should
#'   be larger than any anticipated reasonable step.
#' @param steptol A positive scalar providing the minimum allowable
#'   relative step length.
#' @param iterlim a positive integer specifying the maximum number of
#'   iterations to be performed before the program is terminated.
#' @param check.analyticals a logical scalar specifying whether the
#'   analytic gradients and Hessians, if they are supplied, should be
#'   checked against numerical derivatives at the initial parameter
#'   values. This can help detect incorrectly formulated gradients or
#'   Hessians.
#' @returnType list
#' @return A list containing the following components:
#'   \item{minimum}{the value of the estimated minimum of \code{f}.}
#'   \item{estimate}{the point at which the minimum value of
#'     \code{f} is obtained.}
#'   \item{gradient}{the gradient at the estimated minimum of \code{f}.}
#'   \item{hessian}{the hessian at the estimated minimum of \code{f} (if
#'     requested).}
#'   \item{code}{an integer indicating why the optimization process terminated.
#'     \describe{
#'       \item{1:}{relative gradient is close to zero, current iterate is
#'       probably solution.}
#'       \item{2:}{successive iterates within tolerance, current iterate
#'       is probably solution.}
#'       \item{3:}{last global step failed to locate a point lower than
#'       \code{estimate}.  Either \code{estimate} is an approximate local
#'       minimum of the function or \code{steptol} is too small.}
#'       \item{4:}{iteration limit exceeded.}
#'       \item{5:}{maximum step size \code{stepmax} exceeded five consecutive
#'       times.  Either the function is unbounded below,
#'       becomes asymptotic to a finite value from above in
#'       some direction or \code{stepmax} is too small.}
#'     }
#'   }
#'   \item{iterations}{the number of iterations performed.}
#'
#' @references
#' Dennis, J. E. and Schnabel, R. B. (1983) \emph{Numerical Methods for
#'   Unconstrained Optimization and Nonlinear Equations.} Prentice-Hall,
#'   Englewood Cliffs, NJ.
#' 
#' Schnabel, R. B., Koontz, J. E. and Weiss, B. E. (1985) A modular
#'   system of algorithms for unconstrained minimization.
#'   \emph{ACM Trans. Math. Software}, \bold{11}, 419--440.
#'
#' @seealso
#'   \code{\link{optim}} and \code{\link{nlminb}}.
#'    
#'   \code{\link{constrOptim}} for constrained optimization, 
#'   \code{\link{optimize}} for one-dimensional
#'   minimization and \code{\link{uniroot}} for root finding.
#'   \code{\link{deriv}} to calculate analytical derivatives.
#' 
#'   For nonlinear regression, \code{\link{nls}} may be better.
#'
#' @examples
#' f <- function(x) sum((x-1:length(x))^2)
#' nlm(f, c(10,10))
#' nlm(f, c(10,10), print.level = 2)
#' utils::str(nlm(f, c(5), hessian = TRUE))
#' 
#' f <- function(x, a) sum((x-a)^2)
#' nlm(f, c(10,10), a=c(3,5))
#' f <- function(x, a)
#' {
#'     res <- sum((x-a)^2)
#'     attr(res, "gradient") <- 2*(x-a)
#'     res
#' }
#' nlm(f, c(10,10), a=c(3,5))
#' 
#' ## more examples, including the use of derivatives.
#' \dontrun{demo(nlm)}
#'
#' @keywords nonlinear optimize
nlm <- function(f, p, ..., hessian=FALSE, typsize=rep(1,length(p)),
		fscale=1, print.level=0, ndigit=12, gradtol=1e-6,
		stepmax=max(1000 * sqrt(sum((p/typsize)^2)), 1000),
		steptol=1e-6, iterlim=100, check.analyticals=TRUE)
{

    print.level <- as.integer(print.level)
    if(print.level < 0 || print.level > 2)
	stop("'print.level' must be in {0,1,2}")
    ## msg is collection of bits, i.e., sum of 2^k (k = 0,..,4):
    msg <- (1 + c(8,0,16))[1+print.level]
    if(!check.analyticals) msg <- msg + (2 + 4)
    .Internal(nlm(function(x) f(x, ...), p, hessian, typsize, fscale,
                  msg, ndigit, gradtol, stepmax, steptol, iterlim))
}

optimize <- function(f, interval, ...,
		     lower=min(interval), upper=max(interval),
		     maximum=FALSE, tol=.Machine$double.eps^0.25)
{
    if(maximum) {
	val <- .Internal(fmin(function(arg) -f(arg, ...), lower, upper, tol))
	list(maximum = val, objective= f(val, ...))
    } else {
	val <- .Internal(fmin(function(arg) f(arg, ...), lower, upper, tol))
	list(minimum = val, objective= f(val, ...))
    }
}

##nice to the English (or rather the Scots)
optimise <- optimize

uniroot <- function(f, interval, ...,
                    lower = min(interval), upper = max(interval),
                    f.lower = f(lower, ...), f.upper = f(upper, ...),
		    tol = .Machine$double.eps^0.25, maxiter = 1000)
{
    if(!missing(interval) && length(interval) != 2)
        stop("'interval' must be a vector of length 2")
    if(!is.numeric(lower) || !is.numeric(upper) || lower >= upper)
        stop("lower < upper  is not fulfilled")
    if(is.na(f.lower)) stop("f.lower = f(lower) is NA")
    if(is.na(f.upper)) stop("f.upper = f(upper) is NA")
    if(f.lower * f.upper > 0)
	stop("f() values at end points not of opposite sign")
    val <- .Internal(zeroin2(function(arg) f(arg, ...),
                             lower, upper, f.lower, f.upper,
			     tol, as.integer(maxiter)))
    iter <- as.integer(val[2])
    if(iter < 0) {
	warning("_NOT_ converged in ", maxiter, " iterations")
        iter <- maxiter
    }
    list(root = val[1], f.root = f(val[1], ...),
         iter = iter, estim.prec = val[3])
}
