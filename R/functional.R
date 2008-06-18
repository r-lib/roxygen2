## Thanks, Byron Ellis.
## https://stat.ethz.ch/pipermail/r-devel/2007-November/047318.html
Curry <- function(FUN,...) {
  .orig = list(...);
  function(...) do.call(FUN,c(.orig,list(...)))
}

## Borrowed from src/library/base/R/funprog.R for pre-2.7 Rs.
Negate <- function(f)
  function(...) ! match.fun(f)(...)
