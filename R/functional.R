# Pre-specify a procedures named parameters, returning a new procedure.
#
# Thanks, Byron Ellis.
# \url{https://stat.ethz.ch/pipermail/r-devel/2007-November/047318.html}
# @param FUN the function to be curried
# @param \dots the determining parameters
# @return A new function partially determined
# @export
Curry <- function(FUN,...) {
  .orig = list(...);
  function(...) do.call(FUN,c(.orig,list(...)))
}
