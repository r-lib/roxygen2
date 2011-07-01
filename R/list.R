# Zip \emph{n} lists together into tuplets of
# length \emph{n}.
# @param zipper the zipping function
# @param \dots the lists to be zipped
# @return A list of tuplets
zip <- function(zipper, ...) {
  m <- mapply(zipper, ...)
  split(m, col(m))
}

# Zip using \code{list}.
# @param \dots the lists to be zipped
# @return A list of tuplets
# @seealso \code{\link{zip}}
zip.list <- function(...) {
  zip(list, ...)
}

# Combine a list into pairwise elements; lists should
# be of the same length. In case of odd numbers of members,
# the last will be removed.
# @param list the list to be pairwise decomposed
# @return A list of pairwise elements
pairwise <- function(list) {
  length <- length(list)
  if (length < 2)
    return(list())
  length <- ifelse(length %% 2 == 1,
                   length - 1,
                   length)
  odds <- seq(1, length, 2)
  evens <- seq(2, length, 2)
  zip(c, list[odds], list[evens])
}
