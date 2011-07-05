new_cache <- function() {
  cache <- new.env(parent = emptyenv())
  
  compute <- function(keys, code) {
    hash <- suppressWarnings(digest(keys))
    if (exists(hash, cache, inherits = FALSE)) {
      return(cache[[hash]])
    }
    
    (cache[[hash]] <- force(code))
  }
  
  reset <- function() {
    cache <<- new.env(parent = emptyenv())
  }
  
  
  list(compute = compute, reset = reset)
}

parse_cache <- new_cache()
rd_proc_cache <- new_cache()
rd_out_cache <- new_cache()

#' Clear all roxygen caches.
#'
#' In order to speed up execution time, roxygen caches a number of 
#' interim results. This function empties all caches and guarantees that all
#' results are computed afresh.
#' 
#' @export
clear_caches <- function() {
  parse_cache$reset()
  rd_proc_cache$reset()
  rd_out_cache$reset()
}