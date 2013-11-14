#' @importFrom digest digest
new_cache <- function() {
  cache <- new.env(parent = emptyenv())
  
  hash_if_needed <- function(keys) {
    if (length(keys) == 1) return(keys)
    suppressWarnings(digest(keys))
  }
  
  has_key <- function(keys) {
    hash <- hash_if_needed(keys)
    exists(hash, cache, inherits = FALSE)
  }
  
  get_key <- function(keys) {
    hash <- hash_if_needed(keys)
    cache[[hash]]    
  }
  
  set_key <- function(keys, value) {
    hash <- hash_if_needed(keys)
    cache[[hash]] <- value
    value
  }
  
  compute <- function(keys, code) {
    hash <- hash_if_needed(keys)
    if (has_key(hash)) return(get_key(hash))
    
    set_key(keys, code)
  }
  
  reset <- function() {
    cache <<- new.env(parent = emptyenv())
  }
  
  list(compute = compute, reset = reset, has_key = has_key, set_key = set_key,
    get_key = get_key)
}

parse_cache <- new_cache()

#' Clear all roxygen caches.
#'
#' In order to speed up execution time, roxygen caches a number of 
#' interim results. This function empties all caches and guarantees that all
#' results are computed afresh.
#' 
#' @export
clear_caches <- function() {
  parse_cache$reset()
}
