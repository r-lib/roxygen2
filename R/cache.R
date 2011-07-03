new_cache <- function() {
  cache <- new.env(parent = emptyenv())
  
  compute <- function(keys, code) {
    hash <- suppressWarnings(digest(keys))
    if (exists(hash, cache, inherits = FALSE)) {
      return(cache[[hash]])
    }
    
    (cache[[hash]] <- force(code))
  }
  
  
  list(compute = compute)
}