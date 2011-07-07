library(digest)

new_cache <- function() {
  
  cache <- NULL
  cache_reset <- function() {
    cache <<- new.env(TRUE, emptyenv())
  }
  
  cache_set <- function(key, value) {
    assign(key, value, env = cache)
  }
  
  cache_get <- function(key) {
    get(key, env = cache, inherits = FALSE)
  }
  
  cache_has_key <- function(key) {
    exists(key, env = cache, inherits = FALSE)
  }
  
  cache_reset()
  list(
    reset = cache_reset, 
    set = cache_set, 
    get = cache_get,
    has_key = cache_has_key,
    keys = function() ls(cache)
  )
}

memoize <- function(f) {
  cache <- new_cache()
  
  function(...) {
    hash <- digest(list(...))
    
    if (cache$has_key(hash)) {
      cache$get(hash)
    } else {
      res <- f(...)
      cache$set(hash, res)
      res
    }
  }
}