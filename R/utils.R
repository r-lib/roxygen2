"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

# Does the string contain no matter, but very well [:space:]?
# @param string the string to check
# @return TRUE if the string contains words, otherwise FALSE
is.null.string <- function(string) {
  str_length(str_trim(string)) == 0
}

subs <- matrix(ncol = 2, byrow = T, c(
  '[]', 'sub',
  '<-', 'set',
  '!', 'not',
  '"', 'quote',
  '#', 'hash',
  '$', 'cash',
  '%', 'grapes',
  '&', 'and',
  '|', 'or',
  "'", 'single-quote',
  '(', 'open-paren',
  ')', 'close-paren',
  '*', 'star',
  '+', 'plus',
  ',', '-',
  '/', 'slash',
  ':', 'colon',
  ';', 'semi-colon',
  '<', 'less-than',
  '=', 'equals',
  '>', 'greater-than',
  '?', 'p',
  '@', 'at',
  '[', 'open-brace',
  '\\', 'backslash',
  ']', 'close-brace',
  '^', 'hat',
  '`', 'tick',
  '{', 'open-curly',
  '}', 'close',
  '~', 'twiddle'
))
subs[, 2] <- str_c("-", subs[, 2])

nice_name <- function(x) {
  for(i in seq_len(nrow(subs))) {
    x <- str_replace_all(x, fixed(subs[i, 1]), subs[i, 2])
  }
  x <- str_replace(x, "-+", "-")
  x
}


roxygen_stop <- function(..., srcref = NULL) {
  stop(..., srcref_location(srcref), call. = FALSE)
}

roxygen_warning <- function(..., srcref = NULL) {
  warning(..., srcref_location(srcref), call. = FALSE)
}

srcref_location <- function(srcref = NULL) {
  if (is.null(srcref)) return()
  str_c(" in block ", basename(srcref$filename), ":", srcref$lloc[1])
}

write_if_different <- function(path, contents) {
  if (!file.exists(dirname(path))) {
    dir.create(dirname(path), showWarnings = FALSE)
  }
  
  if (same_contents(path, contents)) return(FALSE)
  
  name <- basename(path)
  if (!str_detect(name, "^[a-zA-Z][a-zA-Z0-9_.-]*$")) {
    cat("Skipping invalid path: ", name, "\n")
    FALSE
  } else {
    cat(sprintf('Writing %s\n', name))
    writeLines(contents, path)
    TRUE
  }
}

same_contents <- function(path, contents) {
  if (!file.exists(path)) return(FALSE)
  
  contents <- str_c(str_c(contents, collapse = "\n"), "\n")
  
  text_hash <- digest(contents, serialize = FALSE)
  file_hash <- digest(file = path)
  
  identical(text_hash, file_hash)
}

r_files <- function(path) {
  dir(file.path(path, "R"), "[.Rr]$", full.names = TRUE)
}
