readLines <- function(...) stop("Use read_lines!")
writeLines <- function(...) stop("Use write_lines!")

read_lines <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_lines <- function(text, path) {
  if (has_windows_le(path)) {
    line_ending <- "\r\n"
  } else {
    line_ending <- "\n"
  }

  path <- file(path, open = "wb")
  base::writeLines(enc2utf8(text), path, sep = line_ending, useBytes = TRUE)
  close(path)
}

has_windows_le <- function(path) {
  tryCatch(
    expr = isTRUE(grepl("\r\n", suppressWarnings(readChar(path, nchars = 500)))),
    error = function(e) FALSE
  )
}
