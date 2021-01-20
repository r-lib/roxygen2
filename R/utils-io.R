readLines <- function(...) stop("Use read_lines!")
writeLines <- function(...) stop("Use write_lines!")

read_lines <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_lines <- function(text, path, line_ending = detect_line_ending(path)) {
  # we need to convert any embedded newlines as well
  text <- gsub("\r?\n", line_ending, text)

  base::writeLines(enc2utf8(text), path, sep = line_ending, useBytes = TRUE)
}

detect_line_ending <- function(path) {
  tryCatch({
    samp <- suppressWarnings(readChar(path, nchars = 500))
    if (isTRUE(grepl("\r\n", samp))) "\r\n" else "\n"
  }, error = function(e) "\n")
}
