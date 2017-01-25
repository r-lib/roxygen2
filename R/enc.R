read_lines_enc <- function(path, file_encoding = "UTF-8", n = -1L, ok = TRUE, skipNul = FALSE) {
  con <- file(path, encoding = file_encoding)
  on.exit(close(con), add = TRUE)

  lines <- readLines(con, warn = FALSE, n = n, ok = ok, skipNul = skipNul)
  Encoding(lines) <- "UTF-8"
  lines
}
