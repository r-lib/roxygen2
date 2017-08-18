readLines <- function(...) stop("Use read_lines!")
writeLines <- function(...) stop("Use write_lines!")

read_lines <- function(path, n = -1L) {
  con <- file(path, open = "r", encoding = "utf-8")
  on.exit(close(con))

  base::readLines(con, n = n, warn = FALSE)
}

write_lines <- function(text, path) {
  con <- file(path, open = "w", encoding = "utf-8")
  on.exit(close(con))

  base::writeLines(text, con, useBytes = TRUE)
}
