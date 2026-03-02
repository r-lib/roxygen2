package_files <- function(path = ".") {
  all <- normalizePath(r_files(path))

  collate <- desc::desc_get_collate(file = file.path(path, "DESCRIPTION"))

  collate <- normalizePath(file.path(path, "R", collate))

  rfiles <- c(collate, setdiff(all, collate))
  ignore_files(rfiles, path)
}

r_files <- function(path) {
  sort_c(dir(file.path(path, "R"), "\\.[Rr]$", full.names = TRUE))
}

ignore_files <- function(rfiles, path) {
  rbuildignore <- file.path(path, ".Rbuildignore")
  if (!file.exists(rbuildignore)) {
    return(rfiles)
  }

  # Strip leading directory and slashes
  rfiles_relative <- sub(
    normalizePath(path, winslash = "/"),
    "",
    normalizePath(rfiles, winslash = "/"),
    fixed = TRUE
  )
  rfiles_relative <- sub("^[/]*", "", rfiles_relative)

  # Remove any files that match any perl-compatible regexp
  patterns <- read_lines(rbuildignore)
  patterns <- patterns[patterns != ""]
  if (length(patterns) == 0L) {
    return(rfiles)
  }
  matches <- lapply(patterns, grepl, rfiles_relative, perl = TRUE)
  matches <- Reduce("|", matches)
  rfiles[!matches]
}
