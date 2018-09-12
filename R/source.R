package_files <- function(path) {
  all <- normalizePath(r_files(path))

  collate <- desc::desc_get_collate(file = file.path(path, "DESCRIPTION"))

  collate <- normalizePath(file.path(path, "R", collate))

  rfiles <- c(collate, setdiff(all, collate))
  ignore_files(rfiles, path)
}
