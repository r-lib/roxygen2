
.roxygen_meta <- new.env(parent = emptyenv())

roxy_meta_read <- function(name) {
  .roxygen_meta[[name]]
}

roxy_meta_load <- function(base_path) {

  meta_dir <- file.path(base_path, "man-roxygen")

  pattern <- "^roxygen-meta[.][Rr]"
  meta_files <- list.files(meta_dir, pattern = pattern, full.names = TRUE)
  if (length(meta_files) == 0)
    return(FALSE)

  if (length(meta_files) > 1) {
    warning("multiple roxygen-meta.R files found: using '",
            basename(meta_files[[1]]),
            "'")
    meta_files <- meta_files[[1]]
  }

  result <- tryCatch(eval(parse(meta_files)), error = identity)
  if (inherits(result, "error")) {
    warning(result)
    return(FALSE)
  }

  if (!is.list(result)) {
    warning("evaluation of roxygen-meta.R did not return a list")
    return(FALSE)
  }

  rm(list = ls(envir = .roxygen_meta), envir = .roxygen_meta)
  list2env(result, envir = .roxygen_meta)
  TRUE

}
