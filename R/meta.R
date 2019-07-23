
.roxygen_meta <- NULL

roxy_meta_read <- function(name) {
  .roxygen_meta[[name]]
}

roxy_meta_load <- function(base_path) {

  meta_dir <- file.path(base_path, "man/roxygen")
  if (!utils::file_test("-d", meta_dir))
    return(FALSE)

  pattern <- "^meta[.][Rr]"
  meta_files <- list.files(meta_dir, pattern = pattern, full.names = TRUE)
  if (length(meta_files) == 0)
    return(FALSE)

  if (length(meta_files) > 1) {
    rlang::abort("multiple 'man/roxygen/meta.R' files found")
  }

  parsed <- tryCatch(parse(meta_files), error = identity)
  if (inherits(parsed, "error")) {
    message <- "could not parse 'man/roxygen/meta.R'"
    rlang::abort(message, parent = parsed)
  }

  # TODO: appropriate evaluation environment?
  envir <- globalenv()
  result <- tryCatch(eval(parsed, envir = envir), error = identity)
  if (inherits(result, "error")) {
    message <- "could not evaluate 'man/roxygen/meta.R'"
    rlang::abort(message, parent = result)
  }

  if (!is.list(result)) {
    message <- "evaluation of 'man/roxygen/meta.R' did not return a list"
    rlang::abort(message)
  }

  .roxygen_meta <<- result
  TRUE

}
