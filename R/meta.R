
.roxygen_meta <- new.env(parent = emptyenv())

roxy_meta_get <- function(name, default = NULL) {
  if (exists(name, envir = .roxygen_meta))
    get(name, envir = .roxygen_meta)
  else
    NULL
}

roxy_meta_set <- function(key, value) {
  assign(key, value, envir = .roxygen_meta)
}

roxy_meta_load <- function(base_path = getwd()) {

  meta_dir <- file.path(base_path, "man/roxygen")
  if (!utils::file_test("-d", meta_dir))
    return(FALSE)

  pattern <- "^meta[.][Rr]"
  meta_files <- list.files(meta_dir, pattern = pattern, full.names = TRUE)
  if (length(meta_files) == 0)
    return(FALSE)

  if (length(meta_files) > 1) {
    rlang::abort("Multiple 'man/roxygen/meta.R' files found")
  }

  parsed <- tryCatch(parse(meta_files), error = identity)
  if (inherits(parsed, "error")) {
    message <- "Could not parse 'man/roxygen/meta.R'"
    rlang::abort(message, parent = parsed)
  }

  # TODO: appropriate evaluation environment?
  envir <- globalenv()
  result <- tryCatch(eval(parsed, envir = envir), error = identity)
  if (inherits(result, "error")) {
    message <- "Could not evaluate 'man/roxygen/meta.R'"
    rlang::abort(message, parent = result)
  }

  if (!is.list(result)) {
    message <- "Evaluation of 'man/roxygen/meta.R' did not return a list"
    rlang::abort(message)
  }

  list2env(result, envir = .roxygen_meta)
  TRUE

}
