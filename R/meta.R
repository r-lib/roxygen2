
.roxygen_meta <- new.env(parent = emptyenv())

roxy_meta_get <- function(key = NULL, default = NULL) {

  if (is.null(key))
    return(as.list(.roxygen_meta))

  if (!is.character(key) || length(key) != 1)
    rlang::abort("name must be NULL or a length one character vector")

  .roxygen_meta[[key]] %||% default

}

roxy_meta_set <- function(key, value = NULL) {
  if (is.list(key) && is.null(value))
    list2env(key, envir = .roxygen_meta)
  else if (is.character(key))
    assign(key, value, envir = .roxygen_meta)
  else
    rlang::abort("unexpected key / value pair in roxy_meta_set")
}

roxy_meta_clear <- function() {
  rm(
    list = ls(.roxygen_meta, all.names = TRUE),
    envir = .roxygen_meta
  )
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

  parsed <- tryCatch(
    parse(meta_files),
    error = function(cnd) {
      message <- "Parse of 'man/roxygen/meta.R' failed"
      rlang::abort(message, parent = cnd)
    }
  )

  # TODO: appropriate evaluation environment?
  result <- tryCatch(
    eval(parsed, envir = globalenv()),
    error = function(cnd) {
      message <- "Evaluation of 'man/roxygen/meta.R' failed"
      rlang::abort(message, parent = result)
    }
  )

  if (!is.list(result)) {
    message <- "Evaluation of 'man/roxygen/meta.R' did not return a list"
    rlang::abort(message)
  }

  roxy_meta_clear()
  list2env(result, envir = .roxygen_meta)
  TRUE

}
