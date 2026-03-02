roxygen_setup <- function(
  path = ".",
  cur_version = NULL,
  frame = caller_env()
) {
  if (!file.exists(file.path(path, "DESCRIPTION"))) {
    cli::cli_abort(
      "{.arg package.dir} ({.path {path}}) does not contain a DESCRIPTION"
    )
  }

  is_first <- first_time(path)
  if (is_first) {
    cli::cli_inform(
      "First time using {.pkg roxygen2}. Upgrading automatically..."
    )
  }

  update_roxygen_version(path, cur_version = cur_version)

  encoding <- desc::desc_get("Encoding", path)[[1]]
  if (!identical(encoding, "UTF-8")) {
    cli::cli_inform(c(
      x = "{.pkg roxygen2} requires {.val Encoding: UTF-8}",
      i = "Current encoding is {.val {encoding}}"
    ))
  }

  man_path <- file.path(path, "man")
  dir.create(man_path, recursive = TRUE, showWarnings = FALSE)

  withr::local_envvar(
    ROXYGEN_PKG = desc::desc_get("Package", path),
    .local_envir = frame
  )

  is_first
}

peek_roxygen_pkg <- function() {
  pkg <- Sys.getenv("ROXYGEN_PKG")

  if (nzchar(pkg)) {
    pkg
  } else {
    NULL
  }
}

update_roxygen_version <- function(path, cur_version = NULL) {
  cur <- cur_version %||% as.character(utils::packageVersion("roxygen2"))
  prev <- roxygen_version(path)

  if (
    !is.na(cur) && !is.na(prev) && package_version(cur) < package_version(prev)
  ) {
    cli::cli_inform(c(
      x = "Installed {.pkg roxygen2} is older than the version used with this package",
      i = "You have {.val {cur}} but you need {.val {prev}}"
    ))
  } else if (!identical(cur, prev)) {
    if (!is.na(prev) && numeric_version(prev) <= "6.1.99") {
      cli::cli_rule()
      cli::cli_inform(c(
        "Changes in {.pkg roxygen2} 7.0.0:",
        "* `%` is now escaped automatically in Markdown mode.",
        "Please carefully check .Rd files for changes"
      ))
      cli::cli_rule()
    }

    cli::cli_inform(c(i = "Setting {.field RoxygenNote} to {.val {cur}}"))
    desc::desc_set(RoxygenNote = cur, file = path)
  }
}

first_time <- function(path) {
  if (!is.na(roxygen_version(path))) {
    return(FALSE)
  }

  generated <- dir(file.path(path, "man"), full.names = TRUE)
  generated <- generated[!file.info(generated)$isdir]

  namespace <- file.path(path, "NAMESPACE")
  if (file.exists(namespace)) {
    generated <- c(generated, namespace)
  }

  roxy <- map_lgl(generated, made_by_roxygen)
  all(!roxy)
}

roxygen_version <- function(path = ".") {
  stringr::str_trim(desc::desc_get("RoxygenNote", path)[[1]])
}
