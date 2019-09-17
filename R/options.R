#' Load roxygen2 options
#'
#' Options can be stored in the `Roxygen` field of the `DESCRIPTION`, or
#' in `man/roxygen/meta.R`. In either case, the code is parsed and evaluated
#' in a child of the base environment.
#'
#' Options in `man/roxygen/meta.R` override those present `DESCRIPTION`.
#'
#' @param base_path Path to package.
#' @export
#' @keywords internal
load_options <- function(base_path = ".") {
  desc <- load_options_description(base_path)
  meta <- load_options_meta(base_path)
  opts <- utils::modifyList(desc, meta)

  defaults <- list(
    wrap = FALSE,
    roclets = c("collate", "namespace", "rd"),
    markdown = markdown_global_default
  )

  unknown_opts <- setdiff(names(opts), names(defaults))
  if (length(unknown_opts) > 0) {
    warn(paste0(
      "Unknown Roxygen options ", paste(unknown_opts, collapse = ", "), ".\n",
      "Supported options: ", paste(names(defaults), collapse = ", ")
    ))
  }

  utils::modifyList(defaults, opts)
}

load_options_description <- function(base_path = ".") {
  desc_path <- file.path(base_path, "DESCRIPTION")
  desc_opts <- read.dcf(desc_path, fields = "Roxygen")[[1, 1]]

  if (is.na(desc_opts)) {
    list()
  } else {
    eval(parse(text = desc_opts), child_env(baseenv()))
  }
}

load_options_meta <- function(base_path = ".", path = "man/roxygen/meta.R") {
  # Only look for .R for consistency with style advice
  meta_path <- file.path(base_path, path)

  if (!file.exists(meta_path)) {
    return(list())
  }

  value <- tryCatch(
    source(meta_path, local = child_env(baseenv()))$value,
    error = function(err) {
      warn("Failed to source `man/roxygen/meta.R`")
      list()
    }
  )

  if (!is.list(value)) {
    warn("`man/roxygen/meta.R` must yield a named list")
    return(list())
  }

  value
}
