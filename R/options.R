#' Load roxygen2 options
#'
#' @description
#' Options can be stored in `DESCRIPTION` using `Config/roxygen2/` fields,
#' or in `man/roxygen/meta.R`. Call `roxy_meta_get()` to access current
#' option values from within tag and roclet methods.
#'
#' Options in `man/roxygen/meta.R` override those present in `DESCRIPTION`.
#'
#' @section Possible options:
#'
#' * `roclets` `<character>`: giving names of [roclets][roclet] to run. See
#'    [roclet_find()] for details.
#'
#' * `packages` `<character>`: packages to load that implement new tags.
#'
#' * `load` `<string>`: how to load R code. See [load] for details.
#'
#' * `old_usage` `<flag>`: use old style usage formatting?
#'
#' * `markdown` `<flag>`: translate markdown syntax to Rd?
#'
#' * `r6` `<flag>`: document R6 classes?
#'
#' * `current_package` `<string>` (read only): name of package being documented.
#'
#' * `rd_family_title` `<list>`: overrides for `@family` titles. See the
#'    _rd_ vignette for details: `vignette("rd", package = "roxygen2")`
#'
#' * `knitr_chunk_options` `<list>`: default chunk options used for knitr.
#'
#' * `restrict_image_formats` `<flag>`: if `TRUE` then PDF images are only
#'   included in the PDF manual, and SVG images are only included in the HTML
#'   manual. (This only applies to images supplied via markdown.)
#'
#' @section How to set:
#' Either set in `DESCRIPTION` using `Config/roxygen2/` fields:
#'
#' ```
#' Config/roxygen2/markdown: TRUE
#' Config/roxygen2/load: installed
#' ```
#'
#' Or if you need more complex options (like `rd_family_title` or
#' `knitr_chunk_options`), put them in `man/roxygen/meta.R`:
#'
#' ```
#' list(
#'   rd_family_title = list(models = "Model functions"),
#'   knitr_chunk_options = list(fig.width = 7)
#' )
#' ```
#'
#' @param base_path Path to package.
#' @export
#' @family extending
load_options <- function(base_path = ".") {
  old <- load_options_roxygen(base_path)
  config <- load_options_config(base_path)
  desc <- utils::modifyList(old, config)
  meta <- load_options_meta(base_path)
  opts <- utils::modifyList(desc, meta)

  defaults <- list(
    roclets = c("collate", "namespace", "rd"),
    packages = character(),
    load = "pkgload",
    old_usage = FALSE,
    markdown = FALSE,
    r6 = TRUE,
    current_package = NA_character_,
    current_package_dir = NA_character_,
    rd_family_title = list(),
    knitr_chunk_options = NULL,
    restrict_image_formats = TRUE,
    lazy_data = FALSE
  )

  unknown_opts <- setdiff(names(opts), names(defaults))
  if (length(unknown_opts) > 0) {
    warn(paste0(
      "Unknown Roxygen options ",
      paste(unknown_opts, collapse = ", "),
      ".\n",
      "Supported options: ",
      paste(names(defaults), collapse = ", ")
    ))
  }

  utils::modifyList(defaults, opts)
}

load_options_roxygen <- function(base_path = ".") {
  desc_path <- file.path(base_path, "DESCRIPTION")
  dcf <- read.dcf(desc_path, fields = c("Roxygen", "Package", "LazyData"))
  desc_opts <- dcf[[1, 1]]

  if (is.na(desc_opts)) {
    opts <- list()
  } else {
    opts <- eval(parse(text = desc_opts), child_env(baseenv()))
  }

  opts$current_package <- dcf[[1, 2]]
  opts$current_package_dir <- normalizePath(base_path)
  opts$lazy_data <- identical(dcf[[1, 3]], "true")
  opts
}

config_fields <- c(
  "markdown",
  "load",
  "old_usage",
  "r6",
  "restrict_image_formats",
  "packages",
  "roclets"
)

load_options_config <- function(base_path = ".") {
  fields <- paste0("Config/roxygen2/", config_fields)
  values <- desc::desc_get(fields, file = base_path)
  names(values) <- config_fields

  values <- values[!is.na(values)]
  lapply(values, parse_config_value)
}

parse_config_value <- function(x) {
  values <- scan(
    text = x,
    what = "character",
    sep = ",",
    strip.white = TRUE,
    quiet = TRUE
  )
  utils::type.convert(values, as.is = TRUE)
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

# Global binding management -----------------------------------------------

roxy_meta <- new_environment()

#' @export
#' @rdname load_options
#' @param key Key of the options, e.g. `"packages"`.
#' @param default Default value.
roxy_meta_get <- function(key = NULL, default = NULL) {
  env_get(roxy_meta, key, default = default)
}

roxy_meta_set <- function(key, value = NULL) {
  env_poke(roxy_meta, key, value)
}

roxy_meta_clear <- function() {
  env_unbind(roxy_meta, env_names(roxy_meta))
}

roxy_meta_load <- function(base_path = getwd()) {
  roxy_meta_clear()
  env_bind(roxy_meta, !!!load_options(base_path))
}

local_roxy_meta_set <- function(key, value, envir = caller_env()) {
  old_value <- roxy_meta_set(key, value)
  withr::defer(roxy_meta_set(key, old_value), envir = envir)
}
