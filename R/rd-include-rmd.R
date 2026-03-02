#' @export
roxy_tag_parse.roxy_tag_includeRmd <- function(x) {
  if (!is_installed("rmarkdown")) {
    warn_roxy_tag(x, "requires the rmarkdown package")
    return()
  }

  tag_two_part(x, "a path", "a section", required = FALSE, markdown = FALSE)
}

#' @export
roxy_tag_rd.roxy_tag_includeRmd <- function(x, base_path, env) {
  rmd <- rel_rmd <- x$val$name
  section <- x$val$description

  if (!file.exists(rmd)) {
    warn_roxy_tag(x, "Can't find Rmd {.path {rmd}}")
    return(NULL)
  }

  if (section == "") {
    section <- "details"
  }
  stopifnot(is.character(rmd), length(rmd) == 1, !is.na(rmd))

  rmd_path <- tempfile(fileext = ".Rmd")
  md_path <- tempfile(fileext = ".md")
  on.exit(unlink(c(rmd_path, md_path), recursive = TRUE), add = TRUE)

  wd <- getwd()
  setwd(base_path)
  on.exit(setwd(wd), add = TRUE)

  # This will create an absolute path
  rmd <- normalizePath(rmd)

  cache_path <- paste0(sub("\\.Rmd$", "", rmd), "_cache/")
  fig_path <- file.path(dirname(rmd), "figure/")
  linkrefs <- rmd_linkrefs_from_file(rmd)
  opts <- c(
    root.dir = dirname(rmd),
    cache.path = cache_path,
    fig.path = fig_path,
    child = rmd
  )
  optss <- paste0(names(opts), "=", encodeString(opts, quote = '"'))
  txt <- sprintf(
    "```{r %s}\n```\n\n%s\n",
    paste(optss, collapse = ","),
    linkrefs
  )
  cat(txt, file = rmd_path)

  local_reproducible_output()
  tryCatch(
    rmarkdown::render(
      rmd_path,
      output_format = "github_document",
      output_options = c(
        list(html_preview = FALSE),
        if (utils::packageVersion("rmarkdown") >= "2.12") {
          list(math_method = NULL)
        }
      ),
      output_file = md_path,
      quiet = TRUE,
      envir = new_environment(parent = global_env())
    ),
    error = function(e) {
      warn_roxy_tag(x, "failed to evaluate {.path {rel_rmd}}", parent = e)
    }
  )

  if (!file.exists(md_path)) {
    return(NULL)
  }

  tryCatch(
    value <- rmd_eval_rd(md_path, x),
    error = function(e) {
      warn_roxy_tag(
        x,
        "failed to process result of {.path {rel_rmd}}",
        parent = e
      )
    }
  )

  rd_section_markdown(section, value)
}

# Helpers -----------------------------------------------------------------

rmd_linkrefs_from_file <- function(path) {
  lines <- read_lines(path)
  txt <- paste(lines, collapse = "\n")
  paste(get_md_linkrefs(txt), collapse = "\n")
}

rmd_eval_rd <- function(path, tag) {
  mdtxt <- paste(read_lines(path), collapse = "\n")
  mdesc <- add_linkrefs_to_md(mdtxt)
  mdxml <- md_to_mdxml(mdesc)
  state <- new.env(parent = emptyenv())
  state$tag <- tag
  state$has_sections <- TRUE
  rd <- mdxml_children_to_rd_top(mdxml, state = state)
  rd
}
