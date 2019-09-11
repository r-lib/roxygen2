
block_include_rmd <- function(tag, block, env) {
  rmd <- tag$val
  stopifnot(is.character(rmd), length(rmd) == 1, !is.na(rmd))
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("@includeRmd requires the rmarkdown package")
  }

  rmd_path <- tempfile(fileext = ".Rmd")
  md_path <- tempfile(fileext = ".md")
  on.exit(unlink(c(rmd_path, md_path), recursive = TRUE), add = TRUE)

  cache_path <- paste0(sub("\\.Rmd$", "", rmd), "_cache/")
  fig_path <- file.path(dirname(rmd), "figure/")
  linkrefs <- rmd_linkrefs_from_file(rmd)
  txt <- sprintf(
    "```{r cache.path = \"%s\", fig.path = \"%s\", child = \"%s\"}\n```\n\n%s\n",
    cache_path, fig_path, rmd, linkrefs)
  cat(txt, file = rmd_path)

  rmarkdown::render(
    rmd_path,
    output_format = rmarkdown::github_document(),
    output_file = md_path,
    quiet = TRUE
  )

  rmd_eval_rd(md_path, tag)
}

rmd_linkrefs_from_file <- function(path) {
  lines <- read_lines(path)
  txt <- paste(lines, collapse = "\n")
  get_md_linkrefs(txt)
}

rmd_eval_rd <- function(path, tag) {
  mdtxt <- paste(read_lines(path), collapse = "\n")
  mdesc <- add_linkrefs_to_md(mdtxt)
  mdx <- commonmark::markdown_xml(mdesc, hardbreaks = TRUE)
  mdxml <- xml2::read_xml(mdx)
  state <- new.env(parent = emptyenv())
  state$tag <- tag
  rd <- mdxml_children_to_rd_top(mdxml, state = state)
  rd
}
