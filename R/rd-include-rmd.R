
block_include_rmd <- function(tag, block, env) {
  rmd <- tag$val
  stopifnot(is.character(rmd), length(rmd) == 1, !is.na(rmd))
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("@includeRmd requires the rmarkdown package")
  }

  md_path <- tempfile(fileext = ".md")
  on.exit(unlink(md_path, recursive = TRUE), add = TRUE)
  rmd_path <- rmd_process_links(rmd)
  on.exit(unlink(rmd_path, recursive = TRUE), add = TRUE)

  rmarkdown::render(
    rmd_path,
    output_format = rmarkdown::github_document(),
    output_file = md_path,
    quiet = TRUE
  )
  rmd_eval_rd(md_path, tag)
}

rmd_process_links <- function(path) {
  tmp <- tempfile(fileext = ".Rmd")
  lines <- read_lines(path)
  txt <- paste(lines, collapse = "\n")
  esc <- add_linkrefs_to_md(txt)
  lines2 <- strsplit(esc, "\n", fixed = TRUE)[[1]]
  write_lines(lines2, tmp)
  tmp
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
