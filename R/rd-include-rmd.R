
block_include_rmd <- function(tag, block, env) {
  stopifnot(is.character(tag), length(tag) == 1, !is.na(tag))
  md_path <- tempfile(fileext = ".md")
  on.exit(unlink(md_path, recursive = TRUE), add = TRUE)
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("@includeRmd requires the rmarkdown package")
  }
  rmd_path <- rmd_process_links(tag)
  on.exit(unlink(rmd_path, recursive = TRUE), add = TRUE)
  rmarkdown::render(rmd_path, output_format = rmarkdown::github_document(),
                    output_file = md_path, quiet = TRUE)
  rmd_eval_rd(md_path)
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

rmd_eval_rd <- function(path) {
  mdtxt <- paste(read_lines(path), collapse = "\n")
  mdesc <- add_linkrefs_to_md(mdtxt)
  mdx <- commonmark::markdown_xml(mdesc, hardbreaks = TRUE)
  mdxml <- xml2::read_xml(mdx)
  state <- new.env(parent = emptyenv())
  state$tag <- "@includeRmd"
  rd <- mdxml_children_to_rd(mdxml, state = state)
  rd
}
