#' Expand embedded inline code
#'
#' @description
#' For example this becomes two: `r 1+1`.
#' Variables can be set and then reused, within the same
#' tag: `r x <- 100; NULL`
#' The value of `x` is `r x`.
#'
#' We have access to the internal functions of the package, e.g.
#' since this is _roxygen2_, we can refer to the internal `markdown`
#' function, and this is `TRUE`: `r is.function(markdown)`.
#'
#' To insert the name of the current package: `r packageName()`.
#'
#' The `iris` data set has `r ncol(iris)` columns:
#' `r paste0("\x60\x60", colnames(iris), "\x60\x60", collapse = ", ")`.
#'
#' ```{r}
#' # Code block demo
#' x + 1
#' ```
#'
#' Chunk options:
#'
#' ```{r results = "hold"}
#' names(mtcars)
#' nrow(mtcars)
#' ```
#'
#' Plots:
#'
#' ```{r test-figure}
#' plot(1:10)
#' ```
#'
#' Alternative knitr engines:
#'
#' ```{verbatim}
#' #| file = "tests/testthat/example.Rmd"
#' ```
#'
#' Also see `vignette("rd-formatting")`.
#'
#' @param text Input text.
#' @return
#' Text with R code expanded.
#' A character vector of the same length as the input `text`.
#'
#' @keywords internal
markdown_evaluate <- function(text) {
  text <- paste(text, collapse = "\n")
  mdxml <- xml_ns_strip(md_to_mdxml(text, sourcepos = TRUE))
  code_nodes <- xml_find_all(mdxml, ".//code | .//code_block")
  rcode_nodes <- keep(code_nodes, is_markdown_code_node)
  if (length(rcode_nodes) == 0) {
    return(text)
  }
  rcode_pos <- parse_md_pos(map_chr(rcode_nodes, xml_attr, "sourcepos"))
  rcode_pos <- work_around_cmark_sourcepos_bug(text, rcode_pos)
  out <- eval_code_nodes(rcode_nodes)
  str_set_all_pos(text, rcode_pos, out, rcode_nodes)
}

# Work around commonmark sourcepos bug for inline R code
# https://github.com/r-lib/roxygen2/issues/1353
work_around_cmark_sourcepos_bug <- function(text, rcode_pos) {
  if (Sys.getenv("ROXYGEN2_NO_SOURCEPOS_WORKAROUND", "") != "") {
    return(rcode_pos)
  }

  lines <- str_split(text, fixed("\n"))[[1]]

  for (l in seq_len(nrow(rcode_pos))) {
    # Do not try to fix multi-line code, we error for that (below)
    if (rcode_pos$start_line[l] != rcode_pos$end_line[l]) {
      next
    }
    line <- lines[rcode_pos$start_line[l]]
    start <- rcode_pos$start_column[l]

    # Maybe correct? At some point this will be fixed upstream, hopefully.
    if (str_sub(line, start - 1, start + 1) == "`r ") {
      next
    }

    # Maybe indented and we can shift it?
    # It is possible that the shift that we try accidentally matches
    # "`r ", but it seems to be extremely unlikely. An example is this:
    # #'       ``1`r `` `r 22*10`
    # (seven spaces after the #', so an indent of six spaces. If we shift
    # the real "`r " left by six characters, there happens to be another
    # "`r " there.

    indent <- nchar(str_extract(line, "^[ ]+"))
    if (
      !is.na(indent) &&
        str_sub(line, start - 1 + indent, start + 1 + indent) == "`r "
    ) {
      rcode_pos$start_column[l] <- rcode_pos$start_column[l] + indent
      rcode_pos$end_column[l] <- rcode_pos$end_column[l] + indent
    }
  }

  rcode_pos
}

is_markdown_code_node <- function(x) {
  info <- xml_attr(x, "info")
  str_sub(xml_text(x), 1, 2) == "r " ||
    (!is.na(info) && grepl("^[{][a-zA-z]+[}, ]", info))
}

parse_md_pos <- function(text) {
  nums <- map(strsplit(text, "[:-]"), as.integer)
  data.frame(
    start_line = map_int(nums, \(x) x[[1]]),
    start_column = map_int(nums, \(x) x[[2]]),
    end_line = map_int(nums, \(x) x[[3]]),
    end_column = map_int(nums, \(x) x[[4]])
  )
}

eval_code_nodes <- function(nodes) {
  evalenv <- roxy_meta_get("evalenv")
  # This should only happen in our test cases
  if (is.null(evalenv)) {
    evalenv <- new.env(parent = baseenv())
  }

  map_chr(nodes, eval_code_node, env = evalenv)
}

eval_code_node <- function(node, env) {
  if (xml_name(node) == "code") {
    # write knitr markup for inline code
    text <- paste0("`", xml_text(node), "`")
  } else {
    lang <- xml_attr(node, "info")
    # write knitr markup for fenced code
    text <- paste0("```", if (!is.na(lang)) lang, "\n", xml_text(node), "```\n")
  }

  chunk_opts <- utils::modifyList(
    knitr_chunk_defaults(),
    as.list(roxy_meta_get("knitr_chunk_options", NULL))
  )

  roxy_knit(text, env, chunk_opts)
}

knitr_chunk_defaults <- function() {
  list(
    error = FALSE,
    fig.path = "man/figures/",
    fig.process = basename,
    comment = "#>",
    collapse = TRUE
  )
}

str_set_all_pos <- function(text, pos, value, nodes) {
  # Cmark has a bug when reporting source positions for multi-line
  # code tags, and it does not count the indenting space in the
  # continuation lines: https://github.com/commonmark/cmark/issues/296
  types <- xml_name(nodes)
  if (any(types == "code" & pos$start_line != pos$end_line)) {
    cli::cli_abort("multi-line `r ` markup is not supported", call = NULL)
  }

  # Need to split the string, because of the potential multi-line
  # code tags, and then also recode the positions
  lens <- nchar(str_split(text, fixed("\n"))[[1]])
  shifts <- c(0, cumsum(lens + 1L))
  shifts <- shifts[-length(shifts)]
  start <- shifts[pos$start_line] + pos$start_column
  end <- shifts[pos$end_line] + pos$end_column

  # Create intervals for the parts we keep
  keep_start <- c(1, end + 2L)
  keep_end <- c(start - 2L, nchar(text))

  # Now piece them together
  out <- paste0(
    substring(text, keep_start, keep_end),
    c(value, ""),
    collapse = ""
  )
  attributes(out) <- attributes(text)
  out
}
