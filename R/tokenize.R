# Returns list of roxy_blocks
tokenize_file <- function(file, srcref_path = NULL) {
  lines <- read_lines(file)

  parsed <- parse(
    text = lines,
    keep.source = TRUE,
    srcfile = srcfilecopy(srcref_path %||% file, lines, isFile = TRUE)
  )
  if (length(parsed) == 0)
    return(list())

  refs <- utils::getSrcref(parsed)

  comment_refs <- comments(refs)
  tokens <- lapply(comment_refs, tokenise_ref)

  has_tokens <- !purrr::map_lgl(tokens, purrr::is_empty)

  blocks <- purrr::pmap(
    list(
      call = as.list(parsed)[has_tokens],
      srcref = refs[has_tokens],
      tokens = tokens[has_tokens]
    ),
    block_create
  )
  purrr::compact(blocks)
}

tokenise_ref <- function(x) {
  tokenise_block(
    as.character(x),
    file = attr(x, "srcfile")$filename,
    offset = x[[1]]
  )
}

# For each src ref, find the comment block preceding it
comments <- function(refs) {
  srcfile <- attr(refs[[1]], "srcfile")

  # first_line, first_byte, last_line, last_byte
  com <- vector("list", length(refs))
  for (i in seq_along(refs)) {
    # Comments begin after last line of last block, and this block is included
    # so that it can be parsed for additional comments
    if (i == 1) {
      first_byte <- 1
      first_line <- 1
    } else {
      first_byte <- refs[[i - 1]][4] + 1
      first_line <- refs[[i - 1]][3]
    }

    last_line <- refs[[i]][3]
    last_byte <- refs[[i]][4]

    lloc <- c(first_line, first_byte, last_line, last_byte)
    com[[i]] <- srcref(srcfile, lloc)
  }

  com
}
