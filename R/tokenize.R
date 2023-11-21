# Returns list of roxy_blocks
tokenize_file <- function(path, srcref_path = NULL) {
  lines <- read_lines(path)
  calls <- parse_lines(lines, srcref_path %||% path)
  srcrefs <- utils::getSrcref(calls)

  comment_refs <- comments(srcrefs)
  tokens <- lapply(comment_refs, tokenise_ref)

  blocks <- map(seq_along(tokens), function(i) {
    block_create(
      call = calls[[i]],
      srcref = srcrefs[[i]],
      tokens = tokens[[i]]
    )
  })
  compact(blocks)
}

parse_lines <- function(lines, path) {
  parse(
    text = lines,
    keep.source = TRUE,
    srcfile = srcfilecopy(path, lines, isFile = TRUE)
  )
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
  if (length(refs) == 0) {
    return(list())
  }

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
