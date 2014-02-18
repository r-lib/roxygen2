parse_package <- function(base_path, load_code) {
  env <- load_code(base_path)
  parsed <- lapply(r_files(base_path), parse_file, env = env)
  blocks <- unlist(parsed, recursive = FALSE)

  list(env = env, blocks = blocks)
}

parse_text <- function(text) {
  file <- tempfile()
  writeLines(text, file)
  on.exit(unlink(file))

  env <- new.env(parent = parent.env(globalenv()))
  setPackageName("roxygen_devtest", env)

  sys.source(file, envir = env)
  blocks <- parse_file(file, env)

  list(env = env, blocks = blocks)
}

parse_file <- function(file, env) {
  parsed <- parse(file = file, keep.source = TRUE)
  if (length(parsed) == 0) return()

  refs <- getSrcref(parsed)
  comment_refs <- comments(refs)

  extract <- function(call, ref, comment_ref) {
    comment_ref2 <- list(filename = file, lloc = as.vector(comment_ref))

    errors_with_srcref(comment_ref2, {
      preref <- parse.preref(as.character(comment_ref))
      if (is.null(preref)) return()

      preref$object <- object_from_call(call, env, preref)
      preref$srcref <- list(filename = file, lloc = as.vector(ref))
      add_defaults(preref)
    })
  }

  Map(extract, parsed, refs, comment_refs)
}

# For each src ref, find the comment block preceeding it
comments <- function(refs) {
  srcfile <- attr(refs[[1]], "srcfile")

  # first_line, first_byte, last_line, last_byte
  com <- vector("list", length(refs))
  for(i in seq_along(refs)) {
    # Comments begin after last line of last block, and continue to
    # first line of this block
    if (i == 1) {
      first_byte <- 1
      first_line <- 1
    } else {
      first_byte <- refs[[i - 1]][4] + 1
      first_line <- refs[[i - 1]][3]
    }

    last_line <- refs[[i]][1]
    last_byte <- refs[[i]][2] - 1
    if (last_byte == 0) {
      if (last_line == 1) {
        last_byte <- 1
        last_line <- 1
      } else {
        last_line <- last_line - 1
        last_byte <- 1e3
      }
    }

    lloc <- c(first_line, first_byte, last_line, last_byte)
    com[[i]] <- srcref(srcfile, lloc)
  }

  com
}
