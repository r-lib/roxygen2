parse_package <- function(base_path, load_code) {
  env <- load_code(base_path)
  #Read and parse the code in the collated order
  #   so that the documentation blocks are  parsed in the same order
  #     as the (collated order) R code
  #       so that any inherited param documentation is in the 'right' order.
  #   package_files (from source.R) almost does what is needed
  #     except that it does not provide the full names of the collated files.
  #   The following is a proof of concept 'solution'
  #     which needs to take care not to break calls to roxygenise
  #       initiated from places other than devtools and
  #       with base_path not = the current working directory.
  #     It could be made (lots) more elegant.
  #
  #   PS roxygenise should probably check / insist that the collate order
  #     in the DESCRIPTION file is up to date, eg :
  #       by running update_collate(base_path) or
  #       by insisting roclet 'collate' has been run
  #         (which is pretty much the same thing).
  #
  collated_files <- package_files(base_path)
  R_path <- paste(base_path,'R',sep='/')
  R_path_pattern <- str_replace_all(R_path,fixed('\\'),'\\\\')
  no_R_path <- !grepl(fixed(R_path_pattern),collated_files)
  collated_files[no_R_path] <- paste(R_path,collated_files[no_R_path],sep='/')
  #collated_files[no_R_path] <- normalizePath(collated_files[no_R_path],winslash='/')
  collated_files <- unique(collated_files)
  parsed <- lapply(collated_files, parse_file, env = env)
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
