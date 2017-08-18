parse_package <- function(base_path, load_code, registry, global_options = list()) {
  env <- load_code(base_path)
  desc <- read_pkg_description(base_path)

  files <- package_files(base_path)
  parsed <- lapply(files, parse_blocks, env = env, registry = registry,
                   global_options = global_options, fileEncoding = desc$Encoding %||% "UTF-8")
  blocks <- unlist(parsed, recursive = FALSE)

  list(env = env, blocks = blocks)
}

#' Parse roxygen2 comments in a piece of code.
#'
#' [parse_code()] takes a file or character vector, and an environment with R
#' values, and parses the roxygen2 comments found in it.
#'
#' @param file Path of the file to process.
#' @param text Instead of specifying `file`, users can also specify a character
#'   vector `text`, containing the R code.
#' @param env An environment containing the result of parsing and evaluating
#'   the file.
#' @param registry A roclet tag registry.
#' @param wrap Logical; should roxygen2 output be wrapped? `FALSE` by default.
#' @param markdown Logical value indicating whether to parse Markdown tags.
#'   This can be overridden locally by the tags '@md` and `@noMd` inside a
#'   Roxygen comment.
#' @param file_encoding The file encoding. Default: `"UTF-8"`.
#' @return A list of roxygen2 blocks.
#' @export
#' @keywords internal
parse_code <- function(file,
                       text = NULL,
                       env,
                       registry = default_tags(),
                       wrap = FALSE,
                       markdown = markdown_global_default,
                       file_encoding = "UTF-8") {
  if (missing(file) && !is.null(text)) {
    file <- tempfile()
    writeLines(text, file)
    on.exit(unlink(file))
  }

  options <- list(wrap = wrap, markdown = markdown)
  parse_blocks(
    file,
    env,
    registry = registry,
    global_options = options,
    fileEncoding = file_encoding
  )
}

parse_text <- function(text, registry = default_tags(), global_options = list()) {
  file <- tempfile()
  writeLines(text, file)
  on.exit(unlink(file))

  env <- new.env(parent = parent.env(globalenv()))
  methods::setPackageName("roxygen_devtest", env)

  sys.source(file, envir = env)
  blocks <- parse_blocks(file, env, registry = registry,
                         global_options = global_options)

  list(env = env, blocks = blocks)
}

parse_blocks <- function(file, env, registry = list(), global_options = list(), fileEncoding = "UTF-8") {

  # Manaully add eval tag to registry - it's always active
  registry$eval <- tag_code

  lines <- read_lines_enc(file, file_encoding = fileEncoding)
  parsed <- parse(text = lines, keep.source = TRUE, srcfile = srcfilecopy(file, lines, isFile = TRUE))
  if (length(parsed) == 0) return()

  refs <- utils::getSrcref(parsed)
  comment_refs <- comments(refs)

  extract <- function(call, ref, comment_ref) {
    block <- parse_block(
      comment_ref,
      file = file,
      env = env,
      registry = registry,
      global_options = global_options
    )
    if (length(block) == 0) return()

    block$object <- object_from_call(call, env, block, file)
    block$srcref <- list(filename = file, lloc = as.vector(ref))
    block <- block_evaluate(block, env, registry = registry)
    block_add_defaults(block)
  }

  Map(extract, parsed, refs, comment_refs)
}

parse_block <- function(x, file, env, registry, offset = x[[1]], global_options = list()) {
  tags <- tokenise_block(as.character(x), file = basename(file), offset = offset)
  if (length(tags) == 0)
    return()

  markdown_activate(tags, file, offset, global_options)

  tags <- parse_description(tags)
  parse_tags(tags, registry = registry)
}

parse_tags <- function(tags, registry) {
  tags <- compact(lapply(tags, parse_tag, registry = registry))

  # Convert to existing named list format - this isn't ideal, but
  # it's what roxygen already uses
  vals <- lapply(tags, `[[`, "val")
  names <- vapply(tags, `[[`, "tag", FUN.VALUE = character(1))
  setNames(vals, names)
}

parse_tag <- function(x, registry) {
  stopifnot(is.roxy_tag(x))

  if (x$tag %in% ls(registry)) {
    registry[[x$tag]](x)
  } else {
    roxy_tag_warning(x, "unknown tag")
  }
}

block_evaluate <- function(block, env, registry) {
  # @eval needs to return a character vector; one for each line
  # should not start with #'

  # Does not currently affect md/noMd status
  # Will not be run in templates
  # Not run recursively

  is_eval <- names(block) == "eval"
  eval <- block[is_eval]
  if (length(eval) == 0)
    return(block)

  # Evaluate
  results <- lapply(eval, block_eval,
    block = block,
    env = env,
    tag_name = "@eval"
  )
  results <- lapply(results, function(x) {
    if (is.null(x)) {
      character()
    } else {
      paste0("#' ", x)
    }
  })

  # Tokenise and parse
  tokens <- lapply(results, tokenise_block,
    file = block$srcref$filename,
    offset = block$srcref$lloc[[1]]
  )
  tags <- lapply(tokens, parse_tags, registry = registry)

  # Interpolate results back into original locations
  out <- lapply(block, list)
  out[is_eval] <- tags
  names(out)[is_eval] <- ""

  compact(unlist(out, recursive = FALSE))
}

parse_description <- function(tags) {
  if (length(tags) == 0) {
    return(tags)
  }

  tag_names <- vapply(tags, `[[`, "tag", FUN.VALUE = character(1))
  if (tag_names[1] != "") {
    return(tags)
  }

  intro <- tags[[1]]
  intro$val <- str_trim(intro$val)
  tags <- tags[-1]
  tag_names <- tag_names[-1]

  paragraphs <- str_trim(str_split(intro$val, fixed('\n\n'))[[1]])

  # 1st paragraph = title (unless has @title)
  if ("title" %in% tag_names) {
    title <- NULL
  } else if (length(paragraphs) > 0) {
    title <- roxy_tag("title", paragraphs[1], intro$file, intro$line)
    paragraphs <- paragraphs[-1]
  } else {
    title <- roxy_tag("title", "", intro$file, intro$line)
  }

  # 2nd paragraph = description (unless has @description)
  if ("description" %in% tag_names || length(paragraphs) == 0) {
    description <- NULL
  } else if (length(paragraphs) > 0) {
    description <- roxy_tag("description", paragraphs[1], intro$file, intro$line)
    paragraphs <- paragraphs[-1]
  }

  # Every thing else = details, combined with @details
  if (length(paragraphs) > 0) {
    details_para <- paste(paragraphs, collapse = "\n\n")

    # Find explicit @details tags
    didx <- which(tag_names == "details")
    if (length(didx) > 0) {
      explicit_details <- vapply(tags[didx], `[[`, "val",
        FUN.VALUE = character(1))
      tags <- tags[-didx]
      details_para <- paste(c(details_para, explicit_details), collapse = "\n\n")
    }

    details <- roxy_tag("details", details_para, intro$file, intro$line)
  } else {
    details <- NULL
  }

  c(compact(list(title, description, details)), tags)
}


# For each src ref, find the comment block preceeding it
comments <- function(refs) {
  srcfile <- attr(refs[[1]], "srcfile")

  # first_line, first_byte, last_line, last_byte
  com <- vector("list", length(refs))
  for(i in seq_along(refs)) {
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
