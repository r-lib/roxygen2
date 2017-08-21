parse_package <- function(base_path = ".",
                          env = source_package(base_path),
                          registry = default_tags(),
                          global_options = list()
                          ) {
  desc <- read_pkg_description(base_path)

  files <- package_files(base_path)
  list_of_blocks <- lapply(files, parse_blocks,
    env = env,
    registry = registry,
    global_options = global_options,
    file_encoding = desc$Encoding %||% "UTF-8"
  )

  blocks <- compact(unlist(list_of_blocks, recursive = FALSE))
  blocks
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
                       env = test_env(),
                       registry = default_tags(),
                       wrap = FALSE,
                       markdown = markdown_global_default) {
  if (missing(file) && !is.null(text)) {
    file <- tempfile()
    writeLines(text, file)
    on.exit(unlink(file))
  }

  parse_file(
    file,
    env = env,
    registry = registry,
    global_options = list(wrap = wrap, markdown = markdown)
  )
}

parse_text <- function(text,
                       env = test_env(),
                       registry = default_tags(),
                       global_options = list()) {

  file <- tempfile()
  writeLines(text, file)
  on.exit(unlink(file))

  parse_file(
    file,
    env = env,
    registry = registry,
    global_options = global_options
  )
}

parse_file <- function(file,
                       env = test_env(),
                       registry = default_tags(),
                       global_options = list()) {

  sys.source(file, envir = env)
  parse_blocks(
    file,
    env = env,
    registry = registry,
    global_options = global_options
  )
}

test_env <- function() {
  env <- new.env(parent = parent.env(globalenv()))
  methods::setPackageName("roxygen_devtest", env)
  env
}

parse_blocks <- function(file, env,
                         registry = list(),
                         global_options = list(),
                         file_encoding = "UTF-8") {

  tokenized <- tokenize_file(file, file_encoding = file_encoding)

  blocks <- purrr::pmap(tokenized, block_create,
    registry = registry,
    global_options = global_options
  )
  blocks <- compact(blocks)

  blocks <- lapply(blocks, block_set_env,
    env = env,
    registry = registry,
    global_options = global_options
  )

  blocks
}

parse_tags <- function(tokens, registry = list(), global_options = list()) {
  markdown_activate(tokens, global_options = global_options)

  tokens <- parse_description(tokens)
  tags <- compact(lapply(tokens, parse_tag, registry = registry))

  # Convert to existing named list format - this isn't ideal, but
  # it's what roxygen already uses
  vals <- lapply(tags, `[[`, "val")
  names <- vapply(tags, `[[`, "tag", FUN.VALUE = character(1))
  setNames(vals, names)
}

parse_tag <- function(x, registry) {
  stopifnot(is.roxy_tag(x))

  if (identical(x$tag, "eval")) {
    tag_code(x)
  } else if (x$tag %in% ls(registry)) {
    registry[[x$tag]](x)
  } else {
    roxy_tag_warning(x, "unknown tag")
  }
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

