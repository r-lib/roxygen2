#' Parse a package, file, or inline code
#'
#' `parse_package()`, `parse_file()`, and `parse_text()` allow you to use
#' roxygen's parsing code to parse the roxygen blocks from a package, file, or
#' character vector of code. `env_package()` and `env_file()` provide
#' defaults that generate a temporary environment making it possible to
#' associate each block with the corresponding live object.
#'
#' @param path,file,text Either specify a `path` to the root directory of
#'   a package, an R `file`, or a character vector `text`.
#' @param env An environment environment containing the result of evaluating
#'   the input code. The defaults will do this for you in a test environment:
#'   for real code you'll need to generate the environment yourself.
#'
#'    You can also set to `NULL` if you only want to get the tokenized code
#'    blocks only. This suppresses evaluation of `@eval` tags, and will not
#'    find the code object associated with each block.
#' @return A list of roxy_block objects
#' @export
#' @keywords internal
parse_package <- function(path = ".", env = env_package(path)) {
  files <- package_files(path)
  list_of_blocks <- lapply(files, tokenize_file)

  blocks <- purrr::flatten(list_of_blocks)

  if (!is.null(env)) {
    blocks <- lapply(blocks, block_set_env, env = env)
  }

  blocks <- order_blocks(blocks)
  blocks
}

#' @export
#' @rdname parse_package
parse_file <- function(file, env = env_file(file), srcref_path = NULL) {
  blocks <- tokenize_file(file, srcref_path = srcref_path)

  if (!is.null(env)) {
    blocks <- lapply(blocks, block_set_env, env = env)
  }

  blocks <- order_blocks(blocks)
  blocks
}

#' @export
#' @rdname parse_package
parse_text <- function(text, env = env_file(file)) {
  file <- tempfile()
  write_lines(text, file)
  on.exit(unlink(file))

  blocks <- parse_file(file, env = env, srcref_path = "<text>")
  blocks
}

#' @export
#' @rdname parse_package
env_file <- function(file) {
  env <- new.env(parent = parent.env(globalenv()))
  methods::setPackageName("roxygen_devtest", env)

  sys.source(file, envir = env, keep.source = TRUE)
  env
}

#' @export
#' @rdname parse_package
env_package <- function(path) {
  load_pkgload(path)
}

# helpers -----------------------------------------------------------------

order_blocks <- function(blocks) {
  block_order <- function(x) {
    if (block_has_tags(x, "order")) {
      ord <- block_get_tag_value(x, "order")
      as.double(ord)
    } else {
      Inf
    }
  }

  ord <- vapply(blocks, block_order, double(1))
  blocks[order(ord)]
}

#' @export
roxy_tag_parse.roxy_tag_order <- function(x) {
  tag_value(x)
}
