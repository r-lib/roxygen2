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
#' @param registry A roclet tag registry: this is a named list where the
#'    name gives the tag and the value gives the parsing code. See the
#'    `tag_` functions in [roxy_tag] for built-in options.
#' @param global_options A list of global options:
#'
#'  * `wrap` Logical; should roxygen2 output be wrapped? `FALSE` by default.
#'  * `markdown`` Logical value indicating whether to parse Markdown tags.
#' @return A list of roxy_block objects
#' @export
#' @keywords internal
parse_package <- function(path = ".",
                          env = env_package(path),
                          registry = default_tags(),
                          global_options = list()
                          ) {

  files <- package_files(path)
  list_of_blocks <- lapply(files, tokenize_file,
    registry = registry,
    global_options = global_options
  )

  blocks <- purrr::flatten(list_of_blocks)

  if (!is.null(env)) {
    blocks <- lapply(blocks, block_set_env,
      env = env,
      registry = registry,
      global_options = global_options
    )
  }

  blocks
}

#' @export
#' @rdname parse_package
parse_file <- function(file,
                       env = env_file(file),
                       registry = default_tags(),
                       global_options = list()) {

  blocks <- tokenize_file(file,
    registry = registry,
    global_options = global_options
  )

  if (!is.null(env)) {
    blocks <- lapply(blocks, block_set_env,
      env = env,
      registry = registry,
      global_options = global_options
    )
  }

  blocks
}

#' @export
#' @rdname parse_package
parse_text <- function(text,
                       env = env_file(file),
                       registry = default_tags(),
                       global_options = list()) {

  file <- tempfile()
  write_lines(text, file)
  on.exit(unlink(file))

  parse_file(
    file,
    env = env,
    registry = registry,
    global_options = global_options
  )
}

#' @export
#' @rdname parse_package
env_file <- function(file) {
  env <- new.env(parent = parent.env(globalenv()))
  methods::setPackageName("roxygen_devtest", env)

  sys.source(file, envir = env)
  env
}

#' @export
#' @rdname parse_package
env_package <- function(path) {
  pkgload::load_all(path)$env
}
