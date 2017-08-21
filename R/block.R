roxy_block <- function(tags,
                       filename,
                       location,
                       call,
                       object = NULL) {
  stopifnot(is.list(tags))
  stopifnot(is.character(filename))
  stopifnot(is.vector(location))

  structure(
    tags,
    filename = filename,
    location = location,
    call = call,
    object = object,
    class = "roxy_block"
  )
}

roxy_block_copy <- function(block, tags) {
  roxy_block(
    tags,
    filename = attr(block, "filename"),
    location = attr(block, "location"),
    call = attr(block, "call"),
    object = attr(block, "object")
  )
}

is_roxy_block <- function(x) inherits(x, "roxy_block")

#' @export
print.roxy_block <- function(x, ...) {
  call <- deparse(attr(x, "call"), nlines = 2)
  if (length(call) == 2) {
    call <- paste0(call[[1]], " ...")
  }

  cat_line("<roxy_block> @ ", block_location(x))
  cat_line("  Tags: ", paste0(names(x), collapse = ", "))
  cat_line("  Call: ", call)
  cat_line("  Obj ? ", !is.null(attr(x, "object")))
}

# Creates roxy_block from list of raw tags ,
block_create <- function(tokens, call, srcref,
                         registry = list(),
                         global_options = list()) {

  tags <- parse_tags(tokens,
    registry = registry,
    global_options = global_options
  )
  if (length(tags) == 0) return()

  roxy_block(tags,
    filename = attr(srcref, "srcfile")$filename,
    location = as.vector(srcref),
    call = call
  )
}

block_set_env <- function(block, env,
                          registry = list(),
                          global_options = list()
                          ) {

  block <- block_evaluate(block, env, registry = registry, global_options = global_options)
  block <- block_find_object(block, env)
  block
}

block_evaluate <- function(block, env,
                           registry = list(),
                           global_options = list()
                           ) {

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
    file = attr(block, "filename"),
    offset = attr(block, "location")[[1]]
  )
  tags <- lapply(tokens, parse_tags,
    registry = registry,
    global_options = global_options
  )

  # Interpolate results back into original locations
  out <- lapply(block, list)
  out[is_eval] <- tags
  names(out)[is_eval] <- ""

  roxy_block_copy(block, compact(unlist(out, recursive = FALSE)))
}

block_find_object <- function(block, env) {
  stopifnot(is_roxy_block(block))

  object <- object_from_call(
    call = attr(block, "call"),
    env = env,
    block = block,
    file = attr(block, "filename")
  )
  attr(block, "object") <- object

  # Add in defaults generated from the object
  defaults <- object_defaults(object)

  for (tag in names(defaults)) {
    if (tag %in% names(block))
      next

    block[[tag]] <- defaults[[tag]]
  }

  block
}

block_location <- function(block) {
  if (is.null(block)) {
    NULL
  } else {
    paste0(basename(attr(block, "filename")), ":", attr(block, "location")[[1]])
  }
}

block_warning <- function(block, ...) {
  warning(
    block_location(block), ": ", ...,
    call. = FALSE,
    immediate. = TRUE
  )
  NULL
}
