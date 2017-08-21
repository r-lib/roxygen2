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
  } else if (identical(x$tag, "include")) {
    tag_value(x)
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

