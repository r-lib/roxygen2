#' Blocks
#'
#' @description
#' A `roxy_block` represents a single roxygen2 block.
#'
#' The `block_*` functions provide a few helpers for common operations:
#' * `block_has_tags(blocks, tags)`: does `block` contain any of these `tags`?
#' * `block_get_tags(block, tags)`: get all instances of `tags`
#' * `block_get_tag(block, tag)`: get single tag. Returns `NULL` if 0,
#'    throws warning if more than 1.
#' * `block_get_tag_value(block, tag)`: gets `val` field from single tag.
#'
#' @param tags A list of [roxy_tag]s.
#' @param file,line Location of the `call` (i.e. the line after the last
#'   line of the block).
#' @param call Expression associated with block.
#' @param object Optionally, the object associated with the block, found
#'   by inspecting/evaluating `call`.
#' @param block A `roxy_block` to manipulate.
#' @param tag A single tag name.
#' @export
#' @keywords internal
#' @examples
#' # The easiest way to see the structure of a roxy_block is to create one
#' # using parse_text:
#' text <- "
#'   #' This is a title
#'   #'
#'   #' @param x,y A number
#'   #' @export
#'   f <- function(x, y) x + y
#' "
#'
#' # parse_text() returns a list of blocks, so I extract the first
#' block <- parse_text(text)[[1]]
#' block
roxy_block <- function(tags, file, line, call, object = NULL) {
  stopifnot(is.list(tags))
  stopifnot(is.character(file), length(file) == 1)
  stopifnot(is.integer(line), length(line) == 1)

  structure(
    list(
      tags = tags,
      file = file,
      line = line,
      call = call,
      object = object
    ),
    class = "roxy_block"
  )
}

is_roxy_block <- function(x) inherits(x, "roxy_block")

#' @export
print.roxy_block <- function(x, ...) {
  call <- deparse(x$call, nlines = 2)
  if (length(call) == 2) {
    call <- paste0(call[[1]], " ...")
  }
  obj <- format(x$object)

  cat_line("<roxy_block> [", basename(x$file), ":", x$line, "]")
  cat_line("  $tag")
  cat_line("    ", map_chr(x$tags, format, file = x$file))
  cat_line("  $call   ", call)
  cat_line("  $object ", obj[[1]])
  cat_line("  ", obj[-1])
}

block_create <- function(call, srcref, tokens = c()) {
  if (is_empty(tokens)) {
    return(NULL)
  }

  tags <- parse_tags(tokens)
  if (length(tags) == 0) {
    return(NULL)
  }

  roxy_block(
    tags = tags,
    file = attr(srcref, "srcfile")$filename,
    line = srcref[[1]],
    call = call
  )
}

block_set_env <- function(block, env) {
  block <- block_evaluate(block, env)
  block <- block_find_object(block, env)
  block
}

block_evaluate <- function(block, env) {
  tags <- block_get_tags(block, "eval")
  if (length(tags) == 0) {
    return(block)
  }

  # Evaluate
  results <- lapply(tags, roxy_tag_eval, env = env)
  results <- lapply(results, function(x) {
    if (is.null(x)) {
      character()
    } else {
      paste0("#' ", x)
    }
  })

  # Tokenise and parse
  tokens <- lapply(
    results,
    tokenise_block,
    file = block$file,
    offset = block$line
  )
  tags <- lapply(tokens, parse_tags)

  # Interpolate results back into original locations
  block_replace_tags(block, "eval", tags)
}

block_find_object <- function(block, env) {
  stopifnot(is_roxy_block(block))

  object <- object_from_call(
    call = block$call,
    env = env,
    block = block,
    file = block$file
  )
  block$object <- object

  class(block) <- unique(c(
    paste0("roxy_block_", class(object)),
    class(block)
  ))

  # Add in defaults generated from the object
  defaults <- object_defaults(object, block)
  defaults <- c(
    defaults,
    list(roxy_generated_tag(block, "backref", block$file))
  )

  default_tags <- map_chr(defaults, "tag")
  defaults <- defaults[!default_tags %in% block_tags(block)]

  block$tags <- c(block$tags, defaults)
  block
}

# block accessors ---------------------------------------------------------

block_tags <- function(block) {
  map_chr(block$tags, "tag")
}

#' @export
#' @rdname roxy_block
block_has_tags <- function(block, tags) {
  any(block_tags(block) %in% tags)
}

#' @export
#' @rdname roxy_block
block_get_tags <- function(block, tags) {
  block$tags[block_tags(block) %in% tags]
}

#' @export
#' @rdname roxy_block
block_get_tag <- function(block, tag) {
  matches <- which(block_tags(block) %in% tag)
  n <- length(matches)
  if (n == 0) {
    NULL
  } else if (n == 1) {
    block$tags[[matches]]
  } else {
    warn_roxy_block(block, "Block must contain only one @{tag}")
    block$tags[[matches[[1]]]]
  }
}

#' @export
#' @rdname roxy_block
block_get_tag_value <- function(block, tag) {
  block_get_tag(block, tag)$val
}

block_replace_tags <- function(block, tags, values) {
  indx <- which(block_tags(block) %in% tags)
  stopifnot(length(indx) == length(values))

  tags <- lapply(block$tags, list)
  tags[indx] <- values

  block$tags <- compact(unlist(tags, recursive = FALSE))
  block
}

# parsing -----------------------------------------------------------------

parse_tags <- function(tokens) {
  # Set up evaluation environment for markdown
  pkgenv <- roxy_meta_get("env") %||% baseenv()
  evalenv <- new.env(parent = pkgenv)
  local_roxy_meta_set("evalenv", evalenv)

  markdown_activate(tokens)

  tokens <- parse_description(tokens)
  tokens <- map(tokens, roxy_tag_parse)
  compact(tokens)
}

#' @export
roxy_tag_parse.roxy_tag_eval <- function(x) {
  tag_code(x)
}

#' @export
roxy_tag_parse.roxy_tag_include <- function(x) {
  tag_value(x)
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
  intro$val <- str_trim(intro$raw)
  if (intro$val == "") {
    return(tags[-1])
  }

  tags <- tags[-1]
  tag_names <- tag_names[-1]

  paragraphs <- str_split(intro$val, fixed('\n\n'))[[1]]
  lines <- str_count(paragraphs, "\n") + rep(2, length(paragraphs))
  offsets <- c(0, cumsum(lines))

  # 1st paragraph = title (unless has @title)
  if ("title" %in% tag_names) {
    title <- NULL
  } else if (length(paragraphs) > 0) {
    title <- roxy_tag(
      "title",
      paragraphs[1],
      NULL,
      intro$file,
      intro$line + offsets[[1]]
    )
    paragraphs <- paragraphs[-1]
    offsets <- offsets[-1]
  } else {
    title <- roxy_tag("title", "", NULL, intro$file, intro$line)
  }

  # 2nd paragraph = description (unless has @description)
  if ("description" %in% tag_names || length(paragraphs) == 0) {
    description <- NULL
  } else if (length(paragraphs) > 0) {
    description <- roxy_tag(
      "description",
      paragraphs[1],
      NULL,
      intro$file,
      intro$line + offsets[[1]]
    )
    paragraphs <- paragraphs[-1]
    offsets <- offsets[-1]
  }

  # Every thing else = details, combined with @details
  if (length(paragraphs) > 0) {
    details_para <- paste(paragraphs, collapse = "\n\n")

    # Find explicit @details tags
    didx <- which(tag_names == "details")
    if (length(didx) > 0) {
      explicit_details <- map_chr(tags[didx], "raw")
      tags <- tags[-didx]
      details_para <- paste(
        c(details_para, explicit_details),
        collapse = "\n\n"
      )
    }

    details <- roxy_tag(
      "details",
      details_para,
      NULL,
      intro$file,
      intro$line + offsets[[1]]
    )
  } else {
    details <- NULL
  }

  c(compact(list(title, description, details)), tags)
}
