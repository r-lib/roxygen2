#' @export
roxy_tag_parse.roxy_tag_R6method <- function(x) {
  raw <- trimws(x$raw)
  if (raw == "") {
    warn_roxy_tag(x, "requires a value like {.code Class$method}")
    return(NULL)
  }
  if (warn_if_multiline(x, raw)) {
    return(NULL)
  }

  pieces <- strsplit(raw, "\\$")[[1]]
  if (length(pieces) != 2 || pieces[1] == "" || pieces[2] == "") {
    warn_roxy_tag(x, "must be of the form {.code Class$method}")
    return(NULL)
  }

  x$val <- list(class = pieces[1], method = pieces[2])
  x
}

# Merge external @R6method blocks into their target R6 class blocks -----------

merge_external_r6methods <- function(blocks) {
  is_external <- map_lgl(blocks, function(b) {
    block_has_tags(b, "R6method") && !inherits(b, "roxy_block_r6class")
  })

  external <- blocks[is_external]
  if (length(external) == 0) {
    return(blocks)
  }

  blocks <- blocks[!is_external]

  # Build index of R6 class blocks by class name
  r6_index <- list()
  for (i in seq_along(blocks)) {
    if (inherits(blocks[[i]], "roxy_block_r6class")) {
      classname <- blocks[[i]]$object$value$classname
      if (!is.null(classname)) {
        r6_index[[classname]] <- i
      }
    }
  }

  for (ext_block in external) {
    r6method_tag <- block_get_tag(ext_block, "R6method")
    classname <- r6method_tag$val$class
    methodname <- r6method_tag$val$method

    idx <- r6_index[[classname]]
    if (is.null(idx)) {
      warn_roxy_tag(r6method_tag, "Can't find R6 class {.cls {classname}}")
      next
    }

    target <- blocks[[idx]]
    # Stamp all tags with the method name so r6_extract_methods()
    # can assign them by name; non-method tags will be filtered out
    # by the existing r6_tag_type() logic
    ext_tags <- discard(ext_block$tags, \(t) t$tag == "R6method")
    for (j in seq_along(ext_tags)) {
      ext_tags[[j]]$r6method <- methodname
    }
    target$tags <- c(target$tags, ext_tags)
    blocks[[idx]] <- target
  }

  blocks
}
