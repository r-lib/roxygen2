
topic_add_r6_methods <- function(rd, block, env) {
  # We get the @param, @examples, and @return tags that belong to
  # the methods. We match them to methods.
  r6data <- block_get_tag_value(block, ".r6data")
  methods <- r6data[r6data$type == "method", ]
  methods <- methods[order(methods$file, methods$line), ]
  methods$tags <- replicate(nrow(methods), list(), simplify = FALSE)

  r6_tags <- c("details", "param", "return", "examples")

  del <- integer()
  for (i in seq_along(block$tags)) {
    tag <- block$tags[[i]]
    # Not inline?
    if (is.na(tag$line) || tag$line < block$line) next
    # Not a method tag?
    if (! tag$tag %in% r6_tags) next
    meth <- find_method_for_tag(methods, tag)
    if (is.na(meth)) {
      roxy_tag_warning(tag, "Cannot find matching R6 method")
      next
    }
    midx <- which(meth == methods$name)
    methods$tags[[midx]] <- c(methods$tags[[midx]], list(tag))
    del <- c(del, i)
  }

  nodoc <- map_int(methods$tags, length) == 0
  for (i in which(nodoc)) {
    msg <- sprintf(
      "Undocumented R6 method at %s:%i: `%s()`",
      methods$file[i], methods$line[i], methods$name[i]
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  block$tags[del] <- NULL

  # Now do the main tags first
  for (tag in block$tags) {
    rd$add(roxy_tag_rd(tag, env = env, base_path = base_path))
  }

  # And then the methods, if any
  if (nrow(methods) == 0) return()

  # We need to add the whole thing as a big section, that's how RoxyTopic
  # works. So we just collect all the text.

  rd_lines <- character()
  push <- function(...) rd_lines <<- c(rd_lines, ...)

  push("\\section{Methods}{")
  push(r6_method_list(block, methods))
  for (i in seq_len(nrow(methods))) {
    push(r6_method_begin(block, methods[i,]))
    push(r6_method_usage(block, methods[i,]))
    push(r6_method_details(block, methods[i,]))
    push(r6_method_params(block, methods[i,]))
    push(r6_method_return(block, methods[i,]))
    push(r6_method_examples(block, methods[i,]))
    push(r6_method_end(block, methods[i,]))
  }
  push("}")

  rd$add(rd_section("rawRd", paste(rd_lines, collapse = "\n")))
}

find_method_for_tag <- function(methods, tag) {
  w <- which(
    basename(methods$file) == basename(tag$file) &
    methods$line > tag$line
  )[1]
  methods$name[w]
}

# vectorized

r6_show_name <- function(names) {
  ifelse(names == "initialize", "new", names)
}

# also vectorized

r6_method_list <- function(block, methods) {
  nms <- r6_show_name(methods$name)
  c("\\subsection{Public methods}{",
    "\\itemize{",
    sprintf(
      "\\item \\href{#method-%s}{\\code{%s$%s()}}",
      nms,
      block$object$alias,
      nms
    ),
    "}",
    "}"
  )
}

r6_method_begin <- function(block, method) {
  nm <- r6_show_name(method$name)
  c(
    paste0("\\if{html}{\\out{<a id=\"method-", nm, "\"></a>}}"),
    paste0("\\subsection{Method \\code{", nm, "()}}{")
  )
}

r6_method_usage <- function(block, method) {
  usage <- format(function_usage("XXX", method$formals[[1]]))
  name <- paste0(block$object$alias, "$", r6_show_name(method$name))
  c("\\if{html}{\\out{<div class=\"r\">}}",
    paste0("\\preformatted{", sub("^XXX", name, usage)),
    "}",
    "\\if{html}{\\out{</div>}}"
  )
}

r6_method_details <- function(block, method) {
  det <- purrr::keep(method$tags[[1]], function(t) t$tag == "details")
  # Add an empty line between @details tags, if there isn't one
  # there already
  txt <- map_chr(det, "val")
  c(
    sub("\n?\n?$", "\n\n", head(txt, -1)),
    utils::tail(txt, 1)
  )
}

r6_method_params <- function(block, method) {
  par <- purrr::keep(method$tags[[1]], function(t) t$tag == "param")
  if (length(par) == 0) return()
  nms <- map_chr(par, c("val", "name"))
  nms <- gsub(",", ", ", nms)
  val <- map_chr(par, c("val", "description"))
  c(
    "\\subsection{Arguments}{",
    "\\if{html}{\\out{<div class=\"arguments\">}}",
    "\\tabular{rl}{",
    paste0(nms, "\\tab ", val, "\\cr ", collapse = "\n\n"),
    "}",
    "\\if{html}{\\out{</div>}}",
    "}"
  )
}

r6_method_return <- function(block, method) {
  ret <- purrr::keep(method$tags[[1]], function(t) t$tag == "return")
  if (length(ret) == 0) return()
  if (length(ret) > 1) {
    roxy_tag_warning(ret[[2]], "May only use one @return per R6 method")
  }
  ret <- ret[[1]]
  c(
    "\\subsection{Returns}{",
    "\\describe{",
    ret$val,
    "}",
    "}"
  )
}

r6_method_examples <- function(block, method) {
  exa <- purrr::keep(method$tags[[1]], function(t) t$tag == "examples")
  if (length(exa) == 0) return()

  txt <- map_chr(exa, "val")

  c("\\subsection{Examples}{",
    paste0(
      "\\if{html}{\\out{<div class=\"r example copy\">}}\n",
      "\\preformatted{", txt, "\n",
      "}\n",
      "\\if{html}{\\out{</div>}}\n",
      collapse = "\n"
    ),
    "}\n"
  )
}

r6_method_end <- function(block, method) {
  c(
    "}",
    "\\if{html}{\\out{<hr>}}"
  )
}
