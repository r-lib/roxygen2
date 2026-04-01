rd_r6_methods <- function(alias, self = list(), inherited = rd_r6_inherited()) {
  structure(
    list(alias = alias, self = self, inherited = inherited),
    class = "rd_r6_methods"
  )
}

#' @export
format.rd_r6_methods <- function(x, ...) {
  if (length(x$self) == 0) {
    return()
  }

  lines <- character()
  push <- function(...) lines <<- c(lines, ...)

  nms <- map_chr(x$self, \(m) m$name)
  classes <- map_chr(x$self, \(m) m$class)
  dest <- sprintf("method-%s-%s", classes, nms)
  code <- sprintf("\\code{%s()}", r6_method_name(classes, nms))

  push("\\section{Methods}{")
  push(
    "\\subsection{Public methods}{",
    "  \\itemize{",
    sprintf("    \\item \\href{#%s}{%s}", dest, code),
    "  }",
    "}"
  )
  push(format(x$inherited))
  for (method in x$self) {
    push(format(method))
  }
  push("}")

  lines
}

r6_extract_methods <- function(r6data, alias, block) {
  self <- r6data$self
  methods_df <- self[self$type == "method", ]
  methods_df <- methods_df[order(methods_df$file, methods_df$line), ]
  methods_df$tags <- replicate(nrow(methods_df), list(), simplify = FALSE)

  # Associate inline tags with methods
  for (i in seq_along(block$tags)) {
    tag <- block$tags[[i]]
    if (r6_tag_type(tag, block) != "method") {
      next
    }
    meth <- find_method_for_tag(methods_df, tag)
    if (is.na(meth)) {
      warn_roxy_tag(tag, "Cannot find matching R6 method")
      next
    }
    midx <- which(meth == methods_df$name)
    methods_df$tags[[midx]] <- c(methods_df$tags[[midx]], list(tag))
  }

  # Flatten markdown sections
  for (i in seq_along(methods_df$tags)) {
    methods_df$tags[[i]] <- lapply(methods_df$tags[[i]], r6_flatten_sections)
  }

  methods_df <- add_default_methods(methods_df, block)

  nodoc <- map_int(methods_df$tags, length) == 0
  if (any(nodoc)) {
    warn_roxy_block(
      block,
      "Undocumented R6 method{?s}: {methods_df$name[nodoc]}"
    )
  }

  # Methods with @noRd are deliberately suppressed
  has_noRd <- map_lgl(methods_df$tags, function(tags) {
    any(map_lgl(tags, \(t) t$tag == "noRd"))
  })
  methods_df <- methods_df[!has_noRd, ]

  self_methods <- lapply(
    seq_len(nrow(methods_df)),
    function(i) r6_method_from_row(methods_df[i, ], block)
  )
  inherited <- r6_extract_inherited_methods(r6data)
  rd_r6_methods(alias, self = self_methods, inherited = inherited)
}

add_default_methods <- function(methods, block) {
  defaults <- list(
    clone = list(
      roxy_generated_tag(
        block,
        "description",
        "The objects of this class are cloneable with this method."
      ),
      roxy_generated_tag(
        block,
        "param",
        list(name = "deep", description = "Whether to make a deep clone.")
      )
    )
  )

  for (mname in names(defaults)) {
    mline <- match(mname, methods$name)
    if (is.na(mline)) {
      next
    }
    if (length(methods$tags[[mline]]) > 0) {
      next
    }
    methods$tags[[mline]] <- defaults[[mname]]
  }

  methods
}

find_method_for_tag <- function(methods, tag) {
  if (nrow(methods) == 0) {
    return(NA_character_)
  }
  if (tag$file == "<text>") {
    # for testing
    same_file <- TRUE
  } else {
    same_file <- basename(methods$file) == basename(tag$file)
  }
  w <- which(same_file & methods$line > tag$line)[1]
  methods$name[w]
}

r6_flatten_sections <- function(tag) {
  if (!tag$tag %in% c("description", "details")) {
    return(tag)
  }
  if (length(tag$val) <= 1) {
    return(tag)
  }
  titles <- names(tag$val)
  sections <- map_chr(
    seq_along(tag$val)[-1],
    \(i) paste0("\\subsection{", titles[[i]], "}{\n", tag$val[[i]], "\n}")
  )
  parts <- if (nzchar(tag$val[[1]])) c(tag$val[[1]], sections) else sections
  tag$val <- paste(parts, collapse = "\n\n")
  tag
}

r6_all_examples <- function(methods) {
  unlist(lapply(methods$self, function(method) {
    if (length(method$examples) == 0) {
      return()
    }
    c(
      "\n## ------------------------------------------------",
      paste0("## Method `", r6_method_name(method$class, method$name), "()`"),
      "## ------------------------------------------------\n",
      paste(method$examples, collapse = "\n")
    )
  }))
}
