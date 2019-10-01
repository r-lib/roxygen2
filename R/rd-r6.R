
topic_add_r6_methods <- function(rd, block, env) {
  # We get the @field, @param, @examples, and @return tags that belong
  # to the methods. We match them to methods.
  r6data <- block_get_tag_value(block, ".r6data")
  self <- r6data$self
  methods <- self[self$type == "method", ]
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

  # Now do the main tags first. We leave out the param tags, those are
  # for the methods
  for (tag in block$tags) {
    if (! tag$tag %in% c("param", "field")) {
      rd$add(roxy_tag_rd(tag, env = env, base_path = base_path))
    }
  }

  # We need to add the whole thing as a big section.
  rd_lines <- c(
    r6_superclass(block, r6data, env),
    r6_fields(block, r6data),
    r6_active_bindings(block, r6data),
    r6_methods(block, r6data, methods)
  )

  rd$add(rd_section("rawRd", paste(rd_lines, collapse = "\n")))
}

r6_superclass <- function(block, r6data, env) {
  super <- r6data$super
  cls <- unique(super$classname)
  if (length(cls) == 0) return()

  lines <- character()
  push <- function(...) lines <<- c(lines, ...)

  title <- if (length(cls) > 1) "Super classes" else "Super class"
  push(paste0("\\section{", title, "}{"))

  pkgs <- super$package[match(cls, super$classname)]
  clsx <- c(rev(cls), block$object$alias)
  pkgsx <- c(rev(pkgs), environmentName(env))
  path <- sprintf("\\code{\\link[%s:%s]{%s::%s}}", pkgsx, clsx, pkgsx, clsx)
  push(paste0(path, collapse = " -> "))

  push("}")

  lines
}

r6_fields <- function(block, r6data) {
  self <- r6data$self
  fields <- self$name[self$type == "field"]
  active <- self$name[self$type == "active"]

  tags <- purrr::keep(
    block$tags,
    function(t) t$tag == "field" && ! t$val$name %in% active
  )

  labels <- gsub(",", ", ", map_chr(tags, c("val", "name")))
  docd <- str_trim(unlist(strsplit(labels, ",")))

  # Check for missing fields
  miss <- setdiff(fields, docd)
  for (f in miss) {
    msg <- sprintf(
      "Undocumented R6 field for block at %s:%i: `%s`",
      block$file, block$line, f
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  # Check for duplicate fields
  dup <- unique(docd[duplicated(docd)])
  for (f in dup) {
    msg <- sprintf(
      "R6 field `%s` documented multiple times for block at %s:%i",
      f, block$file, block$line
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  # Check for extra fields
  xtra <- setdiff(docd, fields)
  for (f in xtra) {
    msg <- sprintf(
      "Unknown R6 field `%s` for block at %s:%i",
      f, block$file, block$line
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  if (length(fields) == 0) return()

  # We keep the order of the documentation

  vals <- map_chr(tags, c("val", "description"))
  c("\\section{Public fields}{",
    "\\if{html}{\\out{<div class=\"r6-fields\">}}",
    "\\tabular{rl}{",
    paste0(labels, "\\tab ", vals, "\\cr ", collapse = "\n\n"),
    "}",
    "\\if{html}{\\out{</div>}}",
    "}"
  )
}

r6_active_bindings <- function(block, r6data) {
  self <- r6data$self
  fields <- self$name[self$type == "field"]
  active <- self$name[self$type == "active"]

  tags <- purrr::keep(
    block$tags,
    function(t) t$tag == "field" && ! t$val$name %in% fields
  )

  labels <- gsub(",", ", ", map_chr(tags, c("val", "name")))
  docd <- str_trim(unlist(strsplit(labels, ",")))

  # Check for missing bindings
  miss <- setdiff(active, docd)
  for (f in miss) {
    msg <- sprintf(
      "Undocumented R6 active binding for block at %s:%i: `%s`",
      block$file, block$line, f
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  # Check for duplicate bindings
  dup <- unique(docd[duplicated(docd)])
  for (f in dup) {
    msg <- sprintf(
      "R6 active binding `%s` documented multiple times for block at %s:%i",
      f, block$file, block$line
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  if (length(active) == 0) return()

  # We keep the order of the documentation

  vals <- map_chr(tags, c("val", "description"))
  c("\\section{Active bindings}{",
    "\\if{html}{\\out{<div class=\"r6-active-bindings\">}}",
    "\\tabular{rl}{",
    paste0(labels, "\\tab ", vals, "\\cr ", collapse = "\n\n"),
    "}",
    "\\if{html}{\\out{</div>}}",
    "}"
  )
}

r6_methods <- function(block, r6data, methods) {
  # And then the methods, if any
  if (nrow(methods) == 0) return()

  lines <- character()
  push <- function(...) lines <<- c(lines, ...)

  push("\\section{Methods}{")
  push(r6_method_list(block, methods))
  push(r6_inherited_method_list(block, r6data))
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

  lines
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

r6_inherited_method_list <- function(block, r6data) {
  super <- r6data$super
  if (is.null(super)) return()

  # drop methods that were shadowed in a subclass
  self <- r6data$self
  super <- super[! super$name %in% self$name, ]
  super <- super[! duplicated(super$name), ]

  c("\\if{html}{\\subsection{Inherited methods}{",
    "\\itemize{",
    sprintf(
      "\\item \\href{../../%s/html/%s.html#method-%s}{\\code{%s::%s$%s()}}",
      super$package,
      super$classname,
      super$name,
      super$package,
      super$classname,
      super$name
    ),
    "}",
    "}}"
  )
}

r6_method_begin <- function(block, method) {
  nm <- r6_show_name(method$name)
  c(
    "\\if{html}{\\out{<hr>}}",
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
  nms <- gsub(",", ", ", map_chr(par, c("val", "name")))

  # Each arg should appear exactly once
  mnames <- str_trim(unlist(strsplit(nms, ",")))
  dup <- unique(mnames[duplicated(mnames)])
  for (m in dup) {
    msg <- sprintf(
      "Argument `%s` documented multiple times for R6 method `%s` at %s:%i",
      m, method$name, method$file, method$line)
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  # Now add the missing ones from the class
  fnames <- names(method$formals[[1]])
  miss <- setdiff(fnames, mnames)
  is_in_cls <- map_lgl(
    block$tags,
    function(t) {
      !is.na(t$line) && t$line < block$line && t$tag == "param" &&
        t$val$name %in% miss
    }
  )
  par <- c(par, block$tags[is_in_cls])

  # Check if anything is missing
  nms <- gsub(",", ", ", map_chr(par, c("val", "name")))
  mnames <- str_trim(unlist(strsplit(nms, ",")))
  miss <- setdiff(fnames, mnames)
    for (m in miss) {
    msg <- sprintf(
      "Undocumented argument for R6 method `%s()` at %s:%i: `%s`",
      method$name, method$file, method$line, m
    )
    warning(msg, call. = FALSE, immediate. = TRUE)
  }

  if (length(par) == 0) return()

  # Order them according to formals
  firstnames <- str_trim(
    map_chr(strsplit(map_chr(par, c("val", "name")), ","), 1)
  )
  par <- par[order(match(firstnames, fnames))]

  val <- map_chr(par, c("val", "description"))
  nms <- gsub(",", ", ", map_chr(par, c("val", "name")))

  # Ready to go
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
    ret$val,
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
    "}"
  )
}
