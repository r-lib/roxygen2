topic_add_r6_methods <- function(rd, block, env) {
  r6data <- block_get_tag_value(block, ".r6data")
  self <- r6data$self
  methods <- self[self$type == "method", ]
  methods <- methods[order(methods$file, methods$line), ]
  methods$tags <- replicate(nrow(methods), list(), simplify = FALSE)

  r6_tags <- c("description", "details", "param", "return", "examples")

  del <- integer()
  for (i in seq_along(block$tags)) {
    tag <- block$tags[[i]]
    # Not inline?
    if (is.na(tag$line) || tag$line < block$line) {
      next
    }
    # Not a method tag?
    if (!tag$tag %in% r6_tags) {
      next
    }
    del <- c(del, i)
    meth <- find_method_for_tag(methods, tag)
    if (is.na(meth)) {
      warn_roxy_tag(tag, "Cannot find matching R6 method")
      next
    }
    midx <- which(meth == methods$name)
    methods$tags[[midx]] <- c(methods$tags[[midx]], list(tag))
    del <- c(del, i)
  }

  methods <- add_default_methods(methods, block)

  nodoc <- map_int(methods$tags, length) == 0
  if (any(nodoc)) {
    warn_roxy_block(block, "Undocumented R6 method{?s}: {methods$name[nodoc]}")
  }

  block$tags[del] <- NULL

  # Now do the main tags first. We leave out the param tags, those are
  # for the methods
  for (tag in block$tags) {
    if (!tag$tag %in% c("param", "field")) {
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

  # Dump all method examples at the end of the examples block
  ex_lines <- r6_all_examples(block, methods)
  if (length(ex_lines) > 0) {
    ex_txt <- paste0(r6_all_examples(block, methods), collapse = "\n")
    rd$add(rd_section("examples", ex_txt), overwrite = FALSE)
  }
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

r6_superclass <- function(block, r6data, env) {
  super <- r6data$super
  cls <- unique(super$classes$classname)
  if (length(cls) == 0) {
    return()
  }

  lines <- character()
  push <- function(...) lines <<- c(lines, ...)

  title <- if (length(cls) > 1) "Super classes" else "Super class"
  push(paste0("\\section{", title, "}{"))

  pkgs <- super$classes$package[match(cls, super$classes$classname)]
  has_topic <- purrr::map2_lgl(cls, pkgs, has_topic)

  path <- ifelse(
    has_topic,
    sprintf("\\code{\\link[%s:%s]{%s::%s}}", pkgs, cls, pkgs, cls),
    sprintf("\\code{%s::%s}", pkgs, cls)
  )
  me <- sprintf("\\code{%s}", block$object$value$classname)
  push(paste(c(rev(path), me), collapse = " -> "))

  push("}")

  lines
}

r6_fields <- function(block, r6data) {
  self <- r6data$self
  fields <- self$name[self$type == "field"]
  active <- self$name[self$type == "active"]

  tags <- purrr::keep(
    block$tags,
    function(t) t$tag == "field" && !t$val$name %in% active
  )

  labels <- gsub(",", ", ", map_chr(tags, c("val", "name")))
  docd <- str_trim(unlist(strsplit(labels, ",")))

  # Check for missing fields
  miss <- setdiff(fields, docd)
  if (length(miss) > 0) {
    warn_roxy_block(block, "Undocumented R6 field{?s}: {miss}")
  }

  # Check for duplicate fields
  dup <- unique(docd[duplicated(docd)])
  if (length(dup) > 0) {
    warn_roxy_block(block, "R6 field{?s} documented multiple times: {dup}")
  }

  # Check for extra fields
  xtra <- setdiff(docd, fields)
  if (length(xtra) > 0) {
    warn_roxy_block(block, "Unknown R6 field{?s}: {xtra}")
  }

  if (length(docd) == 0) {
    return()
  }

  # We keep the order of the documentation

  vals <- map_chr(tags, c("val", "description"))
  c(
    "\\section{Public fields}{",
    "\\if{html}{\\out{<div class=\"r6-fields\">}}",
    "\\describe{",
    paste0("\\item{\\code{", labels, "}}{", vals, "}", collapse = "\n\n"),
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
    function(t) t$tag == "field" && !t$val$name %in% fields
  )

  labels <- gsub(",", ", ", map_chr(tags, c("val", "name")))
  docd <- str_trim(unlist(strsplit(labels, ",")))

  # Check for missing bindings
  miss <- setdiff(active, docd)
  if (length(miss) > 0) {
    warn_roxy_block(block, "Undocumented R6 active binding{?s}: {miss}")
  }

  # Check for duplicate bindings
  dup <- unique(docd[duplicated(docd)])
  if (length(dup) > 0) {
    warn_roxy_block(
      block,
      "R6 active binding{?s} documented multiple times: {dup}"
    )
  }

  if (length(docd) == 0) {
    return()
  }

  # We keep the order of the documentation

  vals <- map_chr(tags, c("val", "description"))
  c(
    "\\section{Active bindings}{",
    "\\if{html}{\\out{<div class=\"r6-active-bindings\">}}",
    "\\describe{",
    paste0("\\item{\\code{", labels, "}}{", vals, "}", collapse = "\n\n"),
    "}",
    "\\if{html}{\\out{</div>}}",
    "}"
  )
}

r6_methods <- function(block, r6data, methods) {
  # And then the methods, if any
  if (nrow(methods) == 0) {
    return()
  }

  lines <- character()
  push <- function(...) lines <<- c(lines, ...)

  push("\\section{Methods}{")
  push(r6_method_list(block, methods))
  push(r6_inherited_method_list(block, r6data))
  for (i in seq_len(nrow(methods))) {
    push(r6_method_begin(block, methods[i, ]))
    push(r6_method_description(block, methods[i, ]))
    push(r6_method_usage(block, methods[i, ]))
    push(r6_method_params(block, methods[i, ]))
    push(r6_method_details(block, methods[i, ]))
    push(r6_method_return(block, methods[i, ]))
    push(r6_method_examples(block, methods[i, ]))
    push(r6_method_end(block, methods[i, ]))
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

r6_method_list <- function(block, methods) {
  nms <- r6_show_name(methods$name)
  c(
    "\\subsection{Public methods}{",
    "\\itemize{",
    sprintf(
      "\\item \\href{#method-%s-%s}{\\code{%s$%s()}}",
      methods$class,
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
  if (is.null(super)) {
    return()
  }

  # drop methods that were shadowed in a subclass
  super_meth <- super$members[super$members$type == "method", ]
  self <- r6data$self
  super_meth <- super_meth[!super_meth$name %in% self$name, ]
  super_meth <- super_meth[!duplicated(super_meth$name), ]
  if (nrow(super_meth) == 0) {
    return()
  }

  super_meth <- super_meth[rev(seq_len(nrow(super_meth))), ]
  details <- paste0(
    "<details",
    if (nrow(super_meth) <= 5) " open",
    "><summary>Inherited methods</summary>"
  )

  c(
    "\\if{html}{\\out{",
    details,
    "<ul>",
    sprintf(
      paste0(
        "<li>",
        "<span class=\"pkg-link\" data-pkg=\"%s\" data-topic=\"%s\" data-id=\"%s\">",
        "<a href='../../%s/html/%s.html#method-%s-%s'><code>%s::%s$%s()</code></a>",
        "</span>",
        "</li>"
      ),
      super_meth$package,
      super_meth$classname,
      super_meth$name,
      super_meth$package,
      super_meth$classname,
      super_meth$classname,
      super_meth$name,
      super_meth$package,
      super_meth$classname,
      super_meth$name
    ),
    "</ul>",
    "</details>",
    "}}"
  )
}

r6_method_begin <- function(block, method) {
  nm <- r6_show_name(method$name)
  c(
    "\\if{html}{\\out{<hr>}}",
    paste0(
      "\\if{html}{\\out{<a id=\"method-",
      method$class,
      "-",
      nm,
      "\"></a>}}"
    ),
    paste0(
      "\\if{latex}{\\out{\\hypertarget{method-",
      method$class,
      "-",
      nm,
      "}{}}}"
    ),
    paste0("\\subsection{Method \\code{", nm, "()}}{")
  )
}

r6_method_description <- function(block, method) {
  det <- purrr::keep(method$tags[[1]], \(t) t$tag == "description")
  # Add an empty line between @description tags, if there isn't one
  # there already
  txt <- map_chr(det, "val")
  c(
    sub("\n?\n?$", "\n\n", head(txt, -1)),
    utils::tail(txt, 1)
  )
}

r6_method_usage <- function(block, method) {
  name <- paste0(block$object$alias, "$", r6_show_name(method$name))
  fake <- paste(rep("X", nchar(name)), collapse = "")
  usage <- format(function_usage(fake, method$formals[[1]]))
  c(
    "\\subsection{Usage}{",
    paste0(
      "\\if{html}{\\out{<div class=\"r\">}}",
      "\\preformatted{",
      sub(paste0("^", fake), name, usage),
      "}",
      "\\if{html}{\\out{</div>}}"
    ),
    "}\n"
  )
}

r6_method_details <- function(block, method) {
  det <- purrr::keep(method$tags[[1]], \(t) t$tag == "details")
  # Add an empty line between @details tags, if there isn't one
  # there already
  txt <- map_chr(det, "val")
  if (length(txt) == 0) {
    return()
  }
  c(
    "\\subsection{Details}{",
    sub("\n?\n?$", "\n\n", head(txt, -1)),
    utils::tail(txt, 1),
    "}\n"
  )
}

r6_method_params <- function(block, method) {
  par <- purrr::keep(method$tags[[1]], \(t) t$tag == "param")
  nms <- gsub(",", ", ", map_chr(par, c("val", "name")))

  # Each arg should appear exactly once
  mnames <- str_trim(unlist(strsplit(nms, ",")))
  dup <- unique(mnames[duplicated(mnames)])
  for (m in dup) {
    warn_roxy_block(
      block,
      c(
        "Must use one @param for each argument",
        x = "${method$name}({m}) is documented multiple times"
      )
    )
  }

  # Now add the missing ones from the class
  fnames <- names(method$formals[[1]])
  miss <- setdiff(fnames, mnames)
  is_in_cls <- map_lgl(
    block$tags,
    function(t) {
      !is.na(t$line) &&
        t$line < block$line &&
        t$tag == "param" &&
        t$val$name %in% miss
    }
  )
  par <- c(par, block$tags[is_in_cls])

  # Check if anything is missing
  nms <- gsub(",", ", ", map_chr(par, c("val", "name")))
  mnames <- str_trim(unlist(strsplit(nms, ",")))
  miss <- setdiff(fnames, mnames)
  for (m in miss) {
    warn_roxy_block(
      block,
      c(
        "Must use one @param for each argument",
        x = "${method$name}({m}) is not documented"
      )
    )
  }

  if (length(par) == 0) {
    return()
  }

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
    "\\describe{",
    paste0("\\item{\\code{", nms, "}}{", val, "}", collapse = "\n\n"),
    "}",
    "\\if{html}{\\out{</div>}}",
    "}"
  )
}

r6_method_return <- function(block, method) {
  ret <- purrr::keep(method$tags[[1]], \(t) t$tag == "return")
  if (length(ret) == 0) {
    return()
  }
  if (length(ret) > 1) {
    warn_roxy_block(block, "Must use one @return per R6 method")
  }
  ret <- ret[[1]]
  c(
    "\\subsection{Returns}{",
    ret$val,
    "}"
  )
}

r6_method_examples <- function(block, method) {
  exa <- purrr::keep(method$tags[[1]], \(t) t$tag == "examples")
  if (length(exa) == 0) {
    return()
  }

  txt <- map_chr(exa, "val")

  c(
    "\\subsection{Examples}{",
    paste0(
      "\\if{html}{\\out{<div class=\"r example copy\">}}\n",
      "\\preformatted{",
      txt,
      "\n",
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

r6_all_examples <- function(block, methods) {
  unlist(lapply(
    seq_len(nrow(methods)),
    function(i) {
      exa <- purrr::keep(methods$tags[[i]], \(t) t$tag == "examples")
      if (length(exa) == 0) {
        return()
      }
      name <- paste0(block$object$alias, "$", r6_show_name(methods$name[i]))
      c(
        "\n## ------------------------------------------------",
        paste0("## Method `", name, "`"),
        "## ------------------------------------------------\n",
        paste(map_chr(exa, "val"), collapse = "\n")
      )
    }
  ))
}

first_five <- function(x) {
  x <- encodeString(x, quote = "`")
  if (length(x) > 5) {
    x <- c(x[1:5], "...")
  }
  paste(x, collapse = ", ")
}
