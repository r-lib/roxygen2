rd_r6_class <- function(
  class,
  alias = class,
  superclasses = data.frame(),
  fields = rd_r6_fields(),
  active_bindings = rd_r6_bindings(),
  methods = list(),
  inherited_methods = data.frame()
) {
  structure(
    list(
      class = class,
      alias = alias,
      superclasses = superclasses,
      fields = fields,
      active_bindings = active_bindings,
      methods = methods,
      inherited_methods = inherited_methods
    ),
    class = "rd_r6_class"
  )
}

#' @export
format.rd_r6_class <- function(x, ...) {
  lines <- character()
  push <- function(...) lines <<- c(lines, ...)

  push(format_r6_superclasses(x))
  push(format(x$fields))
  push(format(x$active_bindings))

  if (length(x$methods) > 0) {
    push("\\section{Methods}{")
    push(format_r6_method_list(x))
    push(format_r6_inherited_methods(x$inherited_methods))
    for (method in x$methods) {
      push(format(method))
    }
    push("}")
  }

  lines
}

# Extraction ---------------------------------------------------------------

r6_class_from_block <- function(block, env) {
  r6data <- block_get_tag_value(block, ".r6data")
  self <- r6data$self
  class <- block$object$value$classname
  alias <- block$object$alias

  # Associate inline tags with methods
  methods_df <- self[self$type == "method", ]
  methods_df <- methods_df[order(methods_df$file, methods_df$line), ]
  methods_df$tags <- replicate(nrow(methods_df), list(), simplify = FALSE)

  r6_tags <- c("description", "details", "param", "return", "examples")
  for (i in seq_along(block$tags)) {
    tag <- block$tags[[i]]
    if (is.na(tag$line) || tag$line < block$line) {
      next
    }
    if (!tag$tag %in% r6_tags) {
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

  fields <- r6_extract_fields(block, r6data)
  active_bindings <- r6_extract_active_bindings(block, r6data)
  superclasses <- r6_extract_superclasses(r6data, env)
  inherited_methods <- r6_extract_inherited_methods(r6data)

  methods <- lapply(
    seq_len(nrow(methods_df)),
    function(i) r6_method_from_row(methods_df[i, ], alias, block)
  )

  rd_r6_class(
    class = class,
    alias = alias,
    superclasses = superclasses,
    fields = fields,
    active_bindings = active_bindings,
    methods = methods,
    inherited_methods = inherited_methods
  )
}

r6_extract_superclasses <- function(r6data, env) {
  super <- r6data$super
  cls <- unique(super$classes$classname)
  if (length(cls) == 0) {
    return(data.frame(
      package = character(),
      classname = character(),
      has_topic = logical()
    ))
  }

  pkgs <- super$classes$package[match(cls, super$classes$classname)]
  ht <- map2_lgl(cls, pkgs, has_topic)
  data.frame(package = pkgs, classname = cls, has_topic = ht)
}

r6_extract_inherited_methods <- function(r6data) {
  super <- r6data$super
  empty <- data.frame(
    package = character(),
    classname = character(),
    name = character()
  )
  if (is.null(super)) {
    return(empty)
  }

  super_meth <- super$members[super$members$type == "method", ]
  self <- r6data$self
  super_meth <- super_meth[!super_meth$name %in% self$name, ]
  super_meth <- super_meth[!duplicated(super_meth$name), ]
  if (nrow(super_meth) == 0) {
    return(empty)
  }

  super_meth <- super_meth[rev(seq_len(nrow(super_meth))), ]
  super_meth[, c("package", "classname", "name")]
}

# Format helpers -----------------------------------------------------------

format_r6_superclasses <- function(x) {
  if (nrow(x$superclasses) == 0) {
    return()
  }

  cls <- x$superclasses$classname
  pkgs <- x$superclasses$package
  ht <- x$superclasses$has_topic

  title <- if (length(cls) > 1) "Super classes" else "Super class"

  path <- ifelse(
    ht,
    sprintf("\\code{\\link[%s:%s]{%s::%s}}", pkgs, cls, pkgs, cls),
    sprintf("\\code{%s::%s}", pkgs, cls)
  )
  me <- sprintf("\\code{%s}", x$class)

  c(
    paste0("\\section{", title, "}{"),
    paste(c(rev(path), me), collapse = " -> "),
    "}"
  )
}

format_r6_method_list <- function(x) {
  nms <- r6_show_name(map_chr(x$methods, \(m) m$name))
  classes <- map_chr(x$methods, \(m) m$class)

  c(
    "\\subsection{Public methods}{",
    "\\itemize{",
    sprintf(
      "\\item \\href{#method-%s-%s}{\\code{%s$%s()}}",
      classes,
      nms,
      x$alias,
      nms
    ),
    "}",
    "}"
  )
}

format_r6_inherited_methods <- function(inherited_methods) {
  if (nrow(inherited_methods) == 0) {
    return()
  }

  im <- inherited_methods
  details <- paste0(
    "<details",
    if (nrow(im) <= 5) " open",
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
      im$package,
      im$classname,
      im$name,
      im$package,
      im$classname,
      im$classname,
      im$name,
      im$package,
      im$classname,
      im$name
    ),
    "</ul>",
    "</details>",
    "}}"
  )
}

# Utilities ---------------------------------------------------------------

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
  w <- which(
    basename(methods$file) == basename(tag$file) &
      methods$line > tag$line
  )[1]
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
  sections <- vapply(
    seq_along(tag$val)[-1],
    \(i) paste0("\\subsection{", titles[[i]], "}{\n", tag$val[[i]], "\n}"),
    character(1)
  )
  parts <- if (nzchar(tag$val[[1]])) c(tag$val[[1]], sections) else sections
  tag$val <- paste(parts, collapse = "\n\n")
  tag
}

r6_all_examples <- function(docs) {
  unlist(lapply(docs$methods, function(method) {
    if (length(method$examples) == 0) {
      return()
    }
    name <- paste0(docs$alias, "$", r6_show_name(method$name))
    c(
      "\n## ------------------------------------------------",
      paste0("## Method `", name, "`"),
      "## ------------------------------------------------\n",
      paste(method$examples, collapse = "\n")
    )
  }))
}
