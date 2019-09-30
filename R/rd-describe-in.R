#' @export
roxy_tag_parse.roxy_tag_describeIn <- function(x) {
  tag_name_description(x)
}

topic_add_describe_in <- function(topic, block, env) {
  tag <- block_get_tag(block, "describeIn")
  if (is.null(tag)) {
    return()
  }

  if (is.null(block$object)) {
    roxy_tag_warning(tag, "must be used with an object")
    return()
  }
  if (block_has_tags(block,  "name")) {
    roxy_tag_warning(tag, "can not be used with @name")
    return()
  }
  if (block_has_tags(block, "rdname")) {
    roxy_tag_warning(tag, "can not be used with @rdname")
    return()
  }

  dest <- find_object(tag$val$name, env)
  label <- build_label(block$object, dest, block)
  if (is.null(label))
    return()

  topic$add(rd_section_minidesc(
    label$type,
    label$label,
    tag$val$description
  ))
  dest$topic
}

# Field -------------------------------------------------------------------

rd_section_minidesc <- function(type, label, desc) {
  stopifnot(is.character(type), is.character(label), is.character(desc))
  stopifnot(length(desc) == length(label))

  rd_section("minidesc", list(type = type, desc = desc, label = label))
}

#' @export
merge.rd_section_minidesc <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  stopifnot(identical(x$value$type, y$value$type))
  rd_section_minidesc(
    x$value$type,
    label = c(x$value$label, y$value$label),
    desc = c(x$value$desc, y$value$desc)
  )
}

#' @export
format.rd_section_minidesc <- function(x, ...) {
  title <- switch(x$value$type,
    generic = "Methods (by class)",
    class = "Methods (by generic)",
    "function" = "Functions"
  )

  paste0(
    "\\section{", title, "}{\n",
    "\\itemize{\n",
    paste0("\\item \\code{", escape(x$value$label), "}: ", x$value$desc,
      collapse = "\n\n"),
    "\n}}\n"
  )
}

# Helpers -----------------------------------------------------------------

# Imperfect:
# * will fail with S3 methods that need manual disambiguation (rare)
# * can't use if @name overridden, but then you could just the use alias
find_object <- function(name, env) {
  if (methods::isClass(name, where = env)) {
    object(methods::getClass(name, where = env), NULL, "s4class")
  } else if (exists(name, envir = env)) {
    object_from_name(name, env, NULL)
  } else {
    object(NULL, name, "data")
  }
}

build_label <- function(src, dest, block) {
  src_type <- class(src)[1]
  dest_type <- class(dest)[1]

  if (dest_type == "s4class" && src_type == "s4method") {
    # Label S4 methods in class with their generic
    type <- "class"
    label <- as.character(src$value@generic)
  } else if (dest_type == "s4generic" && src_type == "s4method") {
    # Label S4 methods in generic with their signature
    type <- "generic"
    sig <- src$value@defined
    if (length(sig) == 1) {
      label <- as.character(sig)
    } else {
      label <- paste0(names(sig), " = ", sig, collapse = ",")
    }
  } else if (dest_type == "function" && src_type == "s3method") {
    # Assuming you document S3 methods in the class constructor
    type <- "class"
    label <- attr(src$value, "s3method")[1]
  } else if (dest_type == "s3generic" && src_type == "s3method") {
    # Label S3 methods in generic with their class
    type <- "generic"
    label <- attr(src$value, "s3method")[2]
  } else {
    # Otherwise just fallback to function + topic
    type <- "function"
    label <- src$topic
  }

  list(type = type, label = label)
}
