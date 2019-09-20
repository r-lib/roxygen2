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

  topic$add(roxy_field_minidesc(
    label$type,
    label$label,
    tag$val$description
  ))
  dest$topic
}

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
