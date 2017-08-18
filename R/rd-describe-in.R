topic_add_describe_in <- function(topic, block, env) {
  tags <- block_tags(block, "describeIn")
  if (length(tags) == 0)
    return(NULL)

  if (length(tags) > 1) {
    block_warning(block, "May only use one @describeIn per block")
    return()
  }
  if (is.null(attr(block, "object"))) {
    block_warning(block, "@describeIn must be used with an object")
    return()
  }
  if (any(names(block) == "name")) {
    block_warning(block, "@describeIn can not be used with @name")
    return()
  }
  if (any(names(block) == "rdname")) {
    block_warning(block, "@describeIn can not be used with @rdname")
    return()
  }

  dest <- find_object(tags$describeIn$name, env)
  label <- build_label(attr(block, "object"), dest, block)
  if (is.null(label))
    return()

  topic$add(roxy_field_minidesc(
    label$type,
    label$label,
    tags$describeIn$description
  ))
  object_topic(dest)
}

# Imperfect:
# * will fail with S3 methods that need manual disambiguation (rare)
# * can't use if @name overridden, but then you could just the use alias
find_object <- function(name, env) {
  if (methods::isClass(name, where = env)) {
    object(methods::getClass(name, where = env))
  } else if (exists(name, envir = env)) {
    obj <- get(name, envir = env)
    obj <- standardise_obj(name, obj, env = env)
    object(obj, name)
  } else {
    object(NULL, name)
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
  } else if (dest_type %in% c("function", "data") && src_type == "function") {
    # Multiple functions in one Rd labelled with function names
    type <- "function"
    label <- object_name(src)
  } else {
    block_warning(block, "Don't know how to describe ", src_type, " in ", dest_type)
    return(NULL)
  }

  list(type = type, label = label)
}
