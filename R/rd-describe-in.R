process_describe_in <- function(block, env) {
  tags <- block[names(block) == "describeIn"]
  if (length(tags) == 0) return(list(rdname = NULL, tag = NULL))
  if (length(tags) > 1) {
    stop("May only use one @describeIn per block", call. = FALSE)
  }
  if (is.null(block$object)) {
    stop("@describeIn must be used with an object", call. = FALSE)
  }

  if(any(names(block) == "name")) {
    stop("@describeIn can not be used with @name", call. = FALSE)
  }
  if(any(names(block) == "rdname")) {
    stop("@describeIn can not be used with @rdname", call. = FALSE)
  }

  describe_in <- tags[[1]]
  dest <- find_object(describe_in$name, env)

  label <- build_label(block$object, dest)

  list(
    rdname = default_topic_name(dest),
    tag = new_tag("minidesc", list(
      type = label$type,
      label = label$label,
      desc = describe_in$description
    ))
  )
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

build_label <- function(src, dest) {
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
    label <- default_name(src)
  } else {
    stop("Don't know how to describe ", src_type, " in ", dest_type, ".",
      call. = FALSE)
  }

  list(type = type, label = label)
}
