parse.describeIn <- function(key, rest) {
  check_rd(key, rest)

  pieces <- str_split_fixed(rest, " ", 2)
  type <- str_trim(pieces[[1]])
  if (!(type %in% c("generic", "class", "function"))) {
    stop("@describeIn must be followed by generic, class or function",
      call. = FALSE)
  }
  desc <- str_trim(pieces[[2]])

  list(type = type, desc = desc)
}

process_describeIn <- function(block) {
  tags <- block[names(block) == "describeIn"]
  if (length(tags) == 0) return()
  if (length(tags) > 1) {
    stop("May only use one @describeIn per block", call. = FALSE)
  }
  if (is.null(block$object)) {
    stop("@describeIn must be used with an object", call. = FALSE)
  }

  tag <- tags[[1]]
  tag$label <- switch(tag$type,
    # If documented in generic, label with class
    generic = label_class(block$object),
    # If documented in class, gets label with generic
    class = label_generic(block$object),
    "function" = label_class(block$object)
  )

  new_tag("minidesc", tag)
}

label_class <- function(obj) {
  if (inherits(obj, "s3method")) {
    attr(obj$value, "s3method")[2]
  } else if (inherits(obj, "s4method")) {
    sig <- obj$value@defined
    if (length(sig) == 1) {
      as.character(sig)
    } else {
      paste0(names(sig), " = ", sig, collapse = ",")
    }
  } else {
    stop("@describeIn class must be used with an S3 or S4 method",
      call = FALSE)
  }
}

# If documented in generic, gets labelled with class
label_generic <- function(obj) {
  if (inherits(obj, "s3method")) {
    attr(obj$value, "s3method")[1]
  } else if (inherits(obj, "s4method")) {
    as.character(obj$value@generic)
  } else {
    stop("@describeIn generic must be used with an S3 or S4 method",
      call = FALSE)
  }

}

label_function <- function(obj) {
  obj$name
}
