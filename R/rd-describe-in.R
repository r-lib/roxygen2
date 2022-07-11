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
    warn_roxy_tag(tag, "must be used with an object")
    return()
  }
  if (block_has_tags(block, "name")) {
    warn_roxy_tag(tag, "can not be used with @name")
    return()
  }
  if (block_has_tags(block, "rdname")) {
    warn_roxy_tag(tag, "can not be used with @rdname")
    return()
  }

  dest <- find_object(tag$val$name, env)
  metadata <- build_minidesc_metadata(block$object, dest)

  topic$add(exec(rd_section_minidesc,
    name = block$object$topic,
    desc = tag$val$description,
    !!!metadata
  ))
  dest$topic
}

# Field -------------------------------------------------------------------

#' Record data for minidescription sections from `@describeIn`
#'
#' @param name name of the source function.
#' @param desc description passed to `@describeIn`.
#' @param extends how the source function extends the destination function:
#' - `"generic"` if the source extends a (S3 or S4) generic in the destination,
#' - `"class"` if the source extends an informal S3 or formal S4 constructor
#'    in the destination.
#'    For S3, there is always only *one* class.
#'    For S4, the methods' signature is used instead, to cover multiple dispatch.
#' - `""` (default) otherwise.
#' @param generic,class name of the generic and class that is being extended by
#' the method, otherwise empty string (`""`).
#' @return a dataframe with one row for each `@describeIn`, wrapped inside
#' `rd_section()`
#' @noRd
rd_section_minidesc <- function(name,
                                desc,
                                extends = c("", "generic", "class"),
                                generic = "",
                                class = "") {
  stopifnot(is_scalar_character(name))
  stopifnot(is_character(desc))
  rlang::arg_match(extends)
  stopifnot(is_scalar_character(generic))
  stopifnot(is_scalar_character(class))
  data <- data.frame(
    name = name,
    desc = desc,
    extends = extends,
    generic = generic,
    class = class,
    stringsAsFactors = FALSE
  )
  rd_section(
    "minidesc",
    data
  )
}

#' @export
merge.rd_section_minidesc <- function(x, y, ..., block) {
  stopifnot(identical(class(x), class(y)))

  rd_section(
    "minidesc",
    rbind(x$value, y$value, stringsAsFactors = FALSE)
  )
}

# Rd Output -------------------------------------------------------------------

#' @export
format.rd_section_minidesc <- function(x, ...) {
  subsections <- split(x$value, x$value$extends)
  body <- purrr::map2_chr(subsections, names(subsections), format_section)

  paste0(body, collapse = "\n")
}

format_section <- function(df, type) {
  if (type == "class") {
    title <- paste0(
      "Methods for class \\code{", escape(df$class[[1]]), "}"
    )
  } else if (type == "generic") {
    title <- paste0(
      "Methods for generic \\code{", escape(df$generic[[1]]), "()}"
    )
  } else {
    title <- "Related functions"
  }

  bullets <- paste0("\\code{", escape(df$name), "}: ", df$desc)
  body <- paste0(
    "\\itemize{\n",
    paste0("  \\item ", bullets, "\n", collapse = ""),
    "}\n"
  )

  paste0("\\section{", title, "}{\n", body, "}")
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

#' Build metadata for how to present `@describeIn` tag
#' @return list of character scalars named `extends`, `generic`, `class`.
#' See rd_section_minidesc() for details.
#' @noRd
build_minidesc_metadata <- function(src, dest) {
  src_type <- class(src)[1]
  dest_type <- class(dest)[1]
  dest_name <- as.character(dest$topic)

  if (src_type == "s3method") {
    generic <- attr(src$value, "s3method")[1]
    class <- attr(src$value, "s3method")[2]
    if (dest_type == "s3generic" && generic == dest_name) {
      # src method fits dest generic
      extends <- "generic"
    } else if (fits_constructor(dest_name, src)) {
      # src method fits informal dest constructor (heuristically)
      extends <- "class"
    } else {
      extends <- ""
    }
  } else if (src_type == "s4method") {
    generic <- as.character(src$value@generic)
    class <- sig2class(src$value@defined)
    if (dest_type == "s4generic") {
      # TODO must test whether src method fits dest generic
      extends <- "generic"
    } else if (dest_type == "s4class") {
      extends <- "class"
      # TODO must test whether src method fits dest constructor
    } else {
      extends <- ""
    }
  } else {
    generic <- ""
    class <- ""
    extends <- ""
  }
  list(extends = extends, generic = generic, class = class)
}

# Turn S4 signature into a string
sig2class <- function(sig) {
  if (length(sig) == 1) {
    as.character(sig)
  } else {
    paste0(names(sig), " = ", sig, collapse = ",")
  }
}

# Is destination is probably constructor for src?
fits_constructor <- function(dest_name, src) {
  src_class <- attr(src$value, "s3method")[2]

  # simple case where class name is the same as the constructor name
  if (src_class == dest_name) {
    return(TRUE)
  }

  # more complex case where class name = package name + constructor name
  evalenv <- roxy_meta_get("env") %||% parent.frame() # needed for tests
  pkg_name <- utils::packageName(evalenv) %||% ""

  src_class == paste0(pkg_name, "_", dest_name)
}
