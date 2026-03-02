#' @export
roxy_tag_parse.roxy_tag_describeIn <- function(x) {
  if (!is.na(x$raw) && !str_detect(x$raw, "[[:space:]]+")) {
    warn_roxy_tag(
      x,
      c(
        "requires a name and description",
        i = "Did you want @rdname instead?"
      )
    )
    NULL
  } else {
    tag_two_part(x, "a topic name", "a description")
  }
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
  if (is.null(object_name(block$object))) {
    warn_roxy_tag(tag, "not supported with this object type")
    return()
  }

  dest <- find_object(tag$val$name, env)
  metadata <- build_minidesc_metadata(block$object, dest)

  topic$add(rd_section_minidesc(
    name = object_name(block$object),
    desc = tag$val$description,
    extends = metadata$extends,
    generic = metadata$generic,
    class = metadata$class
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
rd_section_minidesc <- function(
  name,
  desc,
  extends = c("", "generic", "class"),
  generic = "",
  class = ""
) {
  stopifnot(is_string(name))
  stopifnot(is_character(desc))
  rlang::arg_match(extends)
  stopifnot(is_string(generic))
  stopifnot(is_string(class))

  data <- data.frame(
    name = name,
    desc = desc,
    extends = extends,
    generic = generic,
    class = class
  )
  rd_section("minidesc", data)
}

#' @export
merge.rd_section_minidesc <- function(x, y, ..., block) {
  stopifnot(identical(class(x), class(y)))

  rd_section("minidesc", rbind(x$value, y$value))
}

# Rd Output -------------------------------------------------------------------

#' @export
format.rd_section_minidesc <- function(x, ...) {
  order <- intersect(c("generic", "class", ""), unique(x$value$extends))
  by <- factor(x$value$extends, levels = order)
  subsections <- split(x$value, by)
  body <- purrr::map2_chr(subsections, names(subsections), format_section)

  paste0(body, collapse = "\n")
}

format_section <- function(df, type) {
  title <- switch(
    type,
    class = "Methods (by generic)",
    generic = "Methods (by class)",
    "Functions"
  )

  bullets <- paste0("\\code{", df$name, "}: ", df$desc, "\n")
  body <- paste0(
    "\\itemize{\n",
    paste0("\\item ", bullets, "\n", collapse = ""),
    "}"
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


object_name <- function(x) {
  UseMethod("object_name")
}
#' @export
object_name.default <- function(x) {
  x$alias
}
#' @export
object_name.function <- function(x) {
  object_name_fun(x$alias, x)
}
#' @export
object_name.s3generic <- object_name.function
#' @export
object_name.s3method <- function(x) {
  method <- attr(x$value, "s3method")
  as.character(function_usage(method[[1]], list(as.name(method[[2]]))))
  #
  #   name <- paste(, collapse = ".")
  #   object_name_fun(name, x)
}
#' @export
object_name.s4generic <- function(x) {
  object_name_fun(x$value@generic, x)
}
#' @export
object_name.s4method <- function(x) {
  classes <- lapply(x$value@defined, as.name)
  if (length(classes) == 1) {
    names(classes) <- NULL
  }
  as.character(function_usage(x$value@generic, classes))
}

object_name_fun <- function(name, x, format_name = identity) {
  if (is_replacement_fun(name) || is_infix_fun(name)) {
    args <- formals(x$value)
  } else {
    args <- NULL
  }

  as.character(function_usage(name, args, format_name))
}
