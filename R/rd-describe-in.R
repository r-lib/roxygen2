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
  metadata <- build_minidesc_metadata(src = block$object, dest = dest)

  topic$add(rlang::exec(rd_section_minidesc, !!!c(
    name = block$object$topic,
    desc = tag$val$description,
    metadata
  )))
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
merge.rd_section_minidesc <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  rd_section(
    "minidesc",
    rbind(x$value, y$value, stringsAsFactors = FALSE)
  )
}

# Rd Output -------------------------------------------------------------------

#' @export
format.rd_section_minidesc <- function(x, ...) {
  section_title <- "Related Functions and Methods"
  
  df <- x$value
  subsection_by <- unique(df[, "extends"])
  section_body <- purrr::map_chr(subsection_by, format_subsections, df)
  
  paste0(
    "\\section{", section_title, "}{\n",
    paste0(section_body, collapse = "\n"),
    "}\n"
  )
}

format_subsections <- function(extends, df) {
  class <- unique(df[df$extends == extends, "class"])
  generic <- unique(df[df$extends == extends, "generic"])
  subsection_title <- switch(
    extends,
    class = paste0(
      "Methods extending \\code{", escape(class), "} class (by generic):"
    ),
    generic = paste0(
      "Methods extending \\code{", escape(generic), "} generic (by class):"
    ),
    "Functions"
  )

  list_by <- unique(df[df$extends == extends, , drop = FALSE])
  subsection_body <- purrr::pmap_chr(list_by, format_subsection)
  subsection_body <- paste0(subsection_body, collapse = "\n")

  paste0(
    "\\subsection{", subsection_title, "}{\n", subsection_body, "}",
    collapse = "\n"
  )
}

format_subsection <- function(name, desc, extends, generic, class) {
  label <- switch(
    extends,
    "class" = generic,
    "generic" = class,
    name
  )
  paste0(
  "\\itemize{\n",
    paste0(
      "\\item \\code{",
      escape(label),
      "}: ", desc,
      collapse = "\n"
    ),
    "\n}"
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

#' Build metadata for how to present `@describeIn` tag
#' @return list of character scalars named `extends`, `generic`, `class`.
#' See rd_section_minidesc() for details.
#' @noRd
build_minidesc_metadata <- function(src, dest) {
  src_type <- class(src)[1]
  dest_type <- class(dest)[1]
  dest_name <- as.character(dest$topic)

  # fallback defaults
  extends <- class <- generic <- ""
  
  if (src_type == "s3method") {
    generic <- attr(src$value, "s3method")[1]
    class <- attr(src$value, "s3method")[2]
    if (dest_type == "s3generic" && generic == dest_name) {
      # src method fits dest generic
      extends <- "generic"
    } else if (fits_constructor(dest_name, src)) {
      # src method fits informal dest constructor (heuristically)
      extends <- "class"
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
    }
  }
  list(extends = extends, generic = generic, class = class)
}

#' Turn S4 signatures into a character scalar
#' @noRd
sig2class <- function(sig) {
  if (length(sig) == 1) {
    class <- sig
  } else {
    class <- paste0(names(sig), " = ", sig, collapse = ",")
  }
  as.character(class)
}

#' Tests if destination is a constructor for class of src
#' 
#' No formal test is possible, these are heuristics.
#' @noRd
fits_constructor <- function(dest_name, src) {
  src_class <- attr(src$value, "s3method")[2]
  # assuming that dest is class constructor, when class == dest name
  if (src_class == dest_name) TRUE
  else fits_constructor_disambig_class(src_class, dest_name)
}

#' Test if class of src is a disambiguated version of dest
#' 
#' Tests if `dest_name` and pkg are in `src_class`.
#' 
#' [Advanced R](https://adv-r.hadley.nz/s3.html#s3-classes) recommends to add 
#' the pkg name to the class name, to avoid namespace clashes.
#' In this case, the actual class may be called "pkg_baz", but the exported 
#' "S3 constructor" may still called "pkg::baz()", because "pkg::pkg_baz()"
#' would be redundant.
#' This function checks for these kinds of heuristic matches between src_class
#' and dest_name.
#' @noRd
fits_constructor_disambig_class <- function(src_class, dest_name, pkg_name) {
  evalenv <- roxy_meta_get("env")
  # This should only happen in our test cases
  if (is.null(evalenv)) evalenv <- parent.frame()
  stringr::str_detect(src_class, dest_name) &&
  stringr::str_detect(src_class, utils::packageName(env = evalenv))
}
