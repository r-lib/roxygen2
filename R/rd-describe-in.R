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
    df = subsection(label = label$label, desc = tag$val$description),
    method = label$method,
    generic = label$generic
  ))
  dest$topic
}

# Field -------------------------------------------------------------------

#' Record data for minidescription sections from `@describeIn`
#' @param df a dataframe as created by [subsection()].
#' @param method giving whether method generic a generic (S3 or S4)
#' @param generic Name of the generic in the destination the method generic, otherwise empty string (`""`).
#' @noRd
rd_section_minidesc <- function(df, method = FALSE, generic = "") {
  stopifnot(is_scalar_logical(method))
  stopifnot(is_scalar_character(generic))
  data <- cbind(
    method = method,
    generic = generic,
    df, 
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

#' @export
format.rd_section_minidesc <- function(x, ...) {
  section_title <- "Related Functions and Methods"

  groups <- unique(x$value[, c("method", "generic")])
  out <- purrr::map2_chr(groups$method, groups$generic, function(method, generic) {
    if (method) {
      subsection_title <- "Methods extending"
      if (generic == "") {
        subsection_title <- paste0(subsection_title, " other generics (by generic):")
      } else {
        subsection_title <- paste0(
          subsection_title,
          " \\code{",
          escape(generic),
          "} generic (by class):"
        )
      }
    } else {
      subsection_title <- "Functions"
    }
    df <- x$value
    subsection_body <- format_subsection(
      df[df$method == method & df$generic == generic, c("label", "desc")]
    )
    paste0(
      "\\subsection{", subsection_title, "}{\n",
      subsection_body,
      "}\n"
    )
  })
  paste0(
    "\\section{", section_title, "}{\n",
    paste0(out, collapse = "\n"),
    "}\n"
  )
}

subsection <- function(label, desc) {
  stopifnot(is_character(label), is_character(desc))
  stopifnot(length(label) == length(desc))
  data.frame(
    stringsAsFactors = FALSE,
    label = label,
    desc = desc
  )
}

format_subsection <- function(df) {
  paste0(
    "\\itemize{\n",
    paste0(
      "\\item \\code{",
      escape(df$label),
      "}: ", df$desc,
      collapse = "\n"
    ),
    "\n}\n"
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
  dest_name <- as.character(dest$topic)

  # default to fallback: function + topic
  method <- FALSE
  generic <- ""
  label <- src$topic

  if (src_type == "s3method") {
    if (dest_type == "s3generic" && attr(src$value, "s3method")[1] == dest_name) {
      # applies only to those methods which actually extend the generic in dest
      method <- TRUE
      generic <- dest_name
      label <- attr(src$value, "s3method")[2]
    } else if (attr(src$value, "s3method")[2] == dest_name) {
      # assuming that dest is class constructor, when class == dest name
      # no formal check for S3 constructor is possible
      method <- TRUE
      generic <- ""
      label <- attr(src$value, "s3method")[1]
    }
  } else if (src_type == "s4method") {
    if (dest_type == "s4class") {
      # Label S4 methods in class with their generic
      method <- TRUE
      generic <- ""
      label <- src$value@generic
    } else if (dest_type == "s4generic") {
      # Label S4 methods in generic with their signature
      method <- TRUE
      generic <- dest_name
      sig <- src$value@defined
      if (length(sig) == 1) {
        label <- sig
      } else {
        label <- paste0(names(sig), " = ", sig, collapse = ",")
      }
    }
  }
  label <-  as.character(label)

  list(method = method, generic = generic, label = label)
}
