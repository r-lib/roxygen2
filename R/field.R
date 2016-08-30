roxy_field_simple <- function(field, values) {
  if (is.null(values) || identical(values, "NULL")) {
    # NULL is special sentinel value that suppresses output of that field
    return()
  }

  roxy_field(field, values = values)
}

# Low level constructor that doesn't impose any structure on the values
roxy_field <- function(field, ...) {

  structure(
    list(
      field = field,
      ...
    ),
    class = c(paste0("roxy_field_", field), "roxy_field")
  )
}

is_roxy_field <- function(x) inherits(x, "roxy_field")

#' @export
print.roxy_field <- function(x, ...) {
  cat(format(x), "\n")
}

#' @export
format.roxy_field <- function(x, ...) {
  paste0("[ ", x$field, " FIELD ]\n")
}

#' @export
merge.roxy_field <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  roxy_field_simple(x$field, c(x$values, y$values))
}


# Comment fields -----------------------------------------------------------------------

#' @export
format.roxy_field_backref <- function(x, ...) {
  filename <- unique(x$values)
  filename <- file.path(basename(dirname(filename)), basename(filename), fsep = "/")
  sprintf("%% Please edit documentation in %s\n", paste(filename, collapse = ", "))
}

# Fields that repeat multiple times --------------------------------------------

format_rd <- function(x, ...) {
  vapply(sort_c(unique(x$values)), rd_macro, field = x$field,
    FUN.VALUE = character(1), USE.NAMES = FALSE)
}
#' @export
format.roxy_field_keyword <- format_rd
#' @export
format.roxy_field_alias <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_rd(x, ...)
}

# Fields that keep the first occurence -----------------------------------------
format_first <- function(x, ...) {
  rd_macro(x$field, x$values[1])
}
#' @export
format.roxy_field_name <- function(x, ...) {
  x$values <- str_replace_all(x$values, fixed("%"), "\\%")
  format_first(x, ...)
}
#' @export
format.roxy_field_title <- format_first
#' @export
format.roxy_field_docType <- format_first
#' @export
format.roxy_field_format <- format_first
#' @export
format.roxy_field_encoding <- format_first

# Fields collapse their values into a single string ----------------------------

format_collapse <- function(x, ..., indent = 0, exdent = 0, wrap = TRUE) {
  values <- paste0(x$values, collapse = "\n\n")
  if (wrap) {
    values <- str_wrap(values, width = 60, indent = indent, exdent = exdent)
  }
  rd_macro(x$field, values, space = TRUE)
}
#' @export
format.roxy_field_author <- format_collapse
#' @export
format.roxy_field_concept <- format_collapse
#' @export
format.roxy_field_description <- format_collapse
#' @export
format.roxy_field_details <- format_collapse
#' @export
format.roxy_field_note <- format_collapse
#' @export
format.roxy_field_references <- format_collapse
#' @export
format.roxy_field_seealso <- format_collapse
#' @export
format.roxy_field_source <- format_collapse
#' @export
format.roxy_field_value <- format_collapse

# Fields that don't have output ------------------------------------------------

format_null <- function(x, ...) NULL

#' @export
format.roxy_field_family <- format_null
#' @export
format.roxy_field_inheritParams <- format_null
#' @export
format.roxy_field_formals <- format_null

# Fields with special errors or other semantics --------------------------------

#' @export
format.roxy_field_usage <- function(x, ...) {
  rd_macro(x$field, build_rd(x$values, collapse = "\n\n"), space = TRUE)
}

#' @export
format.roxy_field_param <- function(x, ..., wrap = TRUE) {
  names <- names(x$values)

  # add space to multiple arguments so they can wrap
  names <- gsub(",", ", ", names)

  items <- paste0("\\item{", names, "}{", x$values, "}", collapse = "\n\n")
  if (wrap) {
    items <- str_wrap(items, width = 60, exdent = 2, indent = 2)
  }

  rd_macro("arguments", items, space = TRUE)
}

#' @export
format.roxy_field_slot <- function(x, ...) {
  describe_section("Slots", names(x$values), x$values)
}

#' @export
format.roxy_field_field <- function(x, ...) {
  describe_section("Fields", names(x$values), x$values)
}

describe_section <- function(name, dt, dd) {
  if (length(dt) == 0) return("")

  items <- paste0("\\item{\\code{", dt, "}}{", dd, "}", collapse = "\n\n")
  paste0("\\section{", name, "}{\n\n",
    "\\describe{\n",
    items,
    "\n}}\n"
  )
}

#' @export
format.roxy_field_examples <- function(x, ...) {
  values <- paste0(x$values, collapse = "\n")
  rd_macro(x$field, values, space = TRUE)
}

#' @export
format.roxy_field_rcmethods <- function(x, ...) {
  describe_section("Methods", names(x$values), x$values)
}

#' @export
format.roxy_field_rawRd <- function(x, ...) {
  paste(x$values, collapse = "\n")
}



# Minidesc ----------------------------------------------------------------

roxy_field_minidesc <- function(type, label, desc) {
  stopifnot(is.character(type), is.character(label), is.character(desc))
  stopifnot(length(desc) == length(label))

  roxy_field("minidesc", type = type, desc = desc, label = label)
}

#' @export
merge.roxy_field_minidesc <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  stopifnot(identical(x$type, y$type))
  roxy_field_minidesc(
    x$type,
    label = c(x$label, y$label),
    desc = c(x$desc, y$desc)
  )
}

#' @export
format.roxy_field_minidesc <- function(x, ...) {
  title <- switch(x$type,
    generic = "Methods (by class)",
    class = "Methods (by generic)",
    "function" = "Functions"
  )

  paste0(
    "\\section{", title, "}{\n",
    "\\itemize{\n",
    paste0("\\item \\code{", escape(x$label), "}: ", x$desc,
      collapse = "\n\n"),
    "\n}}\n"
  )
}

# Re-export ----------------------------------------------------------------

roxy_field_reexport <- function(pkg, fun) {
  stopifnot(is.character(pkg), is.character(fun))
  stopifnot(length(pkg) == length(fun))

  roxy_field("reexport", pkg = pkg, fun = fun)
}

#' @export
merge.roxy_field_reexport <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  roxy_field_reexport(c(x$pkg, y$pkg), c(x$fun, y$fun))
}

#' @export
format.roxy_field_reexport <- function(x, ...) {
  pkgs <- split(x$fun, x$pkg)
  pkg_links <- Map(pkg = names(pkgs), funs = pkgs, function(pkg, funs) {
    links <- paste0("\\code{\\link[", pkg, "]{", escape(funs), "}}",
      collapse = ", ")
    paste0("\\item{", pkg, "}{", links, "}")
  })

  paste0(
    "\\description{\n",
    "These objects are imported from other packages. Follow the links\n",
    "below to see their documentation.\n",
    "\n",
    "\\describe{\n",
    paste0("  ", unlist(pkg_links), collapse = "\n\n"),
    "\n}}\n"
  )
}


# Sections ----------------------------------------------------------------

roxy_field_section <- function(title, content) {
  stopifnot(is.character(title), is.character(content))
  stopifnot(length(title) == length(content))

  roxy_field("section", title = title, content = content)
}

#' @export
format.roxy_field_section <- function(x, ..., wrap = TRUE) {
  if (wrap) {
    content <- str_wrap(str_trim(x$content), width = 60, exdent = 2, indent = 2)
  } else {
    content <- x$content
  }

  paste0("\\section{", x$title, "}{\n", content, "\n}\n", collapse = "\n")
}

#' @export
merge.roxy_field_section <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))

  title <- c(x$title, y$title)
  content <- c(x$content, y$content)

  dedup <- tapply(content, title, paste, collapse = "\n\n")
  # tapply orders alphabetically, so reorder to match original order
  dedup <- dedup[unique(title)]

  roxy_field_section(names(dedup), as.vector(dedup))
}
