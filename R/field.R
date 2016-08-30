roxy_field <- function(field, values) {
  if (is.null(values) || identical(values, "NULL")) {
    # NULL is special sentinel value that suppresses output of that field
    return()
  }

  structure(
    list(
      field = field,
      values = values
    ),
    class = c(paste0("roxy_field_", field), "roxy_field")
  )
}

is_roxy_field <- function(x) inherits(x, "roxy_field")

#' @export
print.roxy_field <- function(x, ...) {
  cat(format(x), "\n")
}

# Translate a field and values into an Rd macro.
# Multiple values get their own braces.
rd_macro <- function(field, ..., space = FALSE) {
  if (space) {
    values <- paste0("\n", paste0(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }

  paste0("\\", field, paste0("{", values, "}", collapse = ""), "\n")
}

#' @export
format.roxy_field <- function(x, ...) {
  paste0("[ ", x$field, " FIELD ]\n")
}

#' @export
merge.roxy_field <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  roxy_field(x$field, c(x$values, y$values))
}

#' @export
merge.roxy_field_minidesc <- function(x, y, ...) {
  if (x$values$type != y$values$type) {
    stop("Can't merge @minidesc of different types", call. = FALSE)
  }

  x$values$desc <- c(x$values$desc, y$values$desc)
  x$values$label <- c(x$values$label, y$values$label)
  x
}

#' @export
merge.roxy_field_section <- function(x, y, ...) {
  x_names <- vapply(x$values, `[[`, "name", FUN.VALUE = character(1L))
  y_names <- vapply(y$values, `[[`, "name", FUN.VALUE = character(1L))
  xy_names <- unique(c(x_names, y_names))
  xy_both_names <- intersect(x_names, y_names)
  x_contents <- setNames(lapply(x$values, `[[`, "content"), x_names)
  y_contents <- setNames(lapply(y$values, `[[`, "content"), y_names)
  values <- lapply(
    xy_names,
    function (name) {
      if (name %in% xy_both_names) {
        content <- paste(x_contents[[name]], y_contents[[name]], sep = "\n\n")
      } else {
        content <- x_contents[[name]] %||% y_contents[[name]]
      }

      list(name = name, content = content)
    }
  )
  roxy_field("section", values)
}

#' @export
merge.roxy_field_reexport <- function(x, y, ...) {
  values <- list(
    pkg = c(x$values$pkg, y$values$pkg),
    fun = c(x$values$fun, y$values$fun)
  )
  roxy_field("reexport", values)
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
format.roxy_field_section <- function(x, ..., wrap = TRUE) {
  names <- vapply(x$values, "[[", "name", FUN.VALUE = character(1))

  contents <- vapply(x$values, "[[", "content", FUN.VALUE = character(1))
  if (wrap) {
    contents <- str_wrap(str_trim(contents), width = 60, exdent = 2, indent = 2)
  }

  setions <- paste0("\\section{", names, "}{\n", contents, "\n}\n",
    collapse = "\n")
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
format.roxy_field_minidesc <- function(x, ...) {
  title <- switch(x$values$type,
    generic = "Methods (by class)",
    class = "Methods (by generic)",
    "function" = "Functions"
  )

  paste0(
    "\\section{", title, "}{\n",
    "\\itemize{\n",
    paste0("\\item \\code{", escape(x$values$label), "}: ", x$values$desc,
      collapse = "\n\n"),
    "\n}}\n"
  )
}

#' @export
format.roxy_field_rawRd <- function(x, ...) {
  paste(x$values, collapse = "\n")
}


#' @export
format.roxy_field_reexport <- function(x, ...) {
  pkgs <- split(x$values$fun, x$values$pkg)
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
