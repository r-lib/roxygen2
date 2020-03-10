#' Roclet: make `NAMESPACE`
#'
#' @description
#' This roclet automates the production of a `NAMESPACE` file, which controls
#' the functions imported and exported by your package, as described in
#' [Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html).
#'
#' The `NAMESPACE` is generated in two passes: the first generates only
#' import directives (because this can be computed without evaluating package
#' code), and the second generates everything (after the package has been
#' loaded).
#'
#' See `vignette("namespace")` for details.
#'
#' @family roclets
#' @export
#' @examples
#' # The most common namespace tag is @@export, which declares that a function
#' # is part of the external interface of your package
#' #' @export
#' foofy <- function(x, y, z) {
#' }
#'
#' # You'll also often find global imports living in a file called
#' # R/{package}-package.R.
#' #' @@importFrom magrittr %>%
#' #' @@import rlang
#' NULL
namespace_roclet <- function() {
  roclet("namespace")
}

#' @export
roclet_preprocess.roclet_namespace <- function(x, blocks, base_path) {
  lines <- blocks_to_ns(blocks, emptyenv(), import_only = TRUE)
  NAMESPACE <- file.path(base_path, "NAMESPACE")

  if (length(lines) == 0 && !made_by_roxygen(NAMESPACE)) {
    return(x)
  }

  results <- c(made_by("#"), lines)
  write_if_different(NAMESPACE, results, check = TRUE)

  invisible(x)
}

#' @export
roclet_process.roclet_namespace <- function(x, blocks, env, base_path) {
  blocks_to_ns(blocks, env)
}

#' @export
roclet_output.roclet_namespace <- function(x, results, base_path, ...) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  results <- c(made_by("#"), results)

  # Always check for roxygen2 header before overwriting NAMESPACE (#436),
  # even when running for the first time
  write_if_different(NAMESPACE, results, check = TRUE)

  NAMESPACE
}

#' @export
roclet_clean.roclet_namespace <- function(x, base_path) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  if (made_by_roxygen(NAMESPACE)) {
    unlink(NAMESPACE)
  }
}

# NAMESPACE generation ----------------------------------------------------

blocks_to_ns <- function(blocks, env, import_only = FALSE) {
  lines <- map(blocks, block_to_ns, env = env, import_only = import_only)
  lines <- unlist(lines) %||% character()

  sort_c(unique(lines))
}

block_to_ns <- function(block, env, import_only = FALSE) {
  map(block$tags, roxy_tag_ns, block = block, env = env, import_only = import_only)
}

# Namespace tag methods ---------------------------------------------------

roxy_tag_ns <- function(x, block, env, import_only = FALSE) {
  UseMethod("roxy_tag_ns")
}

#' @export
roxy_tag_ns.default <- function(x, block, env, import_only = FALSE) {

}

#' @export
roxy_tag_parse.roxy_tag_evalNamespace <- function(x) {
  tag_code(x)
}
#' @export
roxy_tag_ns.roxy_tag_evalNamespace <- function(x, block, env, import_only = FALSE) {
  if (import_only) {
    return()
  }

  roxy_tag_eval(x, env)
}

#' @export
roxy_tag_parse.roxy_tag_export <- function(x) {
  tag_words_line(x)
}
#' @export
roxy_tag_ns.roxy_tag_export <- function(x, block, env, import_only = FALSE) {
  if (import_only) {
    return()
  }

  if (identical(x$val, "")) {
    # FIXME: check for empty exports (i.e. no name)
    default_export(block$object, block)
  } else {
    export(x$val)
  }
}

#' @export
roxy_tag_parse.roxy_tag_exportClass <- function(x) {
  tag_words(x, 1)
}
#' @export
roxy_tag_ns.roxy_tag_exportClass <- function(x, block, env, import_only = FALSE) {
  if (import_only) {
    return()
  }

  export_class(x$val)
}

#' @export
roxy_tag_parse.roxy_tag_exportMethod <- function(x) {
  tag_words(x, min = 1)
}
#' @export
roxy_tag_ns.roxy_tag_exportMethod <- function(x, block, env, import_only = FALSE) {
  if (import_only) {
    return()
  }
  export_s4_method(x$val)
}

#' @export
roxy_tag_parse.roxy_tag_exportPattern <- function(x) {
  tag_words(x, min = 1)
}
#' @export
roxy_tag_ns.roxy_tag_exportPattern <- function(x, block, env, import_only = FALSE) {
  if (import_only) {
    return()
  }
  one_per_line("exportPattern", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_exportS3Method <- function(x) {
  tag_words(x, min = 0, max = 2)
}
#' @export
roxy_tag_ns.roxy_tag_exportS3Method <- function(x, block, env, import_only = FALSE) {
  if (import_only) {
    return()
  }

  obj <- block$object
  if (length(x$val) < 2 && !inherits(obj, "s3method")) {
    roxy_tag_warning(x,
      "`@exportS3Method` and `@exportS3Method generic` must be used with an S3 method"
    )
    return()
  }

  if (identical(x$val, "")) {
    method <- attr(obj$value, "s3method")
  } else if (length(x$val) == 1) {
    method <- c(x$val, attr(obj$value, "s3method")[[2]])
  } else {
    method <- x$val
  }

  export_s3_method(method)
}

#' @export
roxy_tag_parse.roxy_tag_import <- function(x) {
  tag_words(x, min = 1)
}
#' @export
roxy_tag_ns.roxy_tag_import <- function(x, block, env, import_only = FALSE) {
  one_per_line("import", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importClassesFrom <- function(x) {
  tag_words(x, min = 2)
}
#' @export
roxy_tag_ns.roxy_tag_importClassesFrom <- function(x, block, env, import_only = FALSE) {
  repeat_first("importClassesFrom", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importFrom <- function(x) {
  tag_words(x, min = 2)
}
#' @export
roxy_tag_ns.roxy_tag_importFrom <- function(x, block, env, import_only = FALSE) {
  repeat_first("importFrom", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importMethodsFrom <- function(x) {
  tag_words(x, min = 2)
}
#' @export
roxy_tag_ns.roxy_tag_importMethodsFrom <- function(x, block, env, import_only = FALSE) {
  repeat_first("importMethodsFrom", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_rawNamespace <- function(x) {
  tag_code(x)
}
#' @export
roxy_tag_ns.roxy_tag_rawNamespace  <- function(x, block, env, import_only = FALSE) {
  x$val
}

#' @export
roxy_tag_parse.roxy_tag_useDynLib <- function(x) {
  tag_words(x, min = 1)
}
#' @export
roxy_tag_ns.roxy_tag_useDynLib <- function(x, block, env, import_only = FALSE) {
  if (length(x$val) == 1) {
    return(paste0("useDynLib(", auto_quote(x$val), ")"))
  }

  if (any(grepl(",", x$val))) {
    # If there's a comma in list, don't quote output. This makes it possible
    # for roxygen2 to support other NAMESPACE forms not otherwise mapped
    args <- paste0(x$val, collapse = " ")
    paste0("useDynLib(", args, ")")
  } else {
    repeat_first("useDynLib", x$val)
  }
}

# Default export methods --------------------------------------------------

default_export <- function(x, block) UseMethod("default_export")
#' @export
default_export.s4class   <- function(x, block) export_class(x$value@className)
#' @export
default_export.s4generic <- function(x, block) export(x$value@generic)
#' @export
default_export.s4method  <- function(x, block) export_s4_method(x$value@generic)
#' @export
default_export.s3method  <- function(x, block) export_s3_method(auto_quote(attr(x$value, "s3method")))
#' @export
default_export.rcclass   <- function(x, block) export_class(x$value@className)
#' @export
default_export.default   <- function(x, block) export(x$alias)
#' @export
default_export.NULL      <- function(x, block) export(block_get_tag_value(block, "name"))

# Helpers -----------------------------------------------------------------

export           <- function(x) one_per_line("export", x)
export_class     <- function(x) one_per_line("exportClasses", x)
export_s4_method <- function(x) one_per_line("exportMethods", x)
export_s3_method <- function(x) {
  args <- paste0(x, collapse = ",")
  paste0("S3method(", args, ")")
}

one_per_line <- function(name, x) {
  paste0(name, "(", auto_quote(x), ")")
}
repeat_first <- function(name, x) {
  paste0(name, "(", auto_quote(x[1]), ",", auto_quote(x[-1]), ")")
}
