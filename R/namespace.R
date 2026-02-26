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
#' @export
#' @seealso [tags-namespace] for tags that generate `NAMESPACE` directives.
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
roclet_process.roclet_namespace <- function(x, blocks, env, base_path) {
  warn_missing_s3_exports(blocks, env)

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

# NAMESPACE updates -------------------------------------------------------

import_directives <- c(
  "import",
  "importFrom",
  "importClassesFrom",
  "importMethodsFrom",
  "useDynLib"
)

update_namespace_imports <- function(base_path) {
  NAMESPACE <- file.path(base_path, "NAMESPACE")
  if (!made_by_roxygen(NAMESPACE) || !file.exists(NAMESPACE)) {
    return(invisible())
  }

  lines <- c(namespace_imports(base_path), namespace_exports(NAMESPACE))
  results <- c(made_by("#"), sort_c(unique(trimws(lines))))
  write_if_different(NAMESPACE, results, check = TRUE)

  invisible()
}

# Here we hand roll parsing and tokenisation from roxygen2 primitives so
# we can filter tags that we know don't require package code.
namespace_imports <- function(base_path = ".") {
  paths <- package_files(base_path)
  parsed <- lapply(paths, parse, keep.source = TRUE)
  srcrefs <- lapply(parsed, utils::getSrcref)
  blocks <- unlist(lapply(srcrefs, namespace_imports_blocks), recursive = FALSE)

  blocks_to_ns(blocks, emptyenv())
}

namespace_imports_blocks <- function(srcref) {
  comment_refs <- comments(srcref)
  tokens <- lapply(comment_refs, tokenise_ref)

  import_tags <- c(import_directives, "rawNamespace")
  tokens_filtered <- lapply(tokens, function(tokens) {
    tokens[map_lgl(tokens, \(x) x$tag %in% import_tags)]
  })

  compact(lapply(tokens_filtered, function(tokens) {
    block_create(
      call = NULL,
      srcref = srcref(srcfile("NAMESPACE"), rep(1, 4)),
      tokens = tokens
    )
  }))
}

# NB: this is designed as the conjugate of namespace_imports(), so also
#   includes @rawNamespace entries which may/may not also include import directives.
namespace_exports <- function(path) {
  parsed <- as.list(parse(path, keep.source = TRUE))

  is_import_directive <- function(x) is_call(x, import_directives)
  export_lines <- attr(parsed, "srcref")[!map_lgl(parsed, is_import_directive)]
  # Each multiline directives are a single element so they're sorted correctly
  unlist(lapply(export_lines, \(x) paste(as.character(x), collapse = "\n")))
}

# NAMESPACE generation ----------------------------------------------------

blocks_to_ns <- function(blocks, env) {
  lines <- map(blocks, block_to_ns, env = env)
  lines <- unlist(lines) %||% character()

  sort_c(unique(lines))
}

block_to_ns <- function(block, env) {
  map(block$tags, roxy_tag_ns, block = block, env = env)
}

# Namespace tag methods ---------------------------------------------------

roxy_tag_ns <- function(x, block, env) {
  UseMethod("roxy_tag_ns")
}

#' @export
roxy_tag_ns.default <- function(x, block, env) {}

#' @export
roxy_tag_parse.roxy_tag_evalNamespace <- function(x) {
  tag_code(x)
}
#' @export
roxy_tag_ns.roxy_tag_evalNamespace <- function(x, block, env) {
  roxy_tag_eval(x, env)
}

#' @export
roxy_tag_parse.roxy_tag_export <- function(x) {
  tag_words_line(x)
}
#' @export
roxy_tag_ns.roxy_tag_export <- function(x, block, env) {
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
roxy_tag_ns.roxy_tag_exportClass <- function(x, block, env) {
  export_class(x$val)
}

#' @export
roxy_tag_parse.roxy_tag_exportMethod <- function(x) {
  tag_words(x, min = 1)
}
#' @export
roxy_tag_ns.roxy_tag_exportMethod <- function(x, block, env) {
  export_s4_method(x$val)
}

#' @export
roxy_tag_parse.roxy_tag_exportPattern <- function(x) {
  tag_words(x, min = 1)
}
#' @export
roxy_tag_ns.roxy_tag_exportPattern <- function(x, block, env) {
  one_per_line("exportPattern", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_exportS3Method <- function(x) {
  tag_words(x, min = 0, max = 2)
}
#' @export
roxy_tag_ns.roxy_tag_exportS3Method <- function(x, block, env) {
  obj <- block$object

  if (identical(x$val, "NULL")) {
    return()
  }

  if (identical(x$val, "")) {
    if (!inherits(obj, "s3method")) {
      warn_roxy_tag(x, "must be used with an known S3 method")
      return()
    }

    method <- attr(obj$value, "s3method")
  } else if (length(x$val) == 1) {
    if (!inherits(obj, "function") && !inherits(obj, "s3method")) {
      warn_roxy_tag(x, "must be used with a function")
      return()
    }

    if (!str_detect(x$val, "::")) {
      warn_roxy_tag(x, "must have form package::generic")
      return()
    }

    generic <- str_split(x$val, "::")[[1]]
    generic_re <- paste0("^", generic[[2]], "\\.")

    if (!str_detect(obj$alias, generic_re)) {
      warn_roxy_tag(
        x,
        "generic ({.str {generic[[2]]}}) doesn't match function ({.str {obj$alias}})",
      )
      return()
    }

    class <- str_remove(obj$alias, generic_re)
    method <- c(x$val, class)
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
roxy_tag_ns.roxy_tag_import <- function(x, block, env) {
  one_per_line_ignore_current("import", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importClassesFrom <- function(x) {
  tag_words(x, min = 2)
}
#' @export
roxy_tag_ns.roxy_tag_importClassesFrom <- function(x, block, env) {
  repeat_first_ignore_current("importClassesFrom", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importFrom <- function(x) {
  tag_words(x, min = 2)
}
#' @export
roxy_tag_ns.roxy_tag_importFrom <- function(x, block, env) {
  pkg <- x$val[1L]
  if (requireNamespace(pkg, quietly = TRUE)) {
    importing <- x$val[-1L]
    # be sure to match '%>%', `%>%`, "%>%" all to %>% given by getNamespaceExports, #1570
    unknown_idx <- !strip_quotes(importing) %in% getNamespaceExports(pkg)
    if (any(unknown_idx)) {
      warn_roxy_tag(
        x,
        "Excluding unknown {cli::qty(sum(unknown_idx))} export{?s} from {.package {pkg}}: {.code {importing[unknown_idx]}}"
      )
      if (all(unknown_idx)) {
        return(NULL)
      }
      x$val <- c(pkg, importing[!unknown_idx])
    }
  }
  repeat_first_ignore_current("importFrom", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importMethodsFrom <- function(x) {
  tag_words(x, min = 2)
}
#' @export
roxy_tag_ns.roxy_tag_importMethodsFrom <- function(x, block, env) {
  repeat_first_ignore_current("importMethodsFrom", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_rawNamespace <- function(x) {
  tag_code(x)
}
#' @export
roxy_tag_ns.roxy_tag_rawNamespace <- function(x, block, env) {
  x$raw
}

#' @export
roxy_tag_parse.roxy_tag_useDynLib <- function(x) {
  tag_words(x, min = 1)
}
#' @export
roxy_tag_ns.roxy_tag_useDynLib <- function(x, block, env) {
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

default_export <- function(x, block) {
  UseMethod("default_export")
}
#' @export
default_export.s4class <- function(x, block) {
  c(
    if (!is.null(block$object$alias)) export(block$object$alias),
    export_class(x$value@className)
  )
}
#' @export
default_export.s4generic <- function(x, block) export(x$value@generic)
#' @export
default_export.s4method <- function(x, block) export_s4_method(x$value@generic)
#' @export
default_export.s3method <- function(x, block) {
  export_s3_method(auto_quote(attr(x$value, "s3method")))
}
#' @export
default_export.rcclass <- function(x, block) export_class(x$value@className)
#' @export
default_export.default <- function(x, block) export(x$alias)
#' @export
default_export.NULL <- function(x, block) {
  export(block_get_tag_value(block, "name"))
}

# Helpers -----------------------------------------------------------------

export <- function(x) one_per_line("export", x)
export_class <- function(x) one_per_line("exportClasses", x)
export_s4_method <- function(x) one_per_line("exportMethods", x)
export_s3_method <- function(x) {
  args <- paste0(x, collapse = ",")
  paste0("S3method(", args, ")")
}

one_per_line <- function(name, x) {
  if (length(x)) {
    paste0(name, "(", auto_quote(x), ")")
  } else {
    NULL
  }
}
repeat_first <- function(name, x) {
  paste0(name, "(", auto_quote(x[1]), ",", auto_quote(x[-1]), ")")
}

one_per_line_ignore_current <- function(name, x) {
  current <- peek_roxygen_pkg()

  # Ignore any occurrence of `current` inside `x`
  if (is_string(current)) {
    x <- x[x != current]
  }

  one_per_line(name, x)
}
repeat_first_ignore_current <- function(name, x) {
  current <- peek_roxygen_pkg()

  # Ignore the whole command if "first" is `current`
  if (is_string(current) && length(x) && x[[1]] == current) {
    NULL
  } else {
    repeat_first(name, x)
  }
}

# missing s3 exports ------------------------------------------------------

warn_missing_s3_exports <- function(blocks, env) {
  objs <- as.list(env)
  funs <- Filter(is.function, objs)
  methods <- funs[map_lgl(names(funs), is_s3_method, env = env)]

  s3blocks <- blocks[map_lgl(
    blocks,
    block_has_tags,
    c("export", "exportS3Method")
  )]
  s3objects <- map(blocks, \(block) block$object$value)
  s3functions <- Filter(is.function, s3objects)

  undocumented <- methods[!methods %in% s3functions]
  srcrefs <- map(undocumented, attr, "srcref")

  map2(undocumented, names(undocumented), function(fun, name) {
    warn_roxy_function(
      fun,
      "S3 method {.arg {name}} needs @export or @exportS3Method tag"
    )
  })
}
