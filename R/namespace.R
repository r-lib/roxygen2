#' Roclet: make `NAMESPACE`
#'
#' @description
#' This [roclet] automates the production of a `NAMESPACE` file, which controls
#' the functions imported and exported by your package, as described in
#' [Writing R extensions](https://cran.r-project.org/doc/manuals/r-release/R-exts.html).
#' It is run by default by [roxygenize()].
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
#' # This results in the following line in `NAMESPACE`:
#' # export(foofy)
#'
#' # You'll also often find global imports living in a file called
#' # R/{package}-package.R.
#' #' @importFrom magrittr %>%
#' #' @import rlang
#' NULL
#'
#' # This results in the following lines in `NAMESPACE`:
#' # importFrom(magrittr,"%>%")
#' # importFrom(rlang, <symbols rlang exports>)
#' #
#' # `@import` expands to an `importFrom()` expression so that
#' # the imported set is frozen at document-time. This prevents
#' # load-time conflicts when an updated package now exports new
#' # symbols that happen to conflict with other imported symbols.
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

  directives <- c(
    namespace_imports(base_path),
    as.list(namespace_exports(NAMESPACE))
  )
  results <- c(made_by("#"), ns_format(directives))
  write_if_different(NAMESPACE, results, check = TRUE)

  invisible()
}

# Here we hand roll parsing and tokenisation from roxygen2 primitives so
# we can filter tags that we know don't require package code. Returns the
# unformatted directives (see `block_directives()`) so the caller can merge
# them with the existing exports in `ns_format()`.
namespace_imports <- function(base_path = ".") {
  paths <- package_files(base_path)
  parsed <- lapply(paths, parse, keep.source = TRUE)
  srcrefs <- lapply(parsed, utils::getSrcref)
  # Ensure parse warnings only fire once during the main parse_package() pass
  suppressMessages(
    blocks <- unlist(
      lapply(srcrefs, namespace_imports_blocks),
      recursive = FALSE
    )
  )

  block_directives(blocks, emptyenv())
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
  ns_format(block_directives(blocks, env))
}

block_directives <- function(blocks, env) {
  directives <- map(blocks, function(block) {
    map(block$tags, roxy_tag_ns, block = block, env = env)
  })
  compact(splice_directives(list_c(directives)))
}

# A `roxy_tag_ns()` method usually returns a single directive, but `@import`
# expands to one `importFrom()` per package, so it returns a bare list of
# directives. `import_from()` objects are themselves lists, so we only splice
# unclassed lists, which leaves those objects (and character directives)
# untouched.
splice_directives <- function(directives) {
  list_c(map(directives, \(x) if (is_bare_list(x)) x else list(x)))
}

# `roxy_tag_ns()` returns either a rendered directive (a character vector) or,
# for `@importFrom`, a structured `import_from()` so that all the imports from a
# package can be merged into a single directive here. This dodges a
# `loadNamespace()` performance issue that scales with the number of
# `importFrom()` directives.
ns_format <- function(directives) {
  is_import <- map_lgl(directives, \(x) inherits(x, "import_from"))
  imports <- directives[is_import]
  check_import_conflicts(imports)

  text <- unique(as.character(unlist(
    directives[!is_import],
    use.names = FALSE
  )))
  blocks <- merge_import_from(imports)

  lines <- c(text, unname(blocks))
  lines[order_c(lines)]
}

import_from <- function(package, funs, expanded = FALSE) {
  structure(
    list(package = package, funs = funs, expanded = expanded),
    class = "import_from"
  )
}

# Conflicting `@import` directives are detected at document-time. An error is
# thrown so the user has to resolve the conflict to build the package.
check_import_conflicts <- function(imports) {
  syms <- map(imports, \(x) strip_quotes(x$funs))
  imported <- data.frame(
    sym = unlist(syms, use.names = FALSE) %||% character(),
    pkg = rep(map_chr(imports, \(x) x$package), lengths(syms)),
    expanded = rep(map_lgl(imports, \(x) x$expanded %||% FALSE), lengths(syms))
  )

  # A symbol conflicts when it's imported from more than one package and at
  # least one of those imports came from an expanded `@import`.
  by_sym <- split(imported, imported$sym)
  conflicts <- keep(
    by_sym,
    \(x) length(unique(x$pkg)) > 1 && any(x$expanded)
  )

  # Re-exports aren't real conflicts: when several packages export the same
  # object (e.g. `%>%`), importing it from more than one is harmless.
  conflicts <- discard(conflicts, \(x) is_reexport(x$sym[[1]], unique(x$pkg)))

  if (length(conflicts) == 0) {
    return(invisible())
  }

  bullets <- map_chr(conflicts, function(x) {
    where <- sort_c(unique(x$pkg))
    cli::format_inline("{.code {x$sym[[1]]}} is exported by {.package {where}}")
  })

  conflict <- conflicts[[1]]
  example_sym <- conflict$sym[[1]]
  example_pkg <- conflict$pkg[conflict$expanded][[1]]

  cli::cli_abort(c(
    "Found {length(conflicts)} conflicting import{?s} from {.code @import}.",
    set_names(bullets, rep("*", length(bullets))),
    i = "Exclude unwanted symbols with e.g. {.code @import {example_pkg}, except = {example_sym}}."
  ))
}

# TRUE when every package exports the identical object for `sym`, so importing
# it from more than one of them doesn't actually clash. Returns FALSE if any
# package's value can't be read (e.g. it isn't installed), since we then can't
# prove they match and would rather flag a false conflict than miss a real one.
is_reexport <- function(sym, pkgs) {
  values <- map(pkgs, function(pkg) {
    tryCatch(getExportedValue(pkg, sym), error = function(cnd) NULL)
  })
  # A failed lookup comes back NULL, meaning we can't prove a match
  if (some(values, is.null)) {
    return(FALSE)
  }

  every(values[-1], \(x) identical(x, values[[1]]))
}

# Merge the `import_from()` directives by package into one `importFrom()` each.
merge_import_from <- function(imports) {
  packages <- map_chr(imports, \(x) x$package)
  funs <- split(
    lapply(imports, \(x) x$funs),
    factor(packages, levels = sort_c(unique(packages)))
  )
  blocks <- map_chr(names(funs), function(package) {
    syms <- sort_c(unique(auto_quote(unlist(
      funs[[package]],
      use.names = FALSE
    ))))
    format_import_from(package, syms)
  })
  set_names(blocks, names(funs))
}

format_import_from <- function(package, funs) {
  package <- auto_quote(package)
  if (length(funs) == 1) {
    sprintf("importFrom(%s,%s)", package, funs)
  } else {
    paste0(
      "importFrom(",
      package,
      ",\n",
      paste0("  ", funs, collapse = ",\n"),
      "\n)"
    )
  }
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
  tag_words(x)
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
      warn_roxy_tag(x, "must be used with a known S3 method")
      return()
    }

    method <- attr(obj$value, "s3method")
  } else if (length(x$val) == 1) {
    if (!inherits(obj, "function") && !inherits(obj, "s3method")) {
      warn_roxy_tag(x, "must be used with a function")
      return()
    }

    if (!grepl("::", x$val, fixed = TRUE)) {
      warn_roxy_tag(x, "must have form package::generic")
      return()
    }

    generic <- re_split_half(x$val, "::")
    generic_re <- paste0("^", generic[[2]], "\\.")

    if (!grepl(generic_re, obj$alias)) {
      warn_roxy_tag(
        x,
        "generic ({.str {generic[[2]]}}) doesn't match function ({.str {obj$alias}})",
      )
      return()
    }

    class <- sub(generic_re, "", obj$alias)
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
  ns_verbatim("import", x$val) %||%
    expand_import(x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importClassesFrom <- function(x) {
  tag_words(x, min = 2, multiline = "indent")
}
#' @export
roxy_tag_ns.roxy_tag_importClassesFrom <- function(x, block, env) {
  repeat_first_ignore_current("importClassesFrom", x$val)
}

#' @export
roxy_tag_parse.roxy_tag_importFrom <- function(x) {
  tag_words(x, min = 2, multiline = "indent")
}
#' @export
roxy_tag_ns.roxy_tag_importFrom <- function(x, block, env) {
  pkg <- x$val[1L]
  if (identical(pkg, peek_roxygen_pkg())) {
    return(NULL)
  }

  funs <- x$val[-1L]
  if (requireNamespace(pkg, quietly = TRUE)) {
    # be sure to match '%>%', `%>%`, "%>%" all to %>% given by getNamespaceExports, #1570
    unknown_idx <- !strip_quotes(funs) %in% getNamespaceExports(pkg)
    if (any(unknown_idx)) {
      warn_roxy_tag(
        x,
        "Excluding unknown {cli::qty(sum(unknown_idx))} export{?s} from {.package {pkg}}: {.code {funs[unknown_idx]}}"
      )
      funs <- funs[!unknown_idx]
    }
    if (length(funs) == 0) {
      return(NULL)
    }
  }

  import_from(pkg, funs)
}

#' @export
roxy_tag_parse.roxy_tag_importMethodsFrom <- function(x) {
  tag_words(x, min = 2, multiline = "indent")
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

  ns_verbatim("useDynLib", x$val) %||%
    repeat_first("useDynLib", x$val)
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

# Escape hatch: if a directive contains a comma, insert it verbatim rather than
# quoting each word.
ns_verbatim <- function(name, x) {
  if (!any(grepl(",", x))) {
    return(NULL)
  }
  paste0(name, "(", paste0(x, collapse = " "), ")")
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

# `import(pkg)` imports everything `pkg` exports. We expand it to an explicit
# `importFrom(pkg, ...)` over every current export so the imported set is frozen
# at document-time. This way a package that later adds new exports doesn't inject
# new conflicts into the namespace at load-time.
expand_import <- function(pkgs) {
  current <- peek_roxygen_pkg()

  # Ignore any occurrence of `current` inside `pkgs`
  if (is_string(current)) {
    pkgs <- pkgs[pkgs != current]
  }

  map(pkgs, expand_import_pkg)
}

# Falls back to an unexpanded `import(pkg)` when `pkg` isn't installed, since we
# can't read its exports without it.
expand_import_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    return(one_per_line("import", pkg))
  }

  exports <- getNamespaceExports(pkg)
  if (length(exports) == 0) {
    one_per_line("import", pkg)
  } else {
    import_from(pkg, exports, expanded = TRUE)
  }
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
  s3objects <- map(s3blocks, \(block) block$object$value)
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
