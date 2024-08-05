#' @export
object_defaults.r6class <- function(x, block) {
  r6on <- roxy_meta_get("r6", TRUE)
  if (isTRUE(r6on)) {
    list(
      roxy_generated_tag(block, "docType", NULL),
      roxy_generated_tag(block, ".r6data", extract_r6_data(x$value))
    )
  } else {
    NextMethod()
  }
}

extract_r6_data <- function(x) {
  list(
    self = extract_r6_self_data(x),
    super = drop_clone_maybe(x, extract_r6_super_data(x))
  )
}

drop_clone_maybe <- function(x, data) {
  if (! "clone" %in% names(.subset2(x, "public_methods"))) {
    cline <- which(data$members$name == "clone" & data$members$type == "method")
    if (length(cline)) data$members <- data$members[-cline, ]
  }
  data
}

extract_r6_self_data <- function(x) {
  rbind(
    extract_r6_methods(x),
    extract_r6_fields(x),
    extract_r6_bindings(x)
  )
}

default_r6_methods <- function() {
  "clone"
}

extract_r6_methods <- function(x) {
  public_methods <- .subset2(x, "public_methods")
  method_nms <- setdiff(names(public_methods), default_r6_methods())
  method_loc <- map_int(
    public_methods[method_nms],
    function(m) {
      ref <- utils::getSrcref(m)
      if (is.null(ref)) {
        name <- .subset2(x, "classname") %||% "unknown"

        cli::cli_abort(
          c(
            "R6 class {.cls {name}} lacks source references.",
            i = paste0(
              "If you are using the `installed` load method in {.file DESCRIPTION}, then ",
              "try re-installing the package with option '--with-keep.source', e.g. ",
              "{.code install.packages(..., INSTALL_OPTS = \"--with-keep.source\")}."
            )
          ),
          call = NULL
        )
      }
      utils::getSrcLocation(ref)
    }
  )
  method_fnm <- map_chr(
    public_methods[method_nms],
    function(m) {
      utils::getSrcFilename(utils::getSrcref(m))
    }
  )
  method_formals <- map(public_methods[method_nms], formals)

  methods <- data.frame(
    stringsAsFactors = FALSE,
    type = if (length(method_loc)) "method" else character(),
    class = if (length(method_loc)) .subset2(x, "classname") %||% NA_character_ else character(),
    name = unname(method_nms),
    file = unname(method_fnm),
    line = unname(method_loc),
    formals = I(unname(method_formals))
  )

  add_default_method_data(x, methods)
}

add_default_method_data <- function(obj, methods) {
  pubm <- .subset2(obj, "public_methods")
  defaults <- list(
    clone = list(
      formals = if ("clone" %in% names(pubm)) I(list(formals(pubm$clone)))
    )
  )

  for (mname in names(defaults)) {
    if (mname %in% methods$name) next
    if (! mname %in% names(pubm)) next
    rec <- data.frame(
      stringsAsFactors = FALSE,
      type = defaults[[mname]]$type %||% "method",
      class = defaults[[mname]]$class %||%
        .subset2(obj, "classname") %||% "unknown",
      name = defaults[[mname]]$name %||% mname,
      file = defaults[[mname]]$file %||% NA_character_,
      line = defaults[[mname]]$line %||% NA_integer_,
      formals = defaults[[mname]]$formals %||% NULL
    )
    methods <- rbind(methods, rec)
  }

  methods
}

extract_r6_fields <- function(x) {
  field_nms <- names(.subset2(x, "public_fields"))
  data.frame(
    stringsAsFactors = FALSE,
    type = rep("field", length(field_nms)),
    name = as.character(field_nms),
    class = rep(.subset2(x, "classname") %||% NA_character_, length(field_nms)),
    file = rep(NA, length(field_nms)),
    line = rep(NA, length(field_nms)),
    formals = I(replicate(length(field_nms), NULL))
  )
}

extract_r6_bindings <- function(x) {
  bind_nms <- names(.subset2(x, "active"))
  data.frame(
    stringsAsFactors = FALSE,
    type = if (length(bind_nms)) "active" else character(),
    name = as.character(bind_nms),
    class = rep(.subset2(x, "classname") %||% NA_character_, length(bind_nms)),
    file = rep(NA, length(bind_nms)),
    line = rep(NA, length(bind_nms)),
    formals = I(replicate(length(bind_nms), NULL))
  )
}

extract_r6_super_data <- function(x) {
  if (is.null(.subset2(x, "inherit"))) return()
  super <- .subset2(x, "get_inherit")()
  super_data <- extract_r6_super_data(super)

  method_nms <- names(.subset2(super, "public_methods"))
  field_nms <- names(.subset2(super, "public_fields"))
  active_nms <- names(.subset2(super, "active"))
  classname <- .subset2(super, "classname") %||% NA_character_
  pkg <- environmentName(topenv(.subset2(super, "parent_env")))

  cls <- rbind(
    data.frame(
      stringsAsFactors = FALSE,
      package = pkg,
      classname = classname
    ),
    super_data$classes
  )

  types <- rep(
    c("method", "field", "active"),
    c(length(method_nms), length(field_nms), length(active_nms))
  )
  rsort <- function(x) sort_c(x, decreasing = TRUE)
  names <- c(rsort(method_nms), rsort(field_nms), rsort(active_nms))
  mth <- rbind(
    data.frame(
      stringsAsFactors = FALSE,
      package = rep(pkg, length(names)),
      classname = rep(classname , length(names)),
      type = types,
      name = names
    ),
    super_data$members
  )

  list(classes = cls, members = mth)
}
