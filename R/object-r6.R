#' @export
object_defaults.r6class <- function(x) {
  list(
    roxy_tag("docType", NULL, NULL),
    roxy_tag(".r6data", NULL, extract_r6_data(x$value))
  )
}

extract_r6_data <- function(x) {
  list(
    self = extract_r6_self_data(x),
    super = extract_r6_super_data(x)
  )
}

extract_r6_self_data <- function(x) {
  rbind(
    extract_r6_methods(x),
    extract_r6_fields(x),
    extract_r6_bindings(x)
  )
}

omit_r6_methods <- function() {
  "clone"
}

extract_r6_methods <- function(x) {
  method_nms <- setdiff(names(x$public_methods), omit_r6_methods())
  method_loc <- map_int(
    x$public_methods[method_nms],
    function(m) {
      ref <- getSrcref(m)
      if (is.null(ref)) stop("R6 class without source references")
      getSrcLocation(ref)
    }
  )
  method_fnm <- map_chr(
    x$public_methods[method_nms],
    function(m) {
      getSrcFilename(getSrcref(m))
    }
  )
  method_formals <- map(x$public_methods[method_nms], formals)

  data.frame(
    stringsAsFactors = FALSE,
    type = if (length(method_loc)) "method" else character(),
    name = unname(method_nms),
    file = unname(method_fnm),
    line = unname(method_loc),
    formals = I(unname(method_formals))
  )
}

extract_r6_fields <- function(x) {
  field_nms <- names(x$public_fields)
  data.frame(
    stringsAsFactors = FALSE,
    type = rep("field", length(field_nms)),
    name = as.character(field_nms),
    file = rep(NA, length(field_nms)),
    line = rep(NA, length(field_nms)),
    formals = I(replicate(length(field_nms), NULL))
  )
}

extract_r6_bindings <- function(x) {
  bind_nms <- names(x$active)
  data.frame(
    stringsAsFactors = FALSE,
    type = if (length(bind_nms)) "active" else character(),
    name = as.character(bind_nms),
    file = rep(NA, length(bind_nms)),
    line = rep(NA, length(bind_nms)),
    formals = I(replicate(length(bind_nms), NULL))
  )
}

extract_r6_super_data <- function(x) {
  if (is.null(x$inherit)) return()
  super <- x$get_inherit()
  super_data <- extract_r6_super_data(super)

  method_nms <- setdiff(names(super$public_methods), omit_r6_methods())
  field_nms <- names(super$public_fields)
  active_nms <- names(super$active)
  classname <- super$classname %||% NA_character_
  pkg <- environmentName(topenv(super$parent_env))

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
  names <-c(sort(method_nms), sort(field_nms), sort(active_nms))
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
