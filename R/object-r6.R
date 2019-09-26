#' @export
object_defaults.r6class <- function(x) {
  list(
    roxy_tag("docType", NULL, NULL),
    roxy_tag(".r6data", NULL, extract_r6_data(x$value))
  )
}

extract_r6_data <- function(x) {
  default_methods <- "clone"
  method_nms <- setdiff(names(x$public_methods), default_methods)
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
    name = method_nms,
    file = method_fnm,
    line = unname(method_loc),
    formals = I(method_formals)
  )
}
