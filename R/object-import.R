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
  pkg_links <- map2(names(pkgs), pkgs, function(pkg, funs) {
    links <- paste0("\\code{\\link[", pkg, "]{", escape(sort(funs)), "}}",
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
