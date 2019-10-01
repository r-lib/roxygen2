# Re-export ----------------------------------------------------------------
rd_section_reexport <- function(pkg, fun) {
  stopifnot(is.character(pkg), is.character(fun))
  stopifnot(length(pkg) == length(fun))

  rd_section("reexport", list(pkg = pkg, fun = fun))
}

#' @export
roxy_tag_rd.roxy_tag_.reexport <- function(x, base_path, env) {
  rd_section_reexport(x$val$pkg, x$val$fun)
}
#' @export
merge.rd_section_reexport <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  rd_section_reexport(c(x$value$pkg, y$value$pkg), c(x$value$fun, y$value$fun))
}
#' @export
format.rd_section_reexport <- function(x, ...) {
  pkgs <- split(x$value$fun, x$value$pkg)
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
