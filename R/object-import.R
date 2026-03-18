# Re-export ----------------------------------------------------------------
rd_section_reexport <- function(pkg, fun) {
  check_character(pkg)
  check_character(fun)
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
  rd_section_reexport(
    c(x$value$pkg, y$value$pkg),
    c(x$value$fun, y$value$fun)
  )
}
#' @export
format.rd_section_reexport <- function(x, ...) {
  info <- data.frame(
    pkg = x$value$pkg,
    fun = x$value$fun
  )

  pkgs <- split(info, x$value$pkg)
  pkg_links <- map(pkgs, function(pkg) {
    pkg <- pkg[order(pkg$fun), ]
    links <- paste0(reexport_link(pkg$pkg, pkg$fun), collapse = ", ")
    paste0("\\item{", pkg$pkg[[1]], "}{", links, "}")
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

reexport_link <- function(pkg, fun) {
  env <- tryCatch(asNamespace(pkg), error = function(e) emptyenv())
  map_chr(seq_along(fun), function(i) {
    rd_link(pkg[[i]], escape(fun[[i]]), escape(fun_suffix(fun[[i]], env)), code = TRUE)
  })
}
