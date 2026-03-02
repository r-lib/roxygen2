# Re-export ----------------------------------------------------------------
rd_section_reexport <- function(pkg, fun, file) {
  stopifnot(is.character(pkg), is.character(fun), is.character(file))
  stopifnot(length(pkg) == length(fun))

  rd_section("reexport", list(pkg = pkg, fun = fun, file = file))
}

#' @export
roxy_tag_rd.roxy_tag_.reexport <- function(x, base_path, env) {
  file <- find_topic_filename(x$val$pkg, x$val$fun, tag = x)
  rd_section_reexport(x$val$pkg, x$val$fun, file)
}
#' @export
merge.rd_section_reexport <- function(x, y, ...) {
  stopifnot(identical(class(x), class(y)))
  rd_section_reexport(
    c(x$value$pkg, y$value$pkg),
    c(x$value$fun, y$value$fun),
    c(x$value$file, y$value$file)
  )
}
#' @export
format.rd_section_reexport <- function(x, ...) {
  info <- data.frame(
    pkg = x$value$pkg,
    fun = x$value$fun,
    file = x$value$file
  )

  pkgs <- split(info, x$value$pkg)
  pkg_links <- map(pkgs, function(pkg) {
    pkg <- pkg[order(pkg$fun), ]
    links <- paste0(
      "\\code{\\link[",
      pkg$pkg,
      ifelse(pkg$file == pkg$fun, "", paste0(":", pkg$file)),
      "]{",
      escape(pkg$fun),
      "}}",
      collapse = ", "
    )
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
