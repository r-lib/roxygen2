rd_r6_inherited <- function(
  package = character(),
  classname = character(),
  name = character()
) {
  structure(
    list(package = package, classname = classname, name = name),
    class = "rd_r6_inherited"
  )
}

#' @export
format.rd_r6_inherited <- function(x, ...) {
  if (length(x$name) == 0) {
    return()
  }

  details <- paste0(
    "<details",
    if (length(x$name) <= 5) " open",
    "><summary>Inherited methods</summary>"
  )

  c(
    "\\if{html}{\\out{",
    details,
    "<ul>",
    sprintf(
      paste0(
        "<li>",
        "<span class=\"pkg-link\" data-pkg=\"%s\" data-topic=\"%s\" data-id=\"%s\">",
        "<a href='../../%s/html/%s.html#method-%s-%s'><code>%s::%s$%s()</code></a>",
        "</span>",
        "</li>"
      ),
      x$package,
      x$classname,
      x$name,
      x$package,
      x$classname,
      x$classname,
      x$name,
      x$package,
      x$classname,
      x$name
    ),
    "</ul>",
    "</details>",
    "}}"
  )
}

r6_extract_inherited_methods <- function(r6data) {
  super <- r6data$super
  if (is.null(super)) {
    return(rd_r6_inherited())
  }

  super_meth <- super$members[super$members$type == "method", ]
  self <- r6data$self
  super_meth <- super_meth[!super_meth$name %in% self$name, ]
  super_meth <- super_meth[!duplicated(super_meth$name), ]
  if (nrow(super_meth) == 0) {
    return(rd_r6_inherited())
  }

  super_meth <- super_meth[rev(seq_len(nrow(super_meth))), ]
  rd_r6_inherited(
    package = super_meth$package,
    classname = super_meth$classname,
    name = super_meth$name
  )
}
