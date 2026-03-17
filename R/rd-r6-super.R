rd_r6_super <- function(
  class,
  package = character(),
  classname = character(),
  has_topic = logical()
) {
  structure(
    list(
      class = class,
      package = package,
      classname = classname,
      has_topic = has_topic
    ),
    class = "rd_r6_super"
  )
}

#' @export
format.rd_r6_super <- function(x, ...) {
  if (length(x$classname) == 0) {
    return()
  }

  cls <- x$classname
  pkgs <- x$package
  ht <- x$has_topic

  title <- if (length(cls) > 1) "Super classes" else "Super class"

  self_pkg <- roxy_meta_get("current_package") %||% ""
  same_pkg <- pkgs == self_pkg

  label <- ifelse(same_pkg, cls, paste0(pkgs, "::", cls))
  path <- map_chr(seq_along(cls), function(i) {
    if (ht[[i]]) {
      rd_link(pkgs[[i]], cls[[i]], label[[i]], code = TRUE)
    } else {
      paste0("\\code{", label[[i]], "}")
    }
  })
  me <- sprintf("\\code{%s}", x$class)

  c(
    paste0("\\section{", title, "}{"),
    paste(c(rev(path), me), collapse = " -> "),
    "}"
  )
}

r6_extract_superclasses <- function(r6data, env, class) {
  super <- r6data$super
  cls <- unique(super$classes$classname)
  if (length(cls) == 0) {
    return(rd_r6_super(class))
  }

  pkgs <- super$classes$package[match(cls, super$classes$classname)]
  ht <- map2_lgl(cls, pkgs, has_topic)
  rd_r6_super(class, package = pkgs, classname = cls, has_topic = ht)
}
