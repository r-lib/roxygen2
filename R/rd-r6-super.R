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

  path <- ifelse(
    ht,
    sprintf("\\code{\\link[%s:%s]{%s::%s}}", pkgs, cls, pkgs, cls),
    sprintf("\\code{%s::%s}", pkgs, cls)
  )
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
