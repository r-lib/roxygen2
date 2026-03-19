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

  # Which parent classes are documented?
  cls <- unique(x$classname)
  pkgs <- x$package[match(cls, x$classname)]
  ht <- map2_lgl(cls, pkgs, has_topic)
  topic_ok <- ht[match(x$classname, cls)]

  # Build display components for each inherited method
  anchor <- sprintf("method-%s-%s", x$classname, x$name)
  self_pkg <- roxy_meta_get("current_package") %||% ""
  same_pkg <- x$package == self_pkg
  prefix <- ifelse(same_pkg, "", paste0(x$package, "::"))
  label <- sprintf("%s%s$%s()", prefix, x$classname, x$name)

  # Linked version: clickable reference to parent class docs
  href <- sprintf("../../%s/html/%s.html#%s", x$package, x$classname, anchor)
  data_attrs <- paste(
    sprintf('data-pkg="%s"', x$package),
    sprintf('data-topic="%s"', x$classname),
    sprintf('data-id="%s"', x$name)
  )
  linked <- sprintf(
    '<span class="pkg-link" %s><a href=\'%s\'><code>%s</code></a></span>',
    data_attrs,
    href,
    label
  )

  # Plain version: just the method name, no link
  plain <- sprintf("<code>%s</code>", label)
  items <- paste0("  <li>", ifelse(topic_ok, linked, plain), "</li>")

  rd_if_html(paste(
    c(details, "<ul>", items, "</ul>", "</details>"),
    collapse = "\n"
  ))
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
