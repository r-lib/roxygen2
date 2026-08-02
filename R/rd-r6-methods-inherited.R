rd_r6_inherited <- function(
  package = character(),
  classname = character(),
  name = character(),
  chain_package = character(),
  chain_classname = character()
) {
  structure(
    list(
      package = package,
      classname = classname,
      name = name,
      # Ancestor chain, nearest first, e.g. chain_classname[[1]] is the
      # immediate parent, chain_classname[[2]] the grandparent, etc.
      chain_package = chain_package,
      chain_classname = chain_classname
    ),
    class = "rd_r6_inherited"
  )
}

#' @export
format.rd_r6_inherited <- function(x, ...) {
  if (length(x$name) == 0) {
    return()
  }

  display <- roxy_meta_get("r6_inherited_documentation_display", "grouped")
  if (!display %in% c("original", "single", "grouped")) {
    cli::cli_abort(
      "{.field r6_inherited_documentation_display} must be one of {.val original}, {.val single}, or {.val grouped}, not {.val {display}}.",
      call = NULL
    )
  }

  if (display == "original") {
    # "original": byte-for-byte the pre-8.1.0 rendering -- a single flat
    # list mixing every ancestor's methods together (using each method's
    # actual originating class, not collapsed to the immediate parent),
    # auto-expanded when there are 5 or fewer. This fully undoes the
    # effect of this PR.
    details <- paste0(
      "<details",
      if (length(x$name) <= 5) " open",
      "><summary>Inherited methods</summary>"
    )

    cls <- unique(x$classname)
    pkgs <- x$package[match(cls, x$classname)]
    ht <- map2_lgl(cls, pkgs, has_topic)
    topic_ok <- ht[match(x$classname, cls)]

    anchor <- sprintf("method-%s-%s", x$classname, x$name)
    self_pkg <- roxy_meta_get("current_package") %||% ""
    same_pkg <- x$package == self_pkg
    prefix <- ifelse(same_pkg, "", paste0(x$package, "::"))
    label <- sprintf("%s%s$%s()", prefix, x$classname, x$name)

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

    plain <- sprintf("<code>%s</code>", label)
    items <- paste0("  <li>", ifelse(topic_ok, linked, plain), "</li>")

    return(rd_if_html(paste(
      c(details, "<ul>", items, "</ul>", "</details>"),
      collapse = "\n"
    )))
  }

  self_pkg <- roxy_meta_get("current_package") %||% ""
  # R6 classes loaded via a non-standard strategy (e.g. sourcing files
  # directly into a scratch environment instead of via pkgload) can end up
  # with package = "" or NA for locally-defined superclasses. Treat that
  # the same as "defined in the package currently being documented".
  fill_pkg <- function(pkg) if (is.na(pkg) || !nzchar(pkg)) self_pkg else pkg

  link_to <- function(pkg, cls) {
    pkg <- fill_pkg(pkg)
    label <- if (pkg == self_pkg) cls else paste0(pkg, "::", cls)
    if (has_topic(cls, pkg)) {
      sprintf(
        "<a href='../../%s/html/%s.html'><code>%s</code></a>",
        pkg,
        cls,
        label
      )
    } else {
      sprintf("<code>%s</code>", label)
    }
  }

  if (display == "single") {
    # "single": always point to the immediate parent class with a single
    # fixed-size line, regardless of how many methods are inherited or how
    # many levels up the chain they were originally defined -- this keeps
    # Rd size for a class O(1) in inheritance depth and method count,
    # instead of enumerating every inherited method (or every distinct
    # ancestor class) explicitly.
    linked <- link_to(x$chain_package[[1]], x$chain_classname[[1]])
    return(rd_if_html(sprintf(
      "<p>+ inherited public methods from %s.</p>",
      linked
    )))
  }

  # "grouped" (the default): one subsection per ancestor class that
  # actually contributed an inherited method, nearest ancestor first,
  # each listing its own methods as a linked bullet list. Every
  # subsection starts collapsed (no `open` attribute) -- the reader clicks
  # the disclosure triangle to expand it.
  sections <- character()
  for (i in seq_along(x$chain_classname)) {
    cls <- x$chain_classname[[i]]
    pkg <- fill_pkg(x$chain_package[[i]])
    idx <- which(x$classname == cls)
    if (length(idx) == 0) next

    label <- if (pkg == self_pkg) cls else paste0(pkg, "::", cls)
    method_label <- sprintf(
      "%s%s$%s()",
      if (pkg == self_pkg) "" else paste0(pkg, "::"),
      cls,
      x$name[idx]
    )
    anchor <- sprintf("method-%s-%s", cls, x$name[idx])
    href <- sprintf("../../%s/html/%s.html#%s", pkg, cls, anchor)
    item_link <- if (has_topic(cls, pkg)) {
      sprintf("<a href='%s'><code>%s</code></a>", href, method_label)
    } else {
      sprintf("<code>%s</code>", method_label)
    }
    items <- paste0("  <li>", item_link, "</li>")

    sections <- c(sections, paste(
      c(
        sprintf(
          "<details><summary>+ inherited public methods from %s</summary>",
          label
        ),
        "<ul>",
        items,
        "</ul>",
        "</details>"
      ),
      collapse = "\n"
    ))
  }

  rd_if_html(paste(sections, collapse = "\n"))
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
    name = super_meth$name,
    chain_package = super$classes$package,
    chain_classname = super$classes$classname
  )
}
