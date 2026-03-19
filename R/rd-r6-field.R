r6_extract_fields <- function(block, r6data) {
  self <- r6data$self
  fields <- self$name[self$type == "field"]
  active <- self$name[self$type == "active"]

  tags <- keep(
    block$tags,
    function(t) t$tag == "field" && !t$val$name %in% active
  )

  rd_fields <- lapply(tags, function(t) {
    rd_r6_field(
      name = gsub(",", ", ", t$val$name),
      description = t$val$description
    )
  })

  docd <- r6_field_names(rd_fields)

  # Inherit undocumented fields from superclass
  miss <- setdiff(fields, docd)
  if (length(miss) > 0) {
    inherited <- r6_find_super_fields(miss, r6data, "fields")
    rd_fields <- c(rd_fields, inherited)
    docd <- r6_field_names(rd_fields)
  }

  miss <- setdiff(fields, docd)
  if (length(miss) > 0) {
    warn_roxy_block(block, "Undocumented R6 field{?s}: {miss}")
  }

  dup <- unique(docd[duplicated(docd)])
  if (length(dup) > 0) {
    warn_roxy_block(block, "R6 field{?s} documented multiple times: {dup}")
  }

  xtra <- setdiff(docd, fields)
  if (length(xtra) > 0) {
    warn_roxy_block(block, "Unknown R6 field{?s}: {xtra}")
  }

  rd_r6_fields(rd_fields)
}

r6_extract_active_bindings <- function(block, r6data) {
  self <- r6data$self
  fields <- self$name[self$type == "field"]
  active <- self$name[self$type == "active"]

  tags <- keep(
    block$tags,
    function(t) t$tag == "field" && !t$val$name %in% fields
  )

  rd_fields <- lapply(tags, function(t) {
    rd_r6_field(
      name = gsub(",", ", ", t$val$name),
      description = t$val$description
    )
  })

  docd <- r6_field_names(rd_fields)

  # Inherit undocumented active bindings from superclass
  miss <- setdiff(active, docd)
  if (length(miss) > 0) {
    inherited <- r6_find_super_fields(miss, r6data, "active_bindings")
    rd_fields <- c(rd_fields, inherited)
    docd <- r6_field_names(rd_fields)
  }

  miss <- setdiff(active, docd)
  if (length(miss) > 0) {
    warn_roxy_block(block, "Undocumented R6 active binding{?s}: {miss}")
  }

  dup <- unique(docd[duplicated(docd)])
  if (length(dup) > 0) {
    warn_roxy_block(
      block,
      "R6 active binding{?s} documented multiple times: {dup}"
    )
  }

  rd_r6_bindings(rd_fields)
}

r6_field_names <- function(rd_fields) {
  labels <- map_chr(rd_fields, \(x) x$name)
  str_trim(unlist(strsplit(labels, ",")))
}

# Rd ---------------------------------------------------------------------------

rd_r6_fields <- function(fields = list()) {
  structure(list(fields = fields), class = "rd_r6_fields")
}

rd_r6_bindings <- function(bindings = list()) {
  structure(list(fields = bindings), class = "rd_r6_bindings")
}

rd_r6_field <- function(name, description) {
  structure(
    list(name = name, description = description),
    class = "rd_r6_field"
  )
}

#' @export
format.rd_r6_field <- function(x, ...) {
  paste0("\\item{\\code{", x$name, "}}{", x$description, "}")
}

#' @export
format.rd_r6_fields <- function(x, ...) {
  format_r6_field_section(x$fields, "Public fields", "r6-fields")
}

#' @export
format.rd_r6_bindings <- function(x, ...) {
  format_r6_field_section(x$fields, "Active bindings", "r6-active-bindings")
}

format_r6_field_section <- function(fields, title, css_class) {
  if (length(fields) == 0) {
    return()
  }

  c(
    paste0("\\section{", title, "}{"),
    paste0("  ", rd_if_html(paste0('<div class="', css_class, '">'))),
    "  \\describe{",
    paste0("    ", map_chr(fields, format), collapse = "\n\n"),
    "  }",
    paste0("  ", rd_if_html("</div>")),
    "}"
  )
}

# Superclass field inheritance -----------------------------------------------

r6_find_super_fields <- function(missing, r6data, section) {
  r6_docs <- roxy_meta_get("r6_docs", list())
  if (length(r6_docs) == 0) {
    return(list())
  }

  super <- r6data$super
  if (is.null(super)) {
    return(list())
  }

  result <- list()

  for (i in seq_len(nrow(super$classes))) {
    classname <- super$classes$classname[i]
    super_doc <- r6_docs[[classname]]
    if (is.null(super_doc)) {
      next
    }

    for (field in super_doc[[section]]$fields) {
      if (field$name %in% missing) {
        result <- c(result, list(field))
        missing <- setdiff(missing, field$name)
      }
    }

    if (length(missing) == 0) break
  }

  result
}
