r6_extract_fields <- function(block, r6data) {
  self <- r6data$self
  fields <- self$name[self$type == "field"]
  active <- self$name[self$type == "active"]

  tags <- keep(
    block$tags,
    function(t) t$tag == "field" && !t$val$name %in% active
  )

  labels <- gsub(",", ", ", map_chr(tags, \(x) x[["val"]][["name"]]))
  docd <- str_trim(unlist(strsplit(labels, ",")))

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

  rd_r6_fields(lapply(tags, function(t) {
    rd_r6_field(
      name = gsub(",", ", ", t$val$name),
      description = t$val$description
    )
  }))
}

r6_extract_active_bindings <- function(block, r6data) {
  self <- r6data$self
  fields <- self$name[self$type == "field"]
  active <- self$name[self$type == "active"]

  tags <- keep(
    block$tags,
    function(t) t$tag == "field" && !t$val$name %in% fields
  )

  labels <- gsub(",", ", ", map_chr(tags, \(x) x[["val"]][["name"]]))
  docd <- str_trim(unlist(strsplit(labels, ",")))

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

  rd_r6_bindings(lapply(tags, function(t) {
    rd_r6_field(
      name = gsub(",", ", ", t$val$name),
      description = t$val$description
    )
  }))
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
