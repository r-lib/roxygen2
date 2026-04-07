r6_extract_field_tags <- function(block, r6data, type = c("field", "active")) {
  type <- match.arg(type)
  other_type <- if (type == "field") "active" else "field"
  label <- if (type == "field") "field" else "active binding"

  self <- r6data$self
  expected <- self$name[self$type == type]
  other <- self$name[self$type == other_type]

  tags <- keep(block$tags, \(t) tag_is(t, "field") && !tag_has_name(t, other))
  docd <- unlist(lapply(tags, tag_names))

  miss <- setdiff(expected, docd)
  if (length(miss) > 0) {
    warn_roxy_block(block, "Undocumented R6 {label}{?s}: {miss}")
  }

  dup <- unique(docd[duplicated(docd)])
  if (length(dup) > 0) {
    warn_roxy_block(block, "R6 {label}{?s} documented multiple times: {dup}")
  }

  if (type == "field") {
    xtra <- setdiff(docd, expected)
    if (length(xtra) > 0) {
      warn_roxy_block(block, "Unknown R6 {label}{?s}: {xtra}")
    }
  }

  # @field name NULL suppresses documentation for that field/binding
  tags <- discard(tags, function(t) toupper(t$val$description) == "NULL")

  items <- lapply(tags, function(t) {
    rd_r6_field(
      name = gsub(",", ", ", t$val$name),
      description = t$val$description
    )
  })

  rd_r6_fields(items, type = type)
}

# Rd ---------------------------------------------------------------------------

rd_r6_fields <- function(fields = list(), type = c("field", "active")) {
  type <- match.arg(type)
  structure(list(fields = fields, type = type), class = "rd_r6_fields")
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
  if (x$type == "field") {
    format_r6_field_section(x$fields, "Public fields", "r6-fields")
  } else {
    format_r6_field_section(x$fields, "Active bindings", "r6-active-bindings")
  }
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
