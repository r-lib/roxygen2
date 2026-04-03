r6_extract_field_tags <- function(block, r6data, type = c("field", "active")) {
  type <- match.arg(type)
  other_type <- if (type == "field") "active" else "field"
  label <- if (type == "field") "field" else "active binding"

  self <- r6data$self
  expected <- self$name[self$type == type]
  other <- self$name[self$type == other_type]

  tags <- keep(block$tags, \(t) tag_is(t, "field") && !tag_has_name(t, other))
  docd <- unlist(lapply(tags, tag_names))

  # Inherit undocumented fields/bindings from superclass
  section <- if (type == "field") "fields" else "active_bindings"
  miss <- setdiff(expected, docd)
  inherited <- list()
  if (length(miss) > 0) {
    inherited <- r6_find_super_fields(miss, r6data, section)
    docd <- c(docd, r6_field_names(inherited))
  }

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
  items <- c(items, inherited)

  rd_r6_fields(items, type = type)
}

r6_field_names <- function(rd_fields) {
  labels <- map_chr(rd_fields, \(x) x$name)
  str_trim(unlist(strsplit(labels, ",")))
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
