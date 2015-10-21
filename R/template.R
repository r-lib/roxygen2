register_tags(
  template = parse.value,
  templateVar = parse.name.description
)

template_find <- function(base_path, template_name) {
  path <- file.path(base_path, "man-roxygen", paste0(template_name, ".", c("R", "r")))
  path_exists <- file.exists(path)

  if (!any(path_exists)) {
    stop("Can not find template ", template_name, call. = FALSE)
  }

  path[path_exists][1]
}

template_eval <- function(template_path, vars) {
  utils::capture.output(brew::brew(template_path, envir = vars))
}

process_templates <- function(partitum, base_path) {
  template_locs <- names(partitum) == "template"
  template_tags <- partitum[template_locs]
  if (length(template_tags) == 0) return(partitum)

  templates <- unlist(template_tags, use.names = FALSE)
  paths <- vapply(templates, template_find, base_path = base_path,
    FUN.VALUE = character(1), USE.NAMES = FALSE)

  var_tags <- partitum[names(partitum) == "templateVar"]
  vars <- lapply(var_tags, "[[", "description")
  names(vars) <- vapply(var_tags, "[[", "name", FUN.VALUE = character(1))
  vars <- lapply(vars, utils::type.convert, as.is = TRUE)

  results <- lapply(paths, template_eval, vars = list2env(vars))

  # Insert templates back in the location where they came from
  partitum_pieces <- lapply(partitum, list)
  partitum_pieces[template_locs] <- lapply(results, parse_preref, file = "TEMPLATE", offset = 0L)
  names(partitum_pieces)[template_locs] <- ""

  unlist(partitum_pieces, recursive = FALSE)
}
