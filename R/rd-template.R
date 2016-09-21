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

process_templates <- function(block, base_path, global_options = list()) {
  template_locs <- names(block) == "template"
  template_tags <- block[template_locs]
  if (length(template_tags) == 0) return(block)

  templates <- unlist(template_tags, use.names = FALSE)
  paths <- vapply(templates, template_find, base_path = base_path,
    FUN.VALUE = character(1), USE.NAMES = FALSE)

  var_tags <- block[names(block) == "templateVar"]
  vars <- lapply(var_tags, "[[", "description")
  names(vars) <- vapply(var_tags, "[[", "name", FUN.VALUE = character(1))
  vars <- lapply(vars, utils::type.convert, as.is = TRUE)

  results <- lapply(paths, template_eval, vars = list2env(vars))

  # Insert templates back in the location where they came from
  block_pieces <- lapply(block, list)
  block_pieces[template_locs] <- lapply(results, parse_block,
    file = "TEMPLATE", registry = roclet_tags.roclet_rd(list()), offset = 0L,
    global_options = global_options)
  names(block_pieces)[template_locs] <- ""

  unlist(block_pieces, recursive = FALSE)
}
