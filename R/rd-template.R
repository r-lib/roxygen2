template_find <- function(base_path, template_name) {
  file_name <- paste0(template_name, ".", c("R", "r"))
  path <- c(
    file.path(base_path, "man-roxygen", file_name),
    file.path(base_path, "man", "roxygen", "templates", file_name)
  )
  path_exists <- file.exists(path)

  if (!any(path_exists)) {
    stop("Can't find template '", template_name, "'", call. = FALSE)
  }

  path[path_exists][[1]]
}

template_eval <- function(template_path, vars) {
  utils::capture.output(brew::brew(template_path, envir = vars))
}

process_templates <- function(block, base_path, global_options = list()) {
  template_locs <- names(block) == "template"
  template_tags <- block[template_locs]
  if (length(template_tags) == 0)
    return(block)

  templates <- unlist(template_tags, use.names = FALSE)
  paths <- map_chr(templates, template_find, base_path = base_path)

  var_tags <- block[names(block) == "templateVar"]
  vars <- set_names(map(var_tags, "description"), map_chr(var_tags, "name"))
  vars <- lapply(vars, utils::type.convert, as.is = TRUE)

  results <- lapply(paths, template_eval, vars = list2env(vars))
  tokens <- lapply(results, tokenise_block, file = "TEMPLATE", offset = 0L)

  # Insert templates back in the location where they came from
  tags <- lapply(block, list)
  tags[template_locs] <- lapply(tokens, parse_tags,
    registry = roclet_tags.roclet_rd(list()),
    global_options = global_options
  )
  names(tags)[template_locs] <- ""

  roxy_block_copy(block, unlist(tags, recursive = FALSE))
}
