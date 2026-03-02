#' @export
roxy_tag_parse.roxy_tag_template <- function(x) {
  tag_value(x)
}

#' @export
roxy_tag_parse.roxy_tag_templateVar <- function(x) {
  tag_two_part(x, "a variable name", "a value")
}

process_templates <- function(block, base_path) {
  tags <- block_get_tags(block, "template")
  if (length(tags) == 0) {
    return(block)
  }

  templates <- map_chr(tags, "val")
  paths <- map_chr(templates, template_find, base_path = base_path)

  var_tags <- block_get_tags(block, "templateVar")
  vars <- set_names(
    map(var_tags, c("val", "description")),
    map_chr(var_tags, c("val", "name"))
  )
  vars <- lapply(vars, utils::type.convert, as.is = TRUE)

  results <- lapply(paths, template_eval, vars = list2env(vars))
  tokens <- lapply(results, tokenise_block, file = "TEMPLATE", offset = 0L)
  tags <- lapply(tokens, parse_tags)

  # Insert templates back in the location where they came from
  block_replace_tags(block, "template", tags)
}

# Helpers -----------------------------------------------------------------

template_find <- function(base_path, template_name) {
  file_name <- paste0(template_name, ".", c("R", "r"))
  path <- c(
    file.path(base_path, "man-roxygen", file_name),
    file.path(base_path, "man", "roxygen", "templates", file_name)
  )
  path_exists <- file.exists(path)

  if (!any(path_exists)) {
    # This should really use warn_roxy_tag() but it's not worth refactoring
    # for this rarely used feature
    cli::cli_abort("Can't find template {.str {template_name}}", call = NULL)
  }

  path[path_exists][[1]]
}

template_eval <- function(template_path, vars) {
  utils::capture.output(brew::brew(template_path, envir = vars))
}
