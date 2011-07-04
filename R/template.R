register.preref.parsers(parse.value, "template") 
register.preref.parsers(parse.name.description, "templateVar")

template_find <- function(base_path, template_name) {
  path <- file.path(base_path, "roxygen", str_c(template_name, ".R"))
  if (!file.exists(path)) {
    stop("Can not find template ", template_name, call. = FALSE)
  } 
  
  path
}

#' @importFrom brew brew
template_eval <- function(template_path, vars) {
  capture.output(brew(template_path, env = vars))
}

process_templates <- function(partitum, base_path) {
  template_tags <- partitum[names(partitum) == "template"]
  if (length(template_tags) == 0) return()

  templates <- unlist(template_tags, use.names = FALSE)
  paths <- vapply(templates, template_find, base_path = base_path, 
    FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  var_tags <- partitum[names(partitum) == "templateVar"]
  vars <- lapply(var_tags, "[[", "description")
  vars <- lapply(vars, type.convert, as.is = TRUE)
  
  results <- lapply(paths, template_eval, vars = list2env(vars))
  
  unlist(lapply(results, parse.preref), recursive = FALSE)
}