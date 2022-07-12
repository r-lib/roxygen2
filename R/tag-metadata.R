#' Access metadata about built-in tags
#'
#' @export
#' @keywords internal
tags_list <- function(built_in = TRUE) {
  if (isTRUE(built_in)) {
    methods <- attr(methods('roxy_tag_parse'), "info")
    methods <- methods[methods$from == "roxygen2", ]
    methods <- rownames(methods)[-1]
  } else {
    # Needed since the info attribute doesn't seem to exist
    # during R CMD check
    methods <- as.character(methods('roxy_tag_parse'))[-1]
  }
  sort(sub('.*\\.roxy_tag_', '', methods))
}

#' @export
#' @rdname tags_list
tags_metadata <- function() {
  check_installed("yaml")

  yaml::read_yaml(yaml_path())
}

yaml_path <- function() {
  system.file("tags.yml", package = "roxygen2")
}
