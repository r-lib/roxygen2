#' Access metadata about built-in or available tags
#'
#' @export
#' @family extending
#' @param built_in Logical. Whether to restrict the result to built-in tags
#' (`TRUE`) or to extend it to available tags (`FALSE`).
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

  meta <- yaml::read_yaml(yaml_path())
  data.frame(
    tag = map_chr(meta, \(x) x[["name"]]),
    description = map_chr(meta, \(x) x[["description"]]),
    # \n not useful outside of RStudio
    template = sub("\n", "", map_chr(meta, \(x) x[["template"]] %||% "")),
    vignette = map_chr(meta, \(x) x[["vignette"]] %||% NA_character_),
    recommend = map_lgl(meta, \(x) x[["recommend"]] %||% FALSE)
  )
}

yaml_path <- function() {
  system.file("roxygen2-tags.yml", package = "roxygen2")
}

tags_rd <- function(type) {
  tags <- tags_metadata()
  tags <- tags[tags$vignette == type & !is.na(tags$vignette), ]

  c(
    paste0("@name tags-", type),
    paste0("@aliases ", "@", tags$tag),
    "@description",
    paste0("Learn the full details in `vignette('", type, "')`."),
    "",
    if (any(tags$recommend)) {
      c(
        "Key tags:",
        tags_rd_desc(tags[tags$recommend, ])
      )
    },
    if (any(!tags$recommend)) {
      c(
        "Other less frequently used tags:",
        tags_rd_desc(tags[!tags$recommend, ])
      )
    },
    "@usage",
    paste0("#' @", tags$tag, tags$template)
  )
}
tags_rd_desc <- function(tags, section) {
  paste0("* `@", tags$tag, tags$template, "`: ", tags$description)
}

#' Tags for documenting functions
#'
#' @eval tags_rd("rd")
#' @family documentation tags
NULL

#' Tags for documenting datasets and classes
#'
#' @eval tags_rd("rd-other")
#' @family documentation tags
NULL

#' Tags that help you reuse documentation
#'
#' @eval tags_rd("reuse")
#' @family documentation tags
NULL

#' Tags for managing the `NAMESPACE`
#'
#' @eval tags_rd("namespace")
NULL

#' Tags related to markdown support
#'
#' @eval tags_rd("rd-formatting")
NULL

#' Tags for indexing and cross-references
#'
#' @eval tags_rd("index-crossref")
#' @family documentation tags
NULL
