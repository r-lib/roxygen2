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

  meta <- yaml::read_yaml(yaml_path())
  data.frame(
    tag = map_chr(meta, "name"),
    description = map_chr(meta, "description"),
    # \n not useful outside of RStudio
    template = sub("\n", "", map_chr(meta, "template", .default = "")),
    vignette = map_chr(meta, "vignette", .default = NA),
    recommend = map_lgl(meta, "recommend", .default = FALSE)
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
    "@aliases",
    tags_rd_section(tags, "aliases"),
    "@description",
    paste0("Learn the full details in `vignette('", type, "')`."),
    "",
    if (any(tags$recommend)) {
      c(
        "Key tags:",
        tags_rd_section(tags[tags$recommend, ], "description")
      )
    },
    if (any(!tags$recommend)) {
      c(
        "Other less frequently used tags:",
        "",
        tags_rd_section(tags[!tags$recommend, ], "description")
      )
    },
    "@usage",
    tags_rd_section(tags, "usage")
  )
}
tags_rd_section <- function(tags, section) {
  if (nrow(tags) == 0) {
    return()
  }

  switch(
    section,
    aliases = paste0("  @", tags$tag),
    usage = paste0("#' @", tags$tag, tags$template),
    description = paste0(
      "* `@",
      tags$tag,
      tags$template,
      "`: ",
      tags$description
    )
  )
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
