find_package <- function(topic, tag = NULL) {
  cur_pkg <- roxy_meta_get("current_package")
  cur_pkg_dir <- roxy_meta_get("current_package_dir")
  if (is.null(cur_pkg)) {
    # Don't try and link in basic tests
    return(NA_character_)
  }

  deps <- rdtools::pkg_search_deps(cur_pkg_dir %||% cur_pkg)
  pkg <- rdtools::topic_qualifier(topic, from = cur_pkg, packages = deps)
  if (is.null(pkg)) {
    warn_roxy_tag(
      tag,
      c(
        "Could not resolve link to topic {.val {topic}} in the dependencies or base packages.",
        "i" = paste(
          "If you haven't documented {.val {topic}} yet, or just changed its name, this is normal.",
          "Once {.val {topic}} is documented, this warning goes away."
        ),
        "i" = "Make sure that the name of the topic is spelled correctly.",
        "i" = "Always list the linked package as a dependency.",
        "i" = "Alternatively, you can fully qualify the link with a package name."
      )
    )
    NA_character_
  } else if (length(pkg) == 1) {
    pkg
  } else {
    warn_roxy_tag(
      tag,
      c(
        "Topic {.val {topic}} is available in multiple packages: {.pkg {pkg}}.",
        i = "Qualify topic explicitly with a package name when linking to it."
      )
    )
    NA_character_
  }
}

has_topic <- function(topic, package) {
  rdtools::topic_exists(topic, package)
}
