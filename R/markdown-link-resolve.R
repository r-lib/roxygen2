find_package <- function(topic, tag = NULL) {
  cur_pkg <- roxy_meta_get("current_package")
  cur_pkg_dir <- roxy_meta_get("current_package_dir")
  if (is.null(cur_pkg)) {
    # Don't try and link in basic tests
    return(NA_character_)
  }

  pkg <- find_package_cached(topic, pkg = cur_pkg, pkg_dir = cur_pkg_dir)
  if (length(pkg) == 0) {
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

find_package_cache <- new_environment()
# run in roxygenize() because the documented functions might change between runs
find_package_cache_reset <- function() {
  env_unbind(find_package_cache, env_names(find_package_cache))
  env_unbind(pkg_deps_cache, env_names(pkg_deps_cache))
}
find_package_cached <- function(topic, pkg, pkg_dir) {
  key <- paste0(pkg, "::", topic)
  env_cache(find_package_cache, key, find_package_lookup(topic, pkg, pkg_dir))
}

# NA_character  = found, doesn't need qualification
# character(0)  = not found
# character(1)  = one match
# character(>1) = multiple matches
find_package_lookup <- function(topic, pkg, pkg_dir) {
  rdtools::topic_qualifier(topic, pkg, pkg_deps(pkg_dir))
}

has_topic <- function(topic, package) {
  rdtools::topic_exists(topic, package)
}

# Cached because find_package_lookup() needs the dependencies for every
# unresolved topic, and parsing DESCRIPTION each time is slow
pkg_deps_cache <- new_environment()

pkg_deps <- function(pkgdir) {
  env_cache(pkg_deps_cache, pkgdir %||% ".", pkg_deps_lookup(pkgdir))
}

pkg_deps_lookup <- function(pkgdir) {
  deps <- desc::desc_get_deps(pkgdir)
  deps <- deps[deps$package != "R", ]
  deps <- deps[deps$type %in% c("Depends", "Imports", "Suggests"), ]
  deps$package
}
