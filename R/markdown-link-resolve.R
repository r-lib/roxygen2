resolve_link_package <- function(topic, tag = NULL) {
  pkg <- roxy_meta_get("current_package")
  pkg_dir <- roxy_meta_get("current_package_dir")
  tag <- tag %||% roxy_tag("unknown", "") # only for tests

  pkg <- find_topic_package(topic, pkg = pkg, pkg_dir = pkg_dir)

  if (length(pkg) == 0) {
    warn_roxy_tag(
      tag,
      c(
        "Could not resolve link to topic {.val {topic}} in the dependencies or base packages",
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
        "Topic {.val {topic}} is available in multiple packages: {.pkg {pkg}}",
        i = "Qualify topic explicitly with a package name when linking to it."
      )
    )
    return(NA_character_)
  }
}

# Returns:
# - NA_character_: topic found in current package or base packages (no
#   qualification needed)
# - A single package name: topic found in exactly one dependency
# - A vector of package names: topic found in multiple dependencies
# - character(): topic not found anywhere
find_topic_package <- function(topic, pkg, pkg_dir) {
  # if it is in the current package, then no need for package name
  if (has_topic(topic, pkg)) {
    return(NA_character_)
  }

  # first look in Depends/Imports/Suggests
  pkgs <- local_pkg_deps(pkg_dir)
  pkg_has_topic <- pkgs[map_lgl(pkgs, has_topic, topic = topic)]
  pkg_has_topic <- map_chr(pkg_has_topic, function(p) {
    find_reexport_source(topic, p) %||% p
  })
  pkg_has_topic <- unique(pkg_has_topic)
  if (length(pkg_has_topic) >= 1) {
    return(pkg_has_topic)
  }

  # then try base packages
  for (bp in base_packages()) {
    if (has_topic(topic, bp)) return(NA_character_)
  }

  character()
}

local_pkg_deps <- function(pkgdir = NULL) {
  pkgdir <- pkgdir %||% roxy_meta_get("current_package_dir")
  deps <- desc::desc_get_deps(pkgdir)
  deps <- deps[deps$package != "R", ]
  deps <- deps[deps$type %in% c("Depends", "Imports", "Suggests"), ]
  deps$package
}

base_packages <- function() {
  if (getRversion() >= "4.4.0") {
    asNamespace("tools")$standard_package_names()[["base"]]
  } else {
    c(
      "base",
      "compiler",
      "datasets",
      "graphics",
      "grDevices",
      "grid",
      "methods",
      "parallel",
      "splines",
      "stats",
      "stats4",
      "tcltk",
      "tools",
      "utils"
    )
  }
}

# Adapted from downlit:::find_reexport_source
find_reexport_source <- function(topic, package) {
  if (package %in% base_packages()) {
    return(NULL)
  }

  ns <- ns_env(package)
  if (!env_has(ns, topic, inherit = TRUE)) {
    return(NULL)
  }

  obj <- env_get(ns, topic, inherit = TRUE)
  if (is.primitive(obj)) {
    # primitive functions all live in base
    "base"
  } else if (is.function(obj)) {
    ## For functions, we can just take their environment.
    ns_env_name(get_env(obj))
  } else {
    ## For other objects, we need to check the import env of the package,
    ## to see where 'topic' is coming from. The import env has redundant
    ## information. It seems that we just need to find a named list
    ## entry that contains `topic`.
    imp <- getNamespaceImports(ns)
    imp <- imp[names(imp) != ""]
    wpkgs <- vapply(imp, `%in%`, x = topic, FUN.VALUE = logical(1))

    if (!any(wpkgs)) {
      return(NULL)
    }
    pkgs <- names(wpkgs)[wpkgs]
    # Take the last match, in case imports have name clashes.
    pkgs[[length(pkgs)]]
  }
}
