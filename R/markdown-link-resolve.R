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

  # Only the documented package's topics change from run to run, so evict
  # it from the topic cache (ours and pkgload's) and keep everything else
  pkg <- roxy_meta_get("current_package")
  if (!is.null(pkg)) {
    env_unbind(pkg_topics_cache, pkg)
    pkgload::dev_topic_index_reset(pkg)
  }
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
  # if it is in the current package, then no need for package name
  if (has_topic(topic, pkg)) {
    return(NA_character_)
  }

  pkgs <- pkg_deps(pkg_dir)
  pkg_has_topic <- pkgs[map_lgl(pkgs, has_topic, topic = topic)]
  pkg_has_topic <- map_chr(pkg_has_topic, \(pkg) find_source(topic, pkg))
  pkg_has_topic <- unique(pkg_has_topic)

  base <- base_packages()
  if (length(pkg_has_topic) == 0) {
    # can't find it anywhere
    character()
  } else if (length(pkg_has_topic) == 1) {
    if (pkg_has_topic %in% base) {
      # never qualify links to base packages
      NA_character_
    } else {
      pkg_has_topic
    }
  } else if (all(pkg_has_topic %in% base)) {
    # multiple base packages, no qualification needed
    NA_character_
  } else {
    pkg_has_topic
  }
}

has_topic <- function(topic, package) {
  nzchar(topic) && env_has(pkg_topics(package), topic)
}

# The topics (help aliases) of a package, as a hashed environment so that
# has_topic() is a single O(1) lookup
pkg_topics_cache <- new_environment()

pkg_topics <- function(package) {
  if (env_has(pkg_topics_cache, package)) {
    return(env_get(pkg_topics_cache, package))
  }

  topics <- pkg_topics_lookup(package)
  index <- new_environment(rep_named(topics, list(TRUE)))
  # Don't cache failure, so that installing a missing dependency
  # mid-session is picked up right away
  if (length(topics) > 0) {
    env_poke(pkg_topics_cache, package, index)
  }
  index
}

pkg_topics_lookup <- function(package) {
  path <- tryCatch(find.package(package), error = function(e) NULL)
  if (is.null(path)) {
    return(character())
  }

  aliases <- file.path(path, "help", "aliases.rds")
  if (file.exists(aliases)) {
    names(readRDS(aliases))
  } else {
    names(pkgload::dev_topic_index(path))
  }
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
  c(deps$package, base_packages())
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
find_source <- function(topic, package) {
  if (package %in% base_packages()) {
    return(package)
  }

  ns <- ns_env(package)
  if (!env_has(ns, topic, inherit = TRUE)) {
    return(package)
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
    wpkgs <- map_lgl(imp, `%in%`, x = topic)
    if (!any(wpkgs)) {
      return(package)
    }

    pkgs <- names(wpkgs)[wpkgs]
    # Take the last match, in case imports have name clashes.
    pkgs[[length(pkgs)]]
  }
}
