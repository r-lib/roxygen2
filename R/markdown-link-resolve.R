resolve_link_package <- function(
  topic,
  me = NULL,
  pkgdir = NULL,
  state = NULL
) {
  me <- me %||% roxy_meta_get("current_package")
  # this is  from the roxygen2 tests, should not happen on a real package
  if (is.null(me) || is.na(me) || me == "") {
    return(NA_character_)
  }

  # if it is in the current package, then no need for package name, right?
  if (has_topic(topic, me)) {
    return(NA_character_)
  }

  # try packages in depends, imports, suggests first, error on name clashes
  pkgs <- local_pkg_deps(pkgdir)

  pkg_has_topic <- pkgs[map_lgl(pkgs, has_topic, topic = topic)]
  pkg_has_topic <- map_chr(pkg_has_topic, function(p) {
    p0 <- find_reexport_source(topic, p)
    if (is.na(p0)) p else p0
  })
  pkg_has_topic <- unique(pkg_has_topic)
  base <- base_packages()
  if (length(pkg_has_topic) == 0) {
    # fall through to check base packages as well
  } else if (length(pkg_has_topic) == 1) {
    if (pkg_has_topic %in% base) {
      return(NA_character_)
    } else {
      return(pkg_has_topic)
    }
  } else {
    warn_roxy_tag(
      state$tag,
      c(
        "Topic {.val {topic}} is available in multiple packages: {.pkg {pkg_has_topic}}",
        i = "Qualify topic explicitly with a package name when linking to it."
      )
    )
    return(NA_character_)
  }

  # try base packages as well, take the first hit,
  # there should not be any name clashes, anyway
  for (bp in base) {
    if (has_topic(topic, bp)) return(NA_character_)
  }

  warn_roxy_tag(
    state$tag,
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
}

local_pkg_deps <- function(pkgdir = NULL) {
  pkgdir <- pkgdir %||% roxy_meta_get("current_package_dir")
  deps <- desc::desc_get_deps(pkgdir)
  deps <- deps[deps$package != "R", ]
  deps <- deps[deps$type %in% c("Depends", "Imports", "Suggests"), ]
  deps$package
}

# this is mostly from downlit
is_exported <- function(name, package) {
  name %in% getNamespaceExports(ns_env(package))
}

is_reexported <- function(name, package) {
  if (package == "base") {
    return(FALSE)
  }
  is_imported <- env_has(ns_imports_env(package), name)
  is_imported && is_exported(name, package)
}

find_reexport_source <- function(topic, package) {
  ns <- ns_env(package)
  if (!env_has(ns, topic, inherit = TRUE)) {
    return(NA_character_)
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
      return(NA_character_)
    }
    pkgs <- names(wpkgs)[wpkgs]
    # Take the last match, in case imports have name clashes.
    pkgs[[length(pkgs)]]
  }
}
