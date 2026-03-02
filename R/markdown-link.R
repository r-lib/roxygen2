#' Add link reference definitions for functions to a markdown text.
#'
#' We find the `[text][ref]` and the `[ref]` forms. There must be no
#' spaces between the closing and opening bracket in the `[text][ref]`
#' form.
#'
#' Starting from R 4.0.2-ish, explicit cross-package links to topics are not
#' allowed, so for each such linked topic, we look up the linked file.
#'
#' These are the link references we add:
#' ```
#' MARKDOWN           LINK TEXT  CODE RD
#' --------           ---------  ---- --
#' [fun()]            fun()       T   \\link[=fun]{fun()} or
#'                                    \\link[pkg:file]{pkg::fun()}
#' [obj]              obj         F   \\link{obj} or
#'                                    \\link[pkg:file]{pkg::obj}
#' [pkg::fun()]       pkg::fun()  T   \\link[pkg:file]{pkg::fun()}
#' [pkg::obj]         pkg::obj    F   \\link[pkg:file]{pkg::obj}
#' [text][fun()]      text        F   \\link[=fun]{text} or
#'                                    \\link[pkg:file]{text}
#' [text][obj]        text        F   \\link[=obj]{text} or
#'                                    \\link[pkg:file]{text}
#' [text][pkg::fun()] text        F   \\link[pkg:file]{text}
#' [text][pkg::obj]   text        F   \\link[pkg:file]{text}
#' [s4-class]         s4          F   \\linkS4class{s4} or
#'                                    \\link[pkg:file]{s4}
#' [pkg::s4-class]    pkg::s4     F   \\link[pkg:file]{pkg::s4}
#' ```
#'
#' For the links with two RD variants the first version is used for
#' within-package links, and the second version is used for cross-package
#' links.
#'
#' The reference links will always look like `R:ref` for `[ref]` and
#' `[text][ref]`. These are explicitly tested in `test-rd-markdown-links.R`.
#'
#' We add in a special `R:` marker to the URL. This way we don't
#' pick up other links, that were specified via `<url>` or
#' `[text](link)`. In the parsed XML tree these look the same as
#' our `[link]` and `[text][link]` links.
#'
#' In the link references, we need to URL encode the reference,
#' otherwise commonmark does not use it (see issue #518).
#'
#' @param text Input markdown text.
#' @return The input text and all dummy reference link definitions
#'   appended.
#'
#' @noRd

get_md_linkrefs <- function(text) {
  refs <- str_match_all(
    text,
    regex(
      comments = TRUE,
      "
        (?<=[^\\]\\\\]|^)       # must not be preceded by ] or \
        \\[([^\\]\\[]+)\\]      # match anything inside of []
        (?:\\[([^\\]\\[]+)\\])? # match optional second pair of []
        (?=[^\\[{]|$)            # must not be followed by [ or {
      "
    )
  )[[1]]

  if (length(refs) == 0) {
    return(character())
  }

  ## For the [fun] form the link text is the same as the destination.
  # Need to check both NA and "" for different versions of stringr
  refs[, 3] <- ifelse(is.na(refs[, 3]) | refs[, 3] == "", refs[, 2], refs[, 3])

  refs3encoded <- map_chr(refs[, 3], URLencode)
  paste0("[", refs[, 3], "]: ", "R:", refs3encoded)
}

add_linkrefs_to_md <- function(text) {
  ref_lines <- get_md_linkrefs(text)
  if (length(ref_lines) == 0) {
    return(text)
  }
  ref_text <- paste0(ref_lines, collapse = "\n")
  paste0(text, "\n\n", ref_text, "\n")
}

#' Parse a MarkDown link, to see if we should create an Rd link
#'
#' See the table above.
#'
#' @param destination string constant, the "url" of the link
#' @param contents An XML node, containing the contents of the link.
#'
#' @noRd

parse_link <- function(destination, contents, state) {
  ## Not a [] or [][] type link, remove prefix if it is
  if (!grepl("^R:", destination)) {
    return(NULL)
  }
  destination <- sub("^R:", "", URLdecode(destination))

  ## if contents is a `code tag`, then we need to move this outside
  is_code <- FALSE
  if (length(contents) == 1 && xml_name(contents) == "code") {
    is_code <- TRUE

    contents <- xml_contents(contents)
    destination <- sub("`$", "", sub("^`", "", destination))

    local_bindings(.env = state, in_link_code = TRUE)
  }

  if (!all(xml_name(contents) %in% c("text", "softbreak", "linebreak"))) {
    incorrect <- setdiff(
      unique(xml_name(contents)),
      c("text", "softbreak", "linebreak")
    )

    warn_roxy_tag(
      state$tag,
      c(
        "markdown links must contain plain text",
        i = "Problematic link: {destination}"
      )
    )
    return("")
  }

  ## If the supplied link text is the same as the reference text,
  ## then we assume that the link text was automatically generated and
  ## it was not specified explicitly. In this case `()` links are
  ## turned to `\\code{}`.
  ## We also assume link text if we see a non-text XML tag in contents.
  has_link_text <- paste(xml_text(contents), collapse = "") != destination ||
    any(xml_name(contents) != "text")

  ## if (is_code) then we'll need \\code
  ## `pkg` is package or NA
  ## `fun` is fun() or obj (fun is with parens)
  ## `is_fun` is TRUE for fun(), FALSE for obj
  ## `obj` is fun or obj (fun is without parens)
  ## `s4` is TRUE if we link to an S4 class (i.e. have -class suffix)
  ## `noclass` is fun with -class removed
  ## `file` is the file name of the linked topic.

  thispkg <- roxy_meta_get("current_package") %||% ""
  is_code <- is_code || (grepl("[(][)]$", destination) && !has_link_text)
  pkg <- str_match(destination, "^(.*)::")[1, 2]
  pkg <- gsub("%", "\\\\%", pkg)
  fun <- utils::tail(strsplit(destination, "::", fixed = TRUE)[[1]], 1)
  fun <- gsub("%", "\\\\%", fun)
  is_fun <- grepl("[(][)]$", fun)
  obj <- sub("[(][)]$", "", fun)
  s4 <- str_detect(destination, "-class$")
  noclass <- str_match(fun, "^(.*)-class$")[1, 2]

  if (is.na(pkg)) {
    pkg <- resolve_link_package(obj, thispkg, state = state)
  }
  if (!is.na(pkg) && pkg == thispkg) {
    pkg <- NA_character_
  }
  file <- find_topic_filename(pkg, obj, state$tag)

  ## To understand this, look at the RD column of the table above
  if (!has_link_text) {
    paste0(
      if (is_code) "\\code{",
      if (s4 && is.na(pkg)) "\\linkS4class" else "\\link",
      if (is_fun || !is.na(pkg)) "[",
      if (is_fun && is.na(pkg)) "=",
      if (!is.na(pkg)) paste0(pkg, ":"),
      if (is_fun || !is.na(pkg)) paste0(if (is.na(pkg)) obj else file, "]"),
      "{",
      if (!is.na(pkg)) paste0(pkg, "::"),
      if (s4) noclass else fun,
      "}",
      if (is_code) "}" else ""
    )
  } else {
    contents <- mdxml_link_text(contents, state)

    list(
      paste0(
        if (is_code) "\\code{",
        "\\link[",
        if (is.na(pkg)) "=" else paste0(pkg, ":"),
        if (is.na(pkg)) obj else file,
        "]{"
      ),
      contents,
      "}",
      if (is_code) "}" else ""
    )
  }
}

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

#' Dummy page to test roxygen's markdown formatting
#'
#' Links are very tricky, so I'll put in some links here:
#' Link to a function: [roxygenize()].
#' Link to an object: [roxygenize] (we just treat it like an object here.
#'
#' Link to another package, function: [desc::desc()].
#' Link to another package, non-function: [desc::desc].
#'
#' Link with link text: [this great function][roxygenize()],
#' [`roxygenize`][roxygenize()], or [that great function][roxygenize].
#'
#' In another package: [and this one][desc::desc].
#'
#' This is a table:
#'
#' | __foo__ | __bar__ |
#' | :-- | --: |
#' | 1   | 2   |
#' | 100 | 200 |
#'
#' @name markdown-test
#' @keywords internal
NULL
