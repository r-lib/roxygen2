#' Add link reference definitions for functions to a markdown text
#'
#' We find the `[text][ref]` and the `[ref]` forms. There must be no
#' spaces between the closing and opening bracket in the `[text][ref]`
#' form.
#'
#' These are the link references we add for local links:
#'
#' ```
#' MARKDOWN           LINK TEXT  CODE RD
#' --------           ---------  ---- --
#' [fun()]            fun()       T   \\link[=fun]{fun()}
#' [obj]              obj         F   \\link{obj}
#' [`obj`]            obj         T   \\link{obj}
#' [text][fun()]      text        F   \\link[=fun]{text}
#' [text][obj]        text        F   \\link[=obj]{text}
#' [s4-class]         s4          F   \\link[=s4-class]{s4}
#' ```
#'
#' And for cross-package links:
#'
#' ```
#' MARKDOWN           LINK TEXT  CODE RD
#' --------           ---------  ---- --
#' [fun()]            fun()       T   \\link[pkg:fun]{pkg::fun()}
#' [obj]              obj         F   \\link[pkg:obj]{pkg::obj}
#' [`obj`]            obj         T   \\link[pkg:obj]{pkg::obj}
#' [pkg::fun()]       pkg::fun()  T   \\link[pkg:fun]{pkg::fun()}
#' [pkg::obj]         pkg::obj    F   \\link[pkg:obj]{pkg::obj}
#' [text][fun()]      text        F   \\link[pkg:fun]{text}
#' [text][obj]        text        F   \\link[pkg:obj]{text}
#' [text][pkg::fun()] text        F   \\link[pkg:fun]{text}
#' [text][pkg::obj]   text        F   \\link[pkg:obj]{text}
#' [s4-class]         s4          F   \\link[pkg:s4-class]{s4}
#' [pkg::s4-class]    pkg::s4     F   \\link[pkg:s4-class]{pkg::s4}
#' ```
#'
#' The reference links will always look like `R:ref` for `[ref]` and
#' `[text][ref]`. We add in a special `R:` marker to the URL to avoid
#' picking up other links, that were specified via `<url>` or
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
NULL


# Append link references for R functions ---------------------------------------

add_linkrefs_to_md <- function(text) {
  ref_lines <- get_md_linkrefs(text)
  if (length(ref_lines) == 0) {
    return(text)
  }
  ref_text <- paste0(ref_lines, collapse = "\n")
  paste0(text, "\n\n", ref_text, "\n")
}

get_md_linkrefs <- function(text) {
  refs <- str_match_all(
    text,
    regex(
      comments = TRUE,
      "
        (?<=[^\\]\\\\]|^)       # must not be preceded by ] or \
        \\[([^\\]\\[]+)\\]      # match anything inside of []
        (?:\\[([^\\]\\[]+)\\])? # match optional second pair of []
        (?=[^\\[{]|$)           # must not be followed by [ or {
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

# Link parsing -----------------------------------------------------------------

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

  ## If the supplied link text is the same as the reference text,
  ## then we assume that the link text was automatically generated and
  ## it was not specified explicitly. In this case `()` links are
  ## turned to `\\code{}`.
  ## We also assume link text if we see a non-text XML tag in contents.
  has_link_text <- paste(xml_text(contents), collapse = "") != destination ||
    any(xml_name(contents) != "text")

  ## if (is_code) then we'll need \\code
  ## `pkg` is package or NA
  ## `fun` is fun() or topic (fun is with parens)
  ## `is_fun` is TRUE for fun(), FALSE for topic
  ## `topic` is fun or topic (fun is without parens)
  ## `s4` is TRUE if we link to an S4 class (i.e. have -class suffix)
  ## `noclass` is fun with -class removed

  thispkg <- roxy_meta_get("current_package", "")
  is_code <- is_code || (grepl("[(][)]$", destination) && !has_link_text)
  pkg <- str_match(destination, "^(.*)::")[1, 2]
  explicit_pkg <- !is.na(pkg)
  fun <- utils::tail(strsplit(destination, "::", fixed = TRUE)[[1]], 1)
  is_fun <- grepl("[(][)]$", fun)
  topic <- sub("[(][)]$", "", fun)
  s4 <- str_detect(destination, "-class$")
  noclass <- str_match(fun, "^(.*)-class$")[1, 2]

  # Lookup topic using unescaped names, then escape % for Rd output
  if (is.na(pkg)) {
    pkg <- find_package(topic, tag = state$tag)
  } else if (!is.na(pkg) && pkg == thispkg) {
    pkg <- NA_character_
  }
  check_topic(pkg, topic, state$tag)

  ## To understand this, look at the RD column of the table above
  if (!has_link_text) {
    text <- if (s4) noclass else fun
    if (explicit_pkg && !is.na(pkg)) {
      text <- paste0(pkg, "::", text)
    }
    text <- escape(text)
  } else {
    text <- mdxml_link_text(contents, state)
  }
  rd_link(pkg, escape(topic), text, code = is_code)
}

check_topic <- function(pkg, topic, tag = NULL) {
  if (is.na(pkg) || identical(roxy_meta_get("current_package"), pkg)) {
    return(invisible())
  }

  if (!is_installed(pkg)) {
    warn_roxy_tag(tag, "refers to un-installed package {pkg}")
    return(invisible())
  }

  help_path <- utils::help((topic), (pkg))[1]
  if (is.na(basename(help_path))) {
    warn_roxy_tag(tag, "refers to unavailable topic {pkg}::{topic}")
  }
  invisible()
}

fun_suffix <- function(name, env) {
  if (is_infix_fun(name)) {
    name
  } else if (!env_has(env, name)) {
    name
  } else {
    obj <- env_get(env, name)
    if (!is.function(obj)) {
      name
    } else {
      paste0(name, "()")
    }
  }
}

rd_link <- function(pkg, topic, text, code = FALSE) {
  if (is.na(pkg) && topic == text) {
    out <- paste0("\\link{", text, "}")
  } else {
    anchor <- if (is.na(pkg)) paste0("=", topic) else paste0(pkg, ":", topic)
    out <- paste0("\\link[", anchor, "]{", text, "}")
  }
  if (code) {
    out <- paste0("\\code{", out, "}")
  }
  out
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
