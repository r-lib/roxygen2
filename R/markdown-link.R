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
#' [fun()]            fun()       T   \\link[=fun]{fun()}
#' [obj]              obj         F   \\link{obj}
#' [pkg::fun()]       pkg::fun()  T   \\link[pkg:file]{pkg::fun()}
#' [pkg::obj]         pkg::obj    F   \\link[pkg:file]{pkg::obj}
#' [text][fun()]      text        F   \\link[=fun]{text}
#' [text][obj]        text        F   \\link[=obj]{text}
#' [text][pkg::fun()] text        F   \\link[pkg:file]{text}
#' [text][pkg::obj]   text        F   \\link[pkg:file]{text}
#' [s4-class]         s4          F   \\linkS4class{s4}
#' [pkg::s4-class]    pkg::s4     F   \\link[pkg:file]{pkg::s4}
#' ```
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
#' @importFrom utils URLencode URLdecode

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
  refs[, 3] <- ifelse(is.na(refs[,3]) | refs[,3] == "", refs[, 2], refs[,3])

  refs3encoded <- map_chr(refs[,3], URLencode)
  paste0("[", refs[, 3], "]: ", "R:", refs3encoded)
}

add_linkrefs_to_md <- function(text) {
  ref_lines <- get_md_linkrefs(text)
  if (length(ref_lines) == 0)
    return(text)
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
#' @importFrom xml2 xml_name

parse_link <- function(destination, contents, state) {

  ## Not a [] or [][] type link, remove prefix if it is
  if (! grepl("^R:", destination)) return(NULL)
  destination <- sub("^R:", "", URLdecode(destination))

  ## if contents is a `code tag`, then we need to move this outside
  is_code <- FALSE
  if (length(contents) == 1 && xml_name(contents) == "code") {
    is_code <- TRUE
    contents <- xml_contents(contents)
    destination <- sub("`$", "", sub("^`", "", destination))
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
  is_code <- is_code || (grepl("[(][)]$", destination) && ! has_link_text)
  pkg <- str_match(destination, "^(.*)::")[1,2]
  pkg <- gsub("%", "\\\\%", pkg)
  if (!is.na(pkg) && pkg == thispkg) pkg <- NA_character_
  fun <- utils::tail(strsplit(destination, "::", fixed = TRUE)[[1]], 1)
  fun <- gsub("%", "\\\\%", fun)
  is_fun <- grepl("[(][)]$", fun)
  obj <- sub("[(][)]$", "", fun)
  s4 <- str_detect(destination, "-class$")
  noclass <- str_match(fun, "^(.*)-class$")[1,2]
  file <- find_topic_filename(pkg, obj, state$tag)

  ## To understand this, look at the RD column of the table above
  if (!has_link_text) {
    paste0(
      if (is_code) "\\code{",
      if (s4 && is.na(pkg)) "\\linkS4class" else "\\link",
      if (is_fun || ! is.na(pkg)) "[",
      if (is_fun && is.na(pkg)) "=",
      if (! is.na(pkg)) paste0(pkg, ":"),
      if (is_fun || ! is.na(pkg)) paste0(if (is.na(pkg)) obj else file, "]"),
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

#' Dummy page to test roxygen's markdown formatting
#'
#' Links are very tricky, so I'll put in some links here:
#' Link to a function: [roxygenize()].
#' Link to an object: [roxygenize] (we just treat it like an object here.
#'
#' Link to another package, function: [devtools::document()].
#' Link to another package, non-function: [devtools::document].
#'
#' Link with link text: [this great function][roxygenize()],
#' [`roxygenize`][roxygenize()], or [that great function][roxygenize].
#'
#' In another package: [and this one][devtools::document].
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
