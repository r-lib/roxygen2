# Output ------------------------------------------------------------------

# A simple object to represent rd escaped text.
rd <- function(x) {
  structure(x, class = "rd")
}

#' @export
c.rd <- function(...) {
  rd(NextMethod())
}

#' @export
print.rd <- function(x, ...) {
  out <- paste0("<rd> ", x, collapse = "\n")
  cat(out)
}

escape <- function(x) UseMethod("escape")
escape.rd <- function(x) x
escape.character <- function(x) {
  # wrap_string uses \u{A0}, the unicode non-breaking space, which
  # is not necessarily valid in windows locales. useBytes is a quick
  # hack to fix the problem.
  x1 <- gsub("\\", "\\\\", x, fixed = TRUE, useBytes = TRUE)
  x2 <- gsub("%", "\\%", x1, fixed = TRUE, useBytes = TRUE)

  rd(x2)
}

# Works like escape, but unescapes special rd example commands
escape_examples <- function(x) {
  gsub("\\\\dont", "\\dont", escape(x))
}

# Works like paste, but automatically escapes all input variables,
# but not literal strings
build_rd <- function(..., collapse = NULL, sep = "") {
  args <- dots(...)
  env <- parent.frame()

  escaped <- lapply(args, function(arg) {
    if (is.character(arg)) return(arg)

    escape(eval(arg, env))
  })

  string <- do.call("paste", c(escaped, list(collapse = collapse, sep = sep)))
  rd(string)
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

# Translate a field and values into an Rd macro.
# Multiple values get their own braces.
rd_macro <- function(field, ..., space = FALSE) {
  if (space) {
    values <- paste0("\n", paste0(..., collapse = "\n"), "\n")
  } else {
    values <- str_trim(c(...))
  }

  paste0("\\", field, paste0("{", values, "}", collapse = ""), "\n")
}


# Input -------------------------------------------------------------------

get_rd <- function(topic, package = NULL) {
  help_call <- substitute(help(t, p), list(t = topic, p = package))
  top <- eval(help_call)
  if (length(top) == 0) return(NULL)

  internal_f("utils", ".getHelpFile")(top)
}

get_tags <- function(rd, tag) {
  Filter(function(x) identical(attr(x, "Rd_tag"), tag), rd)
}

rd2text <- function(x) {
  chr <- as_character_Rd(structure(x, class = "Rd"))
  str <- paste(chr, collapse = "")

  # re-escape comments
  gsub("%", "\\%", str, fixed = TRUE)
}

as_character_Rd <- function(x) {
  ZEROARG <- c("\\cr", "\\dots", "\\ldots", "\\R", "\\tab")
  TWOARG <- c("\\section", "\\item", "\\enc", "\\method", "\\S3method",
              "\\S4method", "\\tabular")
  USERMACROS <- c("USERMACRO", "\\newcommand", "\\renewcommand")
  EQN <- c("\\deqn", "\\eqn", "\\figure")
  modes <- c(RLIKE = 1L, LATEXLIKE = 2L, VERBATIM = 3L, INOPTION = 4L,
             COMMENTMODE = 5L, UNKNOWNMODE = 6L)
  tags <- c(RCODE = 1L, TEXT = 2L, VERB = 3L, COMMENT = 5L,
            UNKNOWN = 6L)
  state <- c(braceDepth = 0L, inRString = 0L)
  needBraces <- FALSE
  inEqn <- 0L
  pr <- function(x, quoteBraces) {
    tag <- attr(x, "Rd_tag")
    if (is.null(tag) || tag == "LIST")
      tag <- ""
    if (is.list(x)) {
      savestate <- state
      state <<- c(0L, 0L)
      needBraces <<- FALSE
      if (tag == "Rd") {
        result <- character()
        for (i in seq_along(x)) result <- c(result, pr(x[[i]],
                                                       quoteBraces))
      }
      else if (length(grep("^#", tag))) {
        if (deparse) {
          dep <- deparseRdElement(x[[1L]][[1L]], c(state,
                                                   modes["LATEXLIKE"], inEqn, as.integer(quoteBraces)))
          result <- c(tag, dep[[1L]])
        }
        else result <- c(tag, x[[1L]][[1L]])
        for (i in seq_along(x[[2L]])) result <- c(result,
                                                  pr(x[[2L]][[i]], quoteBraces))
        result <- c(result, "#endif\n")
      }
      else if (tag %in% ZEROARG) {
        result <- tag
        needBraces <<- TRUE
      }
      else if (tag %in% TWOARG) {
        result <- tag
        for (i in seq_along(x)) result <- c(result, pr(x[[i]],
                                                       quoteBraces))
      }
      else if (tag %in% EQN) {
        result <- tag
        inEqn <<- 1L
        result <- c(result, pr(x[[1L]], quoteBraces))
        inEqn <<- 0L
        if (length(x) > 1L)
          result <- c(result, pr(x[[2L]], quoteBraces))
      }
      else {
        result <- tag
        if (!is.null(option <- attr(x, "Rd_option")))
          result <- c(result, "[", pr(option, quoteBraces),
                      "]")
        result <- c(result, "{")
        for (i in seq_along(x)) result <- c(result, pr(x[[i]],
                                                       quoteBraces))
        result <- c(result, "}")
      }
      if (state[1L])
        result <- pr(x, TRUE)
      state <<- savestate
    }
    else if (tag %in% USERMACROS) {
      result <- c()
    }
    else {
      if (inherits(x, "Rd"))
        class(x) <- setdiff(class(x), "Rd")
      result <- as.character(x)
      if (needBraces) {
        if (grepl("^[[:alpha:]]", result))
          result <- c("{}", result)
        needBraces <<- FALSE
      }
    }
    result
  }
  if (is.null(attr(x, "Rd_tag")))
    attr(x, "Rd_tag") <- "Rd"
  pr(x, quoteBraces = FALSE)
}
