package_seealso <- function(desc) {
  if (!is.null(desc$URL)) {
    links <- paste0("\\url{", strsplit(desc$URL, ",\\s+")[[1]], "}")
  } else {
    links <- character()
  }
  if (!is.null(desc$BugReports)) {
    links <- c(links, paste0("Report bugs at \\url{", desc$BugReports, "}"))
  }

  itemize("Useful links:", links)
}

package_authors <- function(desc) {
  authors <- tryCatch(eval(parse(text = desc$`Authors@R`)),
    error = function(e) {
      warning(e)
      NULL
    }
  )
  if (is.null(authors))
    return()

  desc <- vapply(unclass(authors), author_desc, character(1))
  type <- vapply(unclass(authors), author_type, character(1))
  by_type <- split(desc, type)

  paste(
    c(
      paste0("\\strong{Maintainer}: ", by_type$cre[[1]], "\n"),
      itemize("Authors:", by_type$aut),
      itemize("Other contributors:", by_type$other)
    ),
    collapse = "\n"
  )
}

author_desc <- function(x) {
  desc <- x$given

  if (!is.null(x$family)) {
    desc <- paste0(desc, " ", x$family)
  }

  if (!is.null(x$email)) {
    desc <- paste0(desc, " \\email{", x$email, "}")
  }

  if (!is.null(x$comment)) {
    desc <- paste0(desc, " (", x$comment, ")")
  }

  extra_roles <- setdiff(x$role, c("cre", "aut"))
  if (length(extra_roles) > 0) {
    desc <- paste0(
      desc, " [", paste0(role_lookup[extra_roles], collapse = ", "), "]"
    )
  }

  desc
}

author_type <- function(x) {
  if ("cre" %in% x$role) {
    "cre"
  } else if ("aut" %in% x$role) {
    "aut"
  } else {
    "other"
  }
}

role_lookup <- c(
  "aut" = "author",
  "com" = "compiler",
  "ctb" = "contributor",
  "cph" = "copyright holder",
  "cre" = "maintainer",
  "ctr" = "contractor",
  "dtc" = "data contributor",
  "fnd" = "funder",
  "ths" = "thesis advisor",
  "trl" = "translator"
)

itemize <- function(header, x) {
  if (length(x) == 0)
    return()

  paste0(
    header, "\n",
    "\\itemize{\n",
    paste0("  \\item ", x, "\n", collapse = ""),
    "}\n"
  )
}
