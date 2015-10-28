## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(comment = "#>", collapse = TRUE)

## ------------------------------------------------------------------------
#' \enumerate{
#'   \item First item
#'   \item Second item
#' }

## ------------------------------------------------------------------------
#' \itemize{
#'   \item First item
#'   \item Second item
#' }

## ------------------------------------------------------------------------
#' \describe{
#'   \item{One}{First item}
#'   \item{Two}{Second item}
#' }

## ------------------------------------------------------------------------
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))

  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))

  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
    c(cols, list(sep = " \\tab ", collapse = "\\cr\n  ")))

  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n  ",
    contents, "\n}\n", sep = "")
}

cat(tabular(mtcars[1:5, 1:5]))

