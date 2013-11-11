set_collate <- function(locale) {
  cur <- Sys.getlocale(category = "LC_COLLATE")
  Sys.setlocale(category = "LC_COLLATE", locale = locale)
  cur
}

with_collate <- function(locale, code) {
  old <- set_collate(locale)
  on.exit(set_collate(old))

  force(code)
}

sort_c <- function(x) with_collate("C", sort(x))
