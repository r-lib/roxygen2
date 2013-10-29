with_locale <- function(locale, code) {
  cur <- Sys.getlocale(category = "LC_COLLATE")
  Sys.setlocale(category = "LC_COLLATE", locale = locale)
  on.exit(Sys.setlocale(category = "LC_COLLATE", locale = cur))
  force(code)
}
