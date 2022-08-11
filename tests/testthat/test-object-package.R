test_that("person turned into meaningful text", {
  person_desc <- function(given = "H", family = "W", email = "h@w.com", role = "aut", comment = NULL) {
    out <- person(given = given, family = family, email = email, role = role, comment = comment)
    author_desc(unclass(out)[[1]])
  }

  expect_snapshot({
    "Multiple given/family names"
    person_desc(c("First", "Second"), c("Family1", "Family2"))

    "Multiple roles"
    person_desc(role = "ctb")

    "ORCID comments"
    person_desc(comment = c("ORCID" = "1234"))
    person_desc(comment = c("ORCID" = "https://orcid.org/1234"))
    person_desc(comment = c("ORCID" = "1234", "extra"))
  })
})

test_that("useful message if Authors@R is corrupted", {
  expect_snapshot({
    package_authors("1 + ")
    package_authors("stop('Uhoh')")
  })
})

test_that("can convert quote percentage signs in urls", {
  expect_equal(
    package_seealso_urls("https://www.foo.bar/search?q=see%20also"),
    "\\url{https://www.foo.bar/search?q=see\\%20also}"
  )

  expect_equal(
    package_seealso_urls(BugReports = "https://www.foo.bar/search?q=bug%20report"),
    "Report bugs at \\url{https://www.foo.bar/search?q=bug\\%20report}"
  )
})

test_that("can convert DOIs in url", {
  expect_equal(
    package_seealso_urls("https://doi.org/10.5281/zenodo.1485309"),
    "\\doi{10.5281/zenodo.1485309}"
  )
})

test_that("can autolink urls on package Description", {
  expect_equal(
    package_url_parse("x <https://x.com> y"),
    "x \\url{https://x.com} y"
  )
  expect_equal(
    package_url_parse("x <https://x.com/%3C-%3E> y"),
    "x \\url{https://x.com/\\%3C-\\%3E} y"
  )
})

test_that("can autolink DOIs", {
  expect_equal(package_url_parse("x <doi:abcdef> y"), "x \\doi{abcdef} y")
  expect_equal(package_url_parse("x <DOI:abcdef> y"), "x \\doi{abcdef} y")
  expect_equal(package_url_parse("x <DOI:%3C-%3E> y"), "x \\doi{\\%3C-\\%3E} y")
})

test_that("can autolink arxiv", {
  expect_equal(
    package_url_parse("x <arXiv:abc> y"),
    "x \\href{https://arxiv.org/abs/abc}{arXiv:abc} y"
  )
  expect_equal(
    package_url_parse("x <arxiv:abc> y"),
    "x \\href{https://arxiv.org/abs/abc}{arXiv:abc} y"
  )
  expect_equal(
    package_url_parse("x <arxiv:abc [def]> y"),
    "x \\href{https://arxiv.org/abs/abc}{arXiv:abc [def]} y"
  )
})

test_that("autolink several matching patterns", {
  text <- "url <http://a.com> doi <doi:xx> arxiv <arXiv:xx>"
  expect_equal(
    package_url_parse(text),
    paste(
      "url \\url{http://a.com}",
      "doi \\doi{xx}",
      "arxiv \\href{https://arxiv.org/abs/xx}{arXiv:xx}"
    )
  )
})
