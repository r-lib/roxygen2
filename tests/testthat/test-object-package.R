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


test_that("can convert DOIs in url", {
  expect_equal(
    package_seealso_urls(list(URL = "https://doi.org/10.5281/zenodo.1485309")),
    "\\doi{10.5281/zenodo.1485309}"
  )
})

test_that("package description not affected if no links", {
  
  expect_equal(
    package_url_parse("Simple description with no links."),
    "Simple description with no links."
  )
})

test_that("Autolink several matching patterns", {

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

test_that("can autolink urls on package Description", {

  # http and https
  expect_equal(
    package_url_parse(
      "<http://service.iris.edu/> and <https://abj.org.br/>"
    ),
    "\\url{http://service.iris.edu/} and \\url{https://abj.org.br/}"
  )

  # Decode URL
  expect_equal(
    package_url_parse(
      "<https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=OJ%3AL%3A2015%3A012%3ATOC>"
    ),
    "\\url{https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=OJ:L:2015:012:TOC}"
  )

  # Decode URL with spaces
  expect_equal(
    package_url_parse(
      "<https://www.sibm.it/SITO%20MEDITS/principaleprogramme.htm>>"
    ),
    "\\url{https://www.sibm.it/SITO\\%20MEDITS/principaleprogramme.htm}>"
  )
})

test_that("can autolink dois on package Description", {

  # Regular DOI
  expect_equal(
    package_url_parse(
      "A DOI: <doi:10.1093/biomet/asu017>"
    ),
    "A DOI: \\doi{10.1093/biomet/asu017}"
  )

  # With upcase
  expect_equal(
    package_url_parse(
      "Another DOI: <DOI:10.1086/160554>"
    ),
    "Another DOI: \\doi{10.1086/160554}"
  )
  # An encoded DOI
  expect_equal(
    package_url_parse(
      "Encoded doi: <doi:10.1175/1520-0469(1981)038%3C1179:TSLROA%3E2.0.CO;2>"
    ),
    "Encoded doi: \\doi{10.1175/1520-0469(1981)038<1179:TSLROA>2.0.CO;2}"
  )
})


test_that("can autolink arxiv on package Description", {

  # Regular arXiv
  expect_equal(
    package_url_parse(
      "'abess' <arXiv:1702.04690>"
    ),
    "'abess' \\href{https://arxiv.org/abs/1702.04690}{arXiv:1702.04690}"
  )

  # Different casing: arxiv:
  expect_equal(
    package_url_parse("'DLL' <arxiv:1907.12732>"),
    "'DLL' \\href{https://arxiv.org/abs/1907.12732}{arXiv:1907.12732}"
  )

  # Another valid format
  expect_equal(
    package_url_parse("<arXiv:1602.03990v2 [stat.ME]>"),
    "\\href{https://arxiv.org/abs/1602.03990v2}{arXiv:1602.03990v2 [stat.ME]}"
  )

  # Old-style format
  expect_equal(
    package_url_parse("<arXiv:quant-ph/0208069>"),
    "\\href{https://arxiv.org/abs/quant-ph/0208069}{arXiv:quant-ph/0208069}"
  )
})

test_that("No autolinking if wrong pattern", {
  badurl <- "<www.a.org> and <http:r.com"
  expect_equal(package_url_parse(badurl), badurl)

  ftpurl <- "<ftp://cran.r-project.org/incoming/>"
  expect_equal(package_url_parse(ftpurl), ftpurl)

  baddoi <- "<doi.109/10sm> and <dOi:10.20290>"
  expect_equal(package_url_parse(baddoi), baddoi)

  badarxiv <- "<ARXIV:1907.12732>"
  expect_equal(package_url_parse(badarxiv), badarxiv)
})
