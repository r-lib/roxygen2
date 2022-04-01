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

test_that("package description not affected if no links", {
  text <- "A simple description with no links."

  parsed <- package_description_urls(text)

  expect_equal(
    parsed,
    text
  )
  expect_snapshot(parsed)
})

test_that("can autolink urls on package Description", {
  urls <- paste(
    "<https://github.com/>",
    "Secured <https://github.com/>.",
    "No link <www.github.com/>.",
    "url masked with spaces",
    "<https://database.ich.org/sites/default/files/Q1E%20Guideline.pdf>",
    "url fully masked",
    "<https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=OJ%3AL%3A2015%3A012%3ATOC>"
  )

  parsed <- package_description_urls(urls)

  expect_equal(
    parsed,
    paste(
      "\\url{https://github.com/}",
      "Secured \\url{https://github.com/}.",
      "No link <www.github.com/>.",
      "url masked with spaces",
      "\\url{https://database.ich.org/sites/default/files/Q1E\\%20Guideline.pdf}",
      "url fully masked",
      "\\url{https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=OJ:L:2015:012:TOC}"
    )
  )
  expect_snapshot(parsed)
})

test_that("can autolink dois on package Description", {
  doi <- paste(
    "<doi:10.000/ret.234>",
    "With ampersands <DOI:aaa&.bbb.&c12>.",
    "No link <doi.baddoi>.",
    "I can use encoded dois",
    "<doi:10.1175/1520-0469(1981)038%3C1179:TSLROA%3E2.0.CO;2>"
  )

  parsed <- package_description_urls(doi)

  expect_equal(
    parsed,
    paste(
      "\\doi{10.000/ret.234}",
      "With ampersands \\doi{aaa&.bbb.&c12}.",
      "No link <doi.baddoi>.",
      "I can use encoded dois",
      "\\doi{10.1175/1520-0469(1981)038<1179:TSLROA>2.0.CO;2}"
    )
  )
  expect_snapshot(parsed)
})


test_that("can autolink arxiv on package Description", {
  arxiv <- paste(
    "<arxiv:somecode>",
    "With upper <arXiv:somecode2>.",
    "Strange arxiv",
    "<arXiv:2004.08318 [econ.EM]>",
    "Old-style arxiv",
    "<arXiv:quant-ph/0208069>"
  )

  parsed <- package_description_urls(arxiv)

  expect_equal(
    parsed,
    paste(
      "\\href{https://arxiv.org/abs/somecode}{arXiv:somecode}",
      "With upper \\href{https://arxiv.org/abs/somecode2}{arXiv:somecode2}.",
      "Strange arxiv",
      "\\href{https://arxiv.org/abs/2004.08318}{arXiv:2004.08318 [econ.EM]}",
      "Old-style arxiv",
      "\\href{https://arxiv.org/abs/quant-ph/0208069}{arXiv:quant-ph/0208069}"
    )
  )
  expect_snapshot(parsed)
})
