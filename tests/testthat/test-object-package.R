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

test_that("can autolink urls on package Description", {
  urls <- paste(
    "<https://github.com/>",
    "Secured <https://github.com/>.",
    "No link <www.github.com/>."
  )

  parsed <- package_description_urls(urls)

  expect_equal(
    parsed,
    paste(
      "\\url{https://github.com/}",
      "Secured \\url{https://github.com/}.",
      "No link <www.github.com/>."
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
    "With upper <arXiv:somecode>."
  )

  parsed <- package_description_urls(arxiv)

  expect_equal(
    parsed,
    paste(
      "\\href{https://arxiv.org/abs/somecode}{arXiv:somecode}",
      "With upper \\href{https://arxiv.org/abs/somecode}{arXiv:somecode}."
    )
  )
  expect_snapshot(parsed)
})
