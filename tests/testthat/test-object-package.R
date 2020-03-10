test_that("person turned into meaningful text", {
  person_desc <- function(given = "H", family = "W", email = "h@w.com", role = "aut", comment = NULL) {
    out <- person(given = given, family = family, email = email, role = role, comment = comment)
    author_desc(unclass(out)[[1]])
  }

  verify_output(test_path("test-object-package-author.txt"), {
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
