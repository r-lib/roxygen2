test_that("author with more than one given/family name ", {
  expect_equal(
    author_desc(
      person(
        given = c("First", "Second"),
        family = c("Family1", "Family2"),
        email = "first.second.family@email.tld"
      )
    ),
    paste(
      "First Second",
      "Family1 Family2",
      "\\email{first.second.family@email.tld}"
    )
  )
})

test_that("person turned into meaningful text", {
  verify_output(test_path("test-object-package-author.txt"), {
    hw1 <- person("H", "W", "A", "h@w.com", "aut", c("ORCID" = "1234"))
    author_desc(hw1)

    hw2 <- person("H", "W", "A", "h@w.com", "aut", c("ORCID" = "https://orcid.org/1234"))
    author_desc(hw2)
  })
})
