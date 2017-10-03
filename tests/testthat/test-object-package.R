context("Object: Package")

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
