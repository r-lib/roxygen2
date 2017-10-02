context("Object: Package")

test_that("author with one given name ", {
  expect_equal(author_desc(list(given="Given",
                                family="Family",
                                role=c("aut", "cre"),
                                email="given.family@email.tld",
                                comment=NULL)),
               "Given Family \\email{given.family@email.tld}"
  )
})

test_that("author with more than one given name ", {
  expect_equal(author_desc(list(given=c("First", "Second"),
                                family="Family",
                                role=c("aut", "cre"),
                                email="first.second.family@email.tld",
                                comment=NULL)),
               "First Second Family \\email{first.second.family@email.tld}"
  )
})

test_that("Authors@R with more than one given name ", {
  expect_equal(package_authors(list("Authors@R"=
    paste0("c(person(\"Given\", \"Family\", role=c(\"aut\", \"cre\"), ",
           "email=\"given.family@email.tld\"), ",
           "person(c(\"First\", \"Second\"), \"Family\", role=\"aut\", ",
           "email=\"first.second.family@email.tld\"))"))),
    paste0("\\strong{Maintainer}: Given Family ",
           "\\email{given.family@email.tld}\n\n",
           "Authors:\n\\itemize{\n  \\item First Second Family ",
           "\\email{first.second.family@email.tld}\n}\n")
  )
})
