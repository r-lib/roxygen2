context("Collation with no @includes")

temp_copy_pkg <- function(pkg) {
  file.copy(normalizePath(pkg), tempdir(), recursive = TRUE)
  normalizePath(file.path(tempdir(), pkg))
}

test_that("Collate field unchanged when no @includes", {

  # Make a temporary copy of the package template for this test,
  # since if this test fails update_collate might have permanent side effects,
  # namely changing the collate field in the DESCRIPTION file
  # which would invalidate any future test runs
  test_pkg <- temp_copy_pkg('testCollateNoIncludes')
  on.exit(unlink(test_pkg, recursive = TRUE))

  old_desc <- read.description(file.path(test_pkg, "DESCRIPTION"))
  update_collate(test_pkg)
  new_desc <- read.description(file.path(test_pkg, "DESCRIPTION"))

  expect_named(old_desc,c("Package","Title","License","Description",
                          "Author","Maintainer","Version","Collate"))
  expect_named(new_desc,c("Package","Title","License","Description",
                          "Author","Maintainer","Version","Collate"))
  expect_identical(old_desc$Collate,new_desc$Collate)

})
