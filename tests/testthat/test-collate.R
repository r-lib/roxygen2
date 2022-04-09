test_that("collation as expected", {
  names <- generate_collate(test_path("collate"))

  before <- function(a, b) {
    all(which(names %in% a) < which(names %in% b))
  }

  expect_true(before("undershorts", "pants"))
  expect_true(before(c("tie", "belt"), "jacket"))
  expect_true(before(c("socks", "undershorts", "pants"), "shoes"))
})

test_that("update_collate() checks that directory exists", {
  expect_snapshot(update_collate("doesn't-exist"), error = TRUE)
})

test_that("Collate field unchanged when no @includes", {
  local_package_copy(test_path('testCollateNoIncludes'))

  old_desc <- read.description("DESCRIPTION")
  update_collate(".")
  new_desc <- read.description("DESCRIPTION")

  expect_equal(old_desc, new_desc)
})

test_that("DESCRIPTION file is re-written only if collate changes", {
  local_package_copy(test_path("testCollateOverwrite"))

  # load package: this should update the DESCRIPTION once
  expect_snapshot({
    update_collate(".")
    update_collate(".")
  })
})
