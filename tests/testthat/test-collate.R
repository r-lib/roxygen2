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
  path <- local_package_copy(test_path('testCollateNoIncludes'))

  update_collate(path)
  expect_equal(desc::desc_get_field("Collate", file = path), "b.R a.R")
})

test_that("DESCRIPTION file is re-written only if collate changes", {
  path <- local_package_copy(test_path("testCollateOverwrite"))

  expect_snapshot(
    {
      update_collate(path)
      "Second run should be idempotent"
      update_collate(path)
    },
    transform = function(x) gsub(path, "<path>", x, fixed = TRUE)
  )
})

test_that("drops bad collect directives", {
  path <- local_package_copy(test_path("empty"))
  write_lines(c("#' @include foo", "NULL"), file.path(path, "R", "a.R"))
  write_lines(c("#' @include a.R", "NULL"), file.path(path, "R", "b.R"))
  unlink(file.path(path, "R/empty-package.R"))

  withr::with_dir(path, expect_snapshot(update_collate(".")))
  expect_equal(desc::desc_get_collate(file = path), c("a.R", "b.R"))
})

test_that("can read from file name with utf-8 path", {
  path <- withr::local_tempfile(
    pattern = "Universit\u00e0-",
    lines = c("#' @include foo.R", NULL)
  )
  expect_equal(find_includes(path), "foo.R")
})
