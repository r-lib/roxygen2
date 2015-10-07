context("Collation")

test_that("collation as expected", {
  results <- generate_collate("collate")
  names <- str_replace(basename(results), "\\..*$", "")

  before <- function(a, b) {
    all(which(names %in% a) < which(names %in% b))
  }

  expect_true(before("undershorts", "pants"))
  expect_true(before(c("tie", "belt"), "jacket"))
  expect_true(before(c("socks", "undershorts", "pants"), "shoes"))

  expected <- c('shirt.R', 'undershorts.R','pants.R', 'belt.R', 'tie.R',
    'jacket.R', 'socks.R', 'shoes.R', 'watch.R')

  expect_equal(results, expected)
})

test_that("Collate field unchanged when no @includes", {
  test_pkg <- temp_copy_pkg('testCollateNoIncludes')
  on.exit(unlink(test_pkg, recursive = TRUE))

  old_desc <- read.description(file.path(test_pkg, "DESCRIPTION"))
  update_collate(test_pkg)
  new_desc <- read.description(file.path(test_pkg, "DESCRIPTION"))

  expect_equal(names(old_desc), names(new_desc))
  expect_identical(old_desc$Collate,new_desc$Collate)

})

test_that("DESCRIPTION file is re-written only if collate changes", {
  pkg_path <- "testCollateOverwrite"
  desc_path <- file.path(pkg_path, 'DESCRIPTION')
  
  # make backup copy of incomplete DESCRIPTION file (restored on exit)
  file.copy(desc_path, tmp <- tempfile())
  on.exit( file.copy(tmp, desc_path, overwrite = TRUE), add = TRUE)
  
  # load package: this should update the DESCRIPTION file (warning)
  expect_output(update_collate(pkg_path), "Updating collate directive", info = "update_collate on incomplete package: DESCRIPTION file is updated")
  
  # should not update anymore
  expect_true(!length(capture.output(update_collate(pkg_path))), info = "update_collate on complete package: DESCRIPTION file is NOT updated")
      
 })


