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
