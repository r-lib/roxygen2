context("Collation")

test_that("collation as expected", {

  roclet <- make.collate.roclet()
  collation <- capture.output(roclet$parse('collate/belt.R',
                                           'collate/jacket.R',
                                           'collate/pants.R',
                                           'collate/shirt.R',
                                           'collate/shoes.R',
                                           'collate/socks.R',
                                           'collate/tie.R',
                                           'collate/undershorts.R',
                                           'collate/watch.R'))
                                           
  collation <- gsub("\\s +", " ", paste(collation, collapse = ""))
  expect_equal(collation,
    c("Collate: 'collate/shirt.R' 'collate/undershorts.R' 'collate/pants.R' 'collate/belt.R' 'collate/tie.R' 'collate/jacket.R' 'collate/socks.R' 'collate/shoes.R' 'collate/watch.R'"))
})