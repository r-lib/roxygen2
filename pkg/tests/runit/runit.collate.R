## TODO: merge test
test.collate <- function() {
  roclet <- make.collate.roclet()
  collation <- capture.output(roclet$parse('runit/collate/belt.R',
                                           'runit/collate/jacket.R',
                                           'runit/collate/pants.R',
                                           'runit/collate/shirt.R',
                                           'runit/collate/shoes.R',
                                           'runit/collate/socks.R',
                                           'runit/collate/tie.R',
                                           'runit/collate/undershorts.R',
                                           'runit/collate/watch.R'))
  checkEquals(collation,
              paste("Collate: 'runit/collate/shirt.R'",
                    "'runit/collate/undershorts.R' 'runit/collate/pants.R'",
                    "'runit/collate/belt.R' 'runit/collate/tie.R'",
                    "'runit/collate/jacket.R' 'runit/collate/socks.R'",
                    "'runit/collate/shoes.R' 'runit/collate/watch.R'"),
              msg=collation)
}
